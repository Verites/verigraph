{-|
Module      : DiagramAlgorithms
Description : Implements diagram-based algorithms
Stability   : stable

This diagram shows objects and morphisms names used in the algorithms below:

 @
                     N1    N2
                     ^     ^
         r1     l1   │n1 n2│  l2     r2
     R1◀─────K1────▶L1    L2◀────K2─────▶R2
     │       │       \\   /       │       │
  m1'│     k1│      m1\\ /m2      │       │
     ▼       ▼          ▼         ▼       ▼
     P1◀─────D1───────▶G◀───────D2──────▶P2
         e1       d1
 @

q21 : N2 -> P1
h21e1 : L2 -> P1

prod1 = l1 r1 {n1}

prod2 = l2 r2 {n2}
-}

module Analysis.DiagramAlgorithms (
    isDeleteUse
  , isProduceDangling
  , isProduceForbid
  , isProduceUse
  , isDeleteForbid
  , deleteUseDangling
  , produceForbidOneNac
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Control.Applicative
import           Control.Monad

-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that d1 . h21 = m2
isDeleteUse :: DPO m => MorphismsConfig -> Production m -> (m, m) -> Bool
isDeleteUse conf p1 (m1,m2) = null h21
    where
        (_,d1) = calculatePushoutComplement m1 (getLHS p1) --gets only the morphism D1 to G
        h21 = findAllPossibleH21 conf m2 d1

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that disables @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satisfiesNACs and not satisfiesGluingConditions
isProduceDangling :: DPO m => MorphismsConfig -> Production m -> Production m -> (m, m) -> Bool
isProduceDangling conf p1 p2 (m1,m2) =
  not (null h21) && not (satisfiesGluingConditions conf p2 h21_e1) && satisfiesNACs conf p2 h21_e1
  where
    (k1,d1) = calculatePushoutComplement m1 (getLHS p1)
    (_,e1) = calculatePushout k1 (getRHS p1)
    h21 = findAllPossibleH21 conf m2 d1
    h21_e1 = compose (head h21) e1 --h21 is unique if it exists

isProduceForbid :: (DPO m) => MorphismsConfig -> Production m -> Production m -> (m,m) -> Bool
isProduceForbid conf p1 p2 (m1,m2) =
  let
    (k1,d1) = calculatePushoutComplement m1 (getLHS p1)
    (_,e1) = calculatePushout k1 (getRHS p1)
    h21 = findAllPossibleH21 conf m2 d1
    h21_e1 = compose (head h21) e1
  in not (null h21) && satisfiesGluingConditions conf p2 h21_e1 && not (satisfiesNACs conf p2 h21_e1)

isProduceUse :: DPO m => MorphismsConfig -> Production m -> (m, m) -> Bool
isProduceUse conf p1 (m1',m2) = null h21
    where
        (_,e1) = calculatePushoutComplement m1' (getRHS p1) --gets only the morphism D1 to G
        h21 = findAllPossibleH21 conf m2 e1

isDeleteForbid :: DPO m => MorphismsConfig -> Production m -> Production m -> (m,m) -> Bool
isDeleteForbid conf p1 p2 (m1',m2) =
  let
    (k1,e1) = calculatePushoutComplement m1' (getRHS p1)
    h21 = findAllPossibleH21 conf m2 e1
    (_,d1) = calculatePushout k1 (getLHS p1)
    h21_d1 = compose (head h21) d1
  in not (null h21) && satisfiesGluingConditions conf p2 h21_d1 && not (satisfiesNACs conf p2 h21_d1)

-- | Given the morphisms @m2: L2 -> G@ and @d1 : D1 -> G@, finds all possible @h21 : L2 -> D1@
-- where m2 = h21 . d1
findAllPossibleH21 :: DPO m => MorphismsConfig -> m -> m -> [m]
findAllPossibleH21 conf m2 d1 =
  if length h21 > 1
    then error "produceDangling: non unique h21 morphism"
    else h21
  where
    morphismRestriction = matchRestrictionToMorphismType (matchRestriction conf)
    l2TOd1 = findMorphisms morphismRestriction (domain m2) (domain d1)
    h21 = filter (\x -> m2 == compose x d1) l2TOd1

-- | Verifies delete-use, if false verifies produce-dangling.
-- Returns Left in the case of delete-use and Right for produce-dangling.
deleteUseDangling :: DPO m => MorphismsConfig -> Production m -> Production m -> (m, m)-> Maybe (Either (m,m) (m,m))
deleteUseDangling conf p1 p2 (m1,m2) =
  case (null h21, dangling) of
    (True,_)     -> Just (Left (m1,m2))  -- delete use case
    (False,True) -> Just (Right (m1,m2)) -- produce dangling case
    _            -> Nothing              -- free overlap case
  where
    (k1,d1) = calculatePushoutComplement m1 (getLHS p1)
    (_,e1) = calculatePushout k1 (getRHS p1)
    h21 = findAllPossibleH21 conf m2 d1
    h21_e1 = compose (head h21) e1
    dangling = not (satisfiesGluingConditions conf p2 h21_e1) && satisfiesNACs conf p2 h21_e1

-- | Rule @p1@ is in a produce-forbid conflict with @p2@ if @p1@
-- produces something that enables some nac of @p2@.
--
-- Checks produce-forbid for a NAC @n@ in @p2@
produceForbidOneNac :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> (m, Int) -> [((m,m), (m,m), (m,Int))]
produceForbidOneNac conf p1 p2 (n2,idx) = do
  let p1' = invertProduction conf p1

  -- Pick a jointly epi pair /R1 -m1'-> P1 <-q21- N2/
  (m1', q21) <- createJointlyEpimorphicPairsFromNAC conf (codomain $ getRHS p1) n2

  -- Reconstruct the match /m1/ that would lead to this pair
  guard (satisfiesRewritingConditions conf p1' m1')
  let (k1, m1, e1, d1) = calculateDPO m1' p1'
  guard (satisfiesNACs conf p1 m1)

  -- Look for morphisms /h21 : L2 -> D1/
  let h21Candidates = findMorphisms' conf (domain n2) (codomain k1)
      composeNQ21 = compose n2 q21

  case filter (\h21 -> compose h21 e1 == composeNQ21) h21Candidates of
    [] ->
      -- No proper h21, so no produce-forbid conflict
      empty

    [h21] -> do
      let m2 = compose h21 d1
          m2' = compose h21 e1
      guard (satisfiesRewritingConditions conf p2 m2)

      -- There is h21 and the match m2 is valid, so there is a conflict
      return ((m1, m2), (m1', m2'), (q21, idx))

    _ -> error "produceForbidOneNac: h21 should be unique, but isn't"

findMorphisms' :: FindMorphism m => MorphismsConfig -> Obj m -> Obj m -> [m]
findMorphisms' conf =
  findMorphisms (matchRestrictionToMorphismType $ matchRestriction conf)
