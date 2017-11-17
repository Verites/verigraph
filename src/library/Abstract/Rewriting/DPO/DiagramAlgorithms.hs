{-|
Module      : DiagramAlgorithms
Description : Implements diagram-based algorithms

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
h21 : L2 -> D1
h21_e1 : L2 -> P1

prod1 = l1 r1 {n1}

prod2 = l2 r2 {n2}
-}

module Abstract.Rewriting.DPO.DiagramAlgorithms (
    isDeleteUse
  , isProduceDangling
  , isProduceForbid
  , isProduceUse
  , isDeleteForbid
  , deleteUseDangling
  , produceForbidOneNac
  , findAllPossibleH21
  ) where

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Constraint
import           Abstract.Rewriting.DPO
import           Control.Applicative
import           Control.Monad

-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that d1 . h21 = m2
isDeleteUse :: DPO morph => MorphismsConfig morph -> Production morph -> (morph,morph) -> Bool
isDeleteUse conf p1 (m1,m2) = null h21
  where
    (_,d1) = calculatePushoutComplementAlongM (leftMorphism p1) m1 --gets only the morphism D1 to G
    h21 = findAllPossibleH21 conf m2 d1

isProduceUse :: DPO morph => MorphismsConfig morph -> Production morph -> (morph,morph) -> Bool
isProduceUse conf p1 (m1',m2) = null h21
  where
    (_,e1) = calculatePushoutComplementAlongM (rightMorphism p1) m1' --gets only the morphism D1 to G
    h21 = findAllPossibleH21 conf m2 e1

-- Runs the DPO rewriting on p1 with m1 and
-- returns h21 and h21_e1 morphism as diagram above
getH21fromRewriting :: DPO morph => MorphismsConfig morph -> Production morph -> morph -> morph -> ([morph], morph)
getH21fromRewriting conf p1 m1 m2 = (h21,h21_e1)
  where
    (k1,d1) = calculatePushoutComplementAlongM (leftMorphism p1) m1
    (e1,_) = calculatePushoutAlongM (rightMorphism p1) k1
    h21 = findAllPossibleH21 conf m2 d1
    h21_e1 = e1 <&> head h21 --h21 is unique if it exists

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that disables @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satisfiesNACs and not satisfiesGluingConditions
isProduceDangling :: DPO morph => MorphismsConfig morph -> Production morph -> Production morph -> (morph,morph) -> Bool
isProduceDangling conf p1 p2 (m1,m2) =
  let (h21,h21_e1) = getH21fromRewriting conf p1 m1 m2
  in not (null h21) && not (satisfiesGluingConditions conf p2 h21_e1) && satisfiesNACs conf p2 h21_e1

isProduceForbid :: (DPO morph) => MorphismsConfig morph -> Production morph -> Production morph -> (morph,morph) -> Bool
isProduceForbid conf p1 p2 (m1,m2) =
  let (h21,h21_e1) = getH21fromRewriting conf p1 m1 m2
  in not (null h21) && satisfiesGluingConditions conf p2 h21_e1 && not (satisfiesNACs conf p2 h21_e1)

isDeleteForbid :: DPO morph => MorphismsConfig morph -> Production morph -> Production morph -> (morph,morph) -> Bool
isDeleteForbid conf p1 p2 (m1',m2) =
  let
    validRewriting = hasPushoutComplementAlongM (rightMorphism p1) m1'
    (k1,e1) = calculatePushoutComplementAlongM (rightMorphism p1) m1'
    h21 = findAllPossibleH21 conf m2 e1
    (d1,_) = calculatePushoutAlongM (leftMorphism p1) k1
    h21_d1 = d1 <&> head h21
  in validRewriting && not (null h21) && satisfiesGluingConditions conf p2 h21_d1 && not (satisfiesNACs conf p2 h21_d1)

-- | Given the morphisms @m2: L2 -> G@ and @d1 : D1 -> G@, finds all possible @h21 : L2 -> D1@
-- where m2 = h21 . d1
findAllPossibleH21 :: DPO morph => MorphismsConfig morph -> morph -> morph -> [morph]
findAllPossibleH21 conf m2 d1 =
  if length h21 > 1
    then error "produceDangling: non unique h21 morphism"
    else h21
  where
    morphismRestriction = matchRestriction conf
    h21 = findCospanCommuters morphismRestriction m2 d1

-- | Verifies delete-use, if false verifies produce-dangling.
-- Returns Left in the case of delete-use and Right for produce-dangling.
deleteUseDangling :: DPO morph => MorphismsConfig morph -> Production morph -> Production morph -> (morph,morph)-> Maybe (Either (morph,morph) (morph,morph))
deleteUseDangling conf p1 p2 (m1,m2) =
  case (null h21, dangling) of
    (True,_)     -> Just (Left (m1,m2))  -- delete use case
    (False,True) -> Just (Right (m1,m2)) -- produce dangling case
    _            -> Nothing              -- free overlap case
  where
    (h21,h21_e1) = getH21fromRewriting conf p1 m1 m2
    dangling = not (satisfiesGluingConditions conf p2 h21_e1) && satisfiesNACs conf p2 h21_e1

-- | Rule @p1@ is in a produce-forbid conflict with @p2@ if @p1@
-- produces something that enables some nac of @p2@.
--
-- Enumerate all produce-forbids for a NAC @n@ in @p2@, as long as
-- the overlapping of matches doesn't violate any of the given constraints.
produceForbidOneNac :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph]
                    -> Production morph -> Production morph -> (morph, Int) -> [((morph,morph), (morph,morph), (morph,Int))]
produceForbidOneNac conf constraints p1 p2 (n2,idx) = do
  let p1' = invertProduction conf p1

  -- Pick a jointly epi pair /R1 -m1'-> P1 <-q21- N2/
  (m1', q21) <- createJointlyEpimorphicPairsFromNAC conf (rightObject p1) n2

  -- Reconstruct the match /m1/ that would lead to this pair
  guard (satisfiesRewritingConditions conf p1' m1')
  let (k1, m1, e1, d1) = calculateDPO m1' p1'
  guard (satisfiesNACs conf p1 m1)
  guard (codomain m1 `satisfiesAllConstraints` constraints)

  -- Look for morphisms /h21 : L2 -> D1/
  let h21Candidates = findMorphisms (matchRestriction conf) (domain n2) (codomain k1)
      composeNQ21 = q21 <&> n2

  case filter (\h21 -> e1 <&> h21 == composeNQ21) h21Candidates of
    [] ->
      -- No proper h21, so no produce-forbid conflict
      empty

    [h21] -> do
      let m2 = d1 <&> h21
          m2' = e1 <&> h21
      guard (satisfiesRewritingConditions conf p2 m2)

      -- There is h21 and the match m2 is valid, so there is a conflict
      return ((m1, m2), (m1', m2'), (q21, idx))

    _ -> error "produceForbidOneNac: h21 should be unique, but isn't"

