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
  , deleteUseDangling
  , produceForbidOneNac
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Data.Maybe           (mapMaybe)

-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that d1 . h21 = m2
isDeleteUse :: DPO m => DPOConfig -> Production m -> (m, m) -> Bool
isDeleteUse conf p1 (m1,m2) = null h21
    where
        (_,d1) = calculatePushoutComplement m1 (getLHS p1) --gets only the morphism D1 to G
        h21 = findAllPossibleH21 conf m2 d1

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that disables @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satisfiesNACs and not satisfiesGluingConditions
isProduceDangling :: DPO m => DPOConfig -> Production m -> Production m -> (m, m) -> Bool
isProduceDangling conf p1 p2 (m1,m2) =
  not (null h21) && not (satisfiesGluingConditions conf p2 h21_e1) && satisfiesNACs conf p2 h21_e1
  where
    (k1,d1) = calculatePushoutComplement m1 (getLHS p1)
    (_,e1) = calculatePushout k1 (getRHS p1)
    h21 = findAllPossibleH21 conf m2 d1
    h21_e1 = compose (head h21) e1 --h21 is unique if it exists

-- | Given the morphisms @m2: L2 -> G@ and @d1 : D1 -> G@, finds all possible @h21 : L2 -> D1@
-- where m2 = h21 . d1
findAllPossibleH21 :: DPO m => DPOConfig -> m -> m -> [m]
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
deleteUseDangling :: DPO m => DPOConfig -> Production m -> Production m -> (m, m)-> Maybe (Either (m,m) (m,m))
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
produceForbidOneNac :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> (m, Int) -> [((m,m), (m,m), (m,Int))]
produceForbidOneNac conf p1 p2 (n,idx) =
  map (\(q21,h1,e1,m1,m2) -> ((m1,m2), (h1,e1), (q21,idx))) produceForbids
    where
      p1' = invertProduction conf p1
      r1 = getRHS p1
      validP1 = calculatesAllValidP1s conf p1' r1 n
      validG = calculateRewritingsFromP1ToG conf p1 p1' validP1
      checkH21 = calculateH21InRewriting conf n validG
      produceForbids = calcultateValidProduceForbids conf p2 checkH21

-- | Consider for a NAC n (L2 -> N2) of r any jointly surjective
-- pair of morphisms (m1',q21) with q21 (part)inj
-- Check gluing cond for (m1',r1)
calculatesAllValidP1s :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> m -> m -> [(m,m)]
calculatesAllValidP1s conf p1' r1 n = validP1
  where
    allP1 = createJointlyEpimorphicPairsFromNAC conf (codomain r1) n
    validP1 = filter (\(m1', _) -> satisfiesRewritingConditions conf p1' m1') allP1

-- | Construct PO complement D1
-- Construct PO K and abort if m1 not sats NACs l
calculateRewritingsFromP1ToG :: (DPO m) => DPOConfig -> Production m -> Production m -> [(m,m)] -> [(m,m,m,m,m,m)]
calculateRewritingsFromP1ToG conf p1 p1' = mapMaybe (\(m1', q21) ->
  let (k1, m1, e1, d1) = calculateDPO m1' p1' in
    if satisfiesNACs conf p1 m1 then Just (m1', q21, k1, e1, m1, d1)
    else Nothing)

-- | Check existence of h21: L2 -> D1, st: e1 . h21 = q21 . n2
calculateH21InRewriting :: DPO m => DPOConfig -> m -> [(m,m,m,m,m,m)] -> [(m,m,m,m,m,m,m)]
calculateH21InRewriting conf n = mapMaybe
  (\(m1', q21, k, e1, m1, d1) ->
    let restriction = matchRestrictionToMorphismType (matchRestriction conf)
        h21Candidates = findMorphisms restriction (domain n) (codomain k)
        composeNQ21 = compose n q21
        validH21 = filter (\h21Cand -> compose h21Cand e1 == composeNQ21) h21Candidates
    in case validH21 of --if exists it is unique
      []      -> Nothing
      (h21:_) -> Just (m1', q21, k, e1, m1, d1, h21))

-- | Define m2 = d1 . h21: L2 -> K
-- Check gluing condition for m2 and p2
calcultateValidProduceForbids :: DPO m => DPOConfig -> Production m -> [(m,m,m,m,m,m,m)] -> [(m,m,m,m,m)]
calcultateValidProduceForbids conf p2 = mapMaybe (\(m1', q21, _, e1, m1, d1, h21) ->
  let m2  = compose h21 d1
      m2' = compose h21 e1
  in if satisfiesRewritingConditions conf p2 m2
        then Just (q21, m1', m2', m1, m2)
        else Nothing)
