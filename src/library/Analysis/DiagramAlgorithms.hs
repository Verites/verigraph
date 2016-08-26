{-|
Module      : DiagramAlgorithms
Description : Implements diagram-based algorithms
Stability   : stable

This diagram shows objects and morphisms names used in the algorithms below:

 @
                    N1    N2
                    ^      ^
         r1     l1  │n1  n2│  l2     r2
     R1◀─────K1────▶L1    L2◀────K2─────▶R2
     │       │       \\   /       │       │
  m1'│      k│      m1\\ /m2      │       │
     ▼       ▼         ▼         ▼       ▼
     P1◀─────D1───────▶G◀───────D2──────▶P2
         r'       l'
 @

q21 : N2 -> P1
m2' : L2 -> P1

prod1 = l1 r1 {n1}

prod2 = l2 r2 {n2}
-}

module Analysis.DiagramAlgorithms (
    deleteUse
  , produceDangling
  , deleteUseDangling
  , produceForbidOneNac
  ) where

import           Abstract.Morphism
import           Data.Maybe                (mapMaybe)
import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW hiding (calculateComatch)

-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that l' . h21 = m2
deleteUse :: DPO m => DPOConfig -> Production m -> (m, m) -> Bool
deleteUse config l (m1,m2) = null matchD
    where
        (_,l') = RW.calculatePushoutComplement m1 (getLHS l) --get only the morphism D2 to G
        restriction = matchRestrictionToProp (matchRestriction config)
        l2TOd1 = findMorphisms restriction (domain m2) (domain l')
        matchD = filter (\x -> m2 == compose x l') l2TOd1

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that unable @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satisfiesNACs and not satisfiesGluingConditions
produceDangling :: DPO m => DPOConfig -> Production m -> Production m -> (m, m) -> Bool
produceDangling config l r (m1,m2) =
  not (null matchD) && not (satisfiesGluingConditions config r m2') && satisfiesNACs config r m2'
  where
    (k,l') = RW.calculatePushoutComplement m1 (getLHS l)
    morphismRestriction = matchRestrictionToProp (matchRestriction config)
    l2TOd1 = findMorphisms morphismRestriction (domain m2) (domain l')
    matchD = filter (\x -> m2 == compose x l') l2TOd1
    (_,r') = RW.calculatePushout k (getRHS l)
    m2' = if length matchD > 1
            then error "produceDangling: non unique h21 morphism"
            else compose (head matchD) r' --matchD is unique if exists

-- | Verifies delete-use, if false verifies produce-dangling.
-- Returns Left in the case of delete-use and Right for produce-dangling.
deleteUseDangling :: DPO m => DPOConfig -> Production m
                  -> Production m -> (m, m)-> Maybe (Either (m,m) (m,m))
deleteUseDangling config l r (m1,m2) =
  case (null matchD, dang) of
    (True,_)     -> Just (Left (m1,m2))  -- delete use case
    (False,True) -> Just (Right (m1,m2)) -- produce dangling case
    _            -> Nothing              -- free overlap case
  where
    (k,l') = RW.calculatePushoutComplement m1 (getLHS l)
    morphismRestriction = matchRestrictionToProp (matchRestriction config)
    lTOd = findMorphisms morphismRestriction (domain m2) (domain l')
    matchD = filter (\x -> m2 == compose x l') lTOd
    (_,r') = RW.calculatePushout k (getRHS l)
    m2' = compose (head matchD) r'
    dang = not (satisfiesGluingConditions config r m2') && satisfiesNACs config r m2'

-- | Rule @p1@ is in a produce-forbid conflict with @p2@ if @p1@
-- produces something that able some nac of @p2@.
--
-- Checks produce-forbid for a NAC @n@ in @p2@
produceForbidOneNac :: (EpiPairs m, DPO m) => DPOConfig
                    -> Production m -> Production m -> Production m
                    -> (m, Int) -> [((m,m), (m,m), (m,Int))]
produceForbidOneNac config l inverseLeft r (n,idx) =
  map (\(q21,h1,m2',m1,m2) -> ((m1,m2), (h1,m2'), (q21,idx))) prodForbids
    where
      -- common names
      r1 = getRHS l
      restriction = matchRestrictionToProp (matchRestriction config)
      satsGluingNacs = satisfiesRewritingConditions config

      -- Consider for a NAC n (L2 -> N2) of r any jointly surjective
      -- pair of morphisms (m1',q21) with q21 (part)inj
      allP1 = createPairsNac config (codomain r1) n

      -- Check gluing cond for (m1',r1)
      validP1 = filter (\(m1', _) -> satsGluingNacs inverseLeft m1') allP1

      -- Construct PO complement D1
      -- Construct PO K and abort if m1 not sats NACs l
      validG =
        mapMaybe
          (\(m1', q21) ->
            let (k, m1, r', l') = RW.calculateDPO m1' inverseLeft --allG
            in
              if satisfiesNACs config l m1
                then Just (m1', q21, k, r', m1, l')
                else Nothing)
        validP1

      -- Check existence of h21: L2 -> D1, st: e1 . h21 = q21 . n2
      checkH21 =
        mapMaybe
          (\(m1', q21, k, r', m1, l') ->
            let h21Candidates = findMorphisms restriction (domain n) (codomain k)
                composeNQ21 = compose n q21
                validH21 = filter (\h21Cand -> compose h21Cand r' == composeNQ21) h21Candidates
             in
               case validH21 of --if exists it is unique
                 [] -> Nothing
                 (h21:_) -> Just (m1', q21, k, r', m1, l', h21))
        validG

      -- Define m2 = d1 . h21: L2 -> K
      -- Check gluing condition for m2 and r
      prodForbids =
        mapMaybe
          (\(m1', q21, _, r', m1, l', h21) ->
            let m2  = compose h21 l'
                m2' = compose h21 r'
            in
              if satsGluingNacs r m2
                then Just (q21, m1', m2', m1, m2)
                else Nothing)
          checkH21
