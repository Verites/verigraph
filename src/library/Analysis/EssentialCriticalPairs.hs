{-# LANGUAGE FlexibleContexts  #-}

module Analysis.EssentialCriticalPairs
 ( namedEssentialCriticalPairs,
   findAllEssentialDeleteUse,
   findAllEssentialProduceForbid,
   findAllEssentialProduceDangling
   ) where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW
import           Abstract.Morphism
import           Analysis.CriticalPairs
import           Analysis.EpimorphicPairs

---- Essential Critical Pairs generation.
-- Warning: this algorithms are in development.

type NamedRule m = (String, Production m)
type NamedCriticalPairs m = (String,String,[CriticalPair m])

-- | Returns the Essential Critical Pairs with rule names
namedEssentialCriticalPairs :: (EpiPairs m, DPO m) =>
  DPOConfig -> [NamedRule m] -> [NamedCriticalPairs m]
namedEssentialCriticalPairs conf namedRules =
  map (uncurry getCPs) [(a,b) | a <- namedRules, b <- namedRules]
    where
      getCPs (n1,r1) (n2,r2) =
        (n1, n2, findEssentialCriticalPairs conf r1 r2)

-- | Finds all Essential Critical Pairs between two given Productions
findEssentialCriticalPairs :: (EpiPairs m, DPO m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
findEssentialCriticalPairs conf p1 p2 =
  findAllEssentialDeleteUse conf p1 p2 ++
  findAllEssentialProduceDangling conf p1 p2 ++
  findAllEssentialProduceForbid conf p1 p2

-- | Get all essential delete-use and organize them in a list of 'CriticalPair'.
findAllEssentialDeleteUse :: (EpiPairs m, DPO m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllEssentialDeleteUse conf p1 p2 =
  map (\(_,_,m1,m2) -> CriticalPair (m1,m2) Nothing Nothing DeleteUse) essentialCPs
  where
    essentialCPs =
      filter
        (isEssentialDeleteUse conf)
        (findPotentialEssentialCPs conf p1 p2)

-- | Generates all "epi" pairs for essential delete-use,
-- returns part of the initial pushout to avoid recalculations. 
findPotentialEssentialCPs :: (DPO m, EpiPairs m) => DPOConfig -> Production m -> Production m -> [(m, m, m, m)]
findPotentialEssentialCPs conf p1 p2 = satisfyingPairs
  where
    (_,l1',c) = calculateInitialPushout (getLHS p1)
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) l1' (getLHS p2)
    shiftedPairs =
      map
        (\(e1,e2) ->
          let (m1,d1') = calculatePushout e1 c
              m2 = compose e2 d1'
           in (l1', c, m1, m2)
        )
      pairs
    satisfyingPairs = filter (\(_,_,m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) shiftedPairs

-- | A pair of monomorphic matches (with precalcultated initial pushout (l1',c) elements)
-- is an essential delete use when the
-- pullback of the compose of the c (from the initial pushout) with m1
-- and m2 is a pushout (guaranteed by the construction of 'findPotentialEssentialCPs')
-- and does not exist morphism from S1 to B that commutes.
isEssentialDeleteUse :: DPO m => DPOConfig -> (m, m, m, m) -> Bool
isEssentialDeleteUse conf (l1',c,m1,m2) = null commuting
  where
    (_,o1) = calculatePullback (compose c m1) m2
    alls1 = findMorphismsFromDomains conf o1 l1'
    commuting = filter (\s1 -> compose s1 l1' == o1) alls1

findMorphismsFromDomains :: FindMorphism m => DPOConfig -> m -> m -> [m]
findMorphismsFromDomains conf  a b =
  findMorphisms (matchRestrictionToMorphismType $ matchRestriction conf) (domain a) (domain b)

-- Check if it is correct.
findAllEssentialProduceDangling :: --(DPO m, EpiPairs m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
--findAllEssentialProduceDangling conf p1 p2 =
--  filter
--    (\(CriticalPair (m1,m2) Nothing Nothing ProduceDangling) -> isEssentialDeleteUse conf p2 (m2,m1))
--    (findAllProduceDangling conf p1 p2)

-- Returning empty for while.
findAllEssentialProduceDangling _ _ _ = []

-- TODO
-- Returning all produce forbid for while
findAllEssentialProduceForbid :: --(DPO m, EpiPairs m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
--findAllEssentialProduceForbid conf p1 p2 = findAllProduceForbid conf p1 p2

-- Returning empty for while
findAllEssentialProduceForbid _ _ _ = []

