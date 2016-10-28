module Analysis.EssentialCriticalPairs
 ( namedEssentialCriticalPairs,
   findAllEssentialDeleteUse,
   findAllEssentialProduceForbid,
   findAllEssentialProduceDangling
   ) where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW
import           Analysis.CriticalPairs
import           Analysis.DiagramAlgorithms


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

-- | Filter all Delete-Use selecting just the essentials
findAllEssentialDeleteUse :: (EpiPairs m, DPO m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllEssentialDeleteUse conf p1 p2 =
  filter
    (\(CriticalPair m Nothing Nothing DeleteUse) -> isEssentialDeleteUse conf p1 m)
    (findAllDeleteUse conf p1 p2)

-- Check if it is correct
findAllEssentialProduceDangling :: (DPO m, EpiPairs m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllEssentialProduceDangling conf p1 p2 =
  filter
    (\(CriticalPair (m1,m2) Nothing Nothing ProduceDangling) -> isEssentialDeleteUse conf p2 (m2,m1))
    (findAllProduceDangling conf p1 p2)

-- TODO
-- Returning all produce forbid for while
findAllEssentialProduceForbid :: (DPO m, EpiPairs m) =>
  DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllEssentialProduceForbid conf p1 p2 = findAllProduceForbid conf p1 p2

