module Analysis.EpimorphicPairs (
  createJointlyEpimorphicPairsFromCodomains
) where

import        Abstract.AdhesiveHLR

-- | Create all jointly epimorphic pairs of morphisms from the codomains of
-- the given morphisms.
-- The flag indicates only monomorphic morphisms.
createJointlyEpimorphicPairsFromCodomains :: (EpiPairs m) => MatchRestriction -> m -> m -> [(m, m)]
createJointlyEpimorphicPairsFromCodomains inj m1 m2 =
  createJointlyEpimorphicPairs (inj == MonoMatches) (codomain m1) (codomain m2)
