{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.EpiPairs () where

import           Abstract.AdhesiveHLR
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Partitions.GPToVeri      (mountTGMBoth)
import           Partitions.GraphPart     (genGraphEqClass)
import           Partitions.VeriToGP      (mixGM,mixNac)

instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createPairs m1 m2 = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM m1 m2))
  
  createPairsNac r nac = map (mountTGMBoth r (codomain nac)) (genGraphEqClass (mixNac r nac))

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes, considering they have same domain
  commutingPairs m1 m2 = filt
    where
      allPairs = createPairs (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
