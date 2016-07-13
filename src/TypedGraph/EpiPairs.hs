{-# OPTIONS_GHC -fno-warn-orphans #-}

module TypedGraph.EpiPairs where

import           Abstract.AdhesiveHLR
import           Graph.Graph              (empty)
import           Graph.GraphMorphism      (gmbuild)
import           TypedGraph.Morphism (TypedGraphMorphism)
import           Partitions.GPToVeri      (mountTGMBoth)
import           Partitions.GraphPart     (genGraphEqClass)
import           Partitions.VeriToGP      (mixGM,mixNac)

instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createPairs inj m1 m2 = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM (m1,inj) (m2,inj)))

  partitions inj m1 = map fst part
    where
      m2 = gmbuild empty empty [] []
      part = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM (m1,inj) (m2,inj)))

  -- | Create all jointly surjective pairs of @m1@ and @m2@ with some of both injective
  --createPairsAlt (m1,inj1) (m2,inj2) = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM (m1,inj1) (m2,inj2)))

  createPairsNac nacInj inj r nac = map (mountTGMBoth r (codomain nac)) (genGraphEqClass (mixNac (r, inj == MonoMatches) (nac, nacInj == MonoNacSatisfaction)))

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes,
  -- considering they have same domain
  -- and flags indicating the injective of each morphism
  commutingPairsAlt (m1,inj1) (m2,inj2) = filt
    where
      cod1 = codomain m1
      cod2 = codomain m2
      allPairs = map (mountTGMBoth cod1 cod2) (genGraphEqClass (mixGM (cod1,inj1) (cod2,inj2)))
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
