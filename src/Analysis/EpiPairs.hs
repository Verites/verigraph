module Analysis.EpiPairs
 ( createPairs
 , commutingPairs) where

import           Abstract.Morphism        (compose)
import           Analysis.GPToVeri        (mountTGMBoth)
import           Analysis.GraphPart       (genEqClass)
import           Analysis.VeriToGP        (mixTGM)
import           Graph.TypedGraphMorphism (TypedGraphMorphism)

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairs :: TypedGraphMorphism a b
     -> TypedGraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
createPairs m1 m2 = map (mountTGMBoth m1 m2) (genEqClass (mixTGM m1 m2))

-- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes, considering they have same domain
commutingPairs :: TypedGraphMorphism a b
     -> TypedGraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
commutingPairs m1 m2 = filt
  where
    allPairs = createPairs m1 m2
    filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
