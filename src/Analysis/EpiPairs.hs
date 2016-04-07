module Analysis.EpiPairs
 ( createPairs
 , createPairsCodomain
 , commutingPairs) where

import           Abstract.Morphism        (codomain,compose)
import           Analysis.GPToVeri        (mountTGMBoth)
import           Analysis.GraphPart       (genEqClass)
import           Analysis.VeriToGP        (mixTGM)
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.GraphMorphism      (GraphMorphism)

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairs :: GraphMorphism a b
     -> GraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
createPairs m1 m2 = map (mountTGMBoth m1 m2) (genEqClass (mixTGM m1 m2))

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairsCodomain :: TypedGraphMorphism a b
     -> TypedGraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
createPairsCodomain m1 m2 = createPairs (codomain m1) (codomain m2)

-- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes, considering they have same domain
commutingPairs :: TypedGraphMorphism a b
     -> TypedGraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
commutingPairs m1 m2 = filt
  where
    allPairs = createPairs (codomain m1) (codomain m2)
    filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
