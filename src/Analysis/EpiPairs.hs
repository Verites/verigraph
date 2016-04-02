module Analysis.EpiPairs
 (createPairs) where

import           Analysis.GPToVeri        (mountTGMBoth)
import           Analysis.GraphPart       (genEqClass)
import           Analysis.VeriToGP        (mixTGM)
import           Graph.TypedGraphMorphism (TypedGraphMorphism)

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairs :: TypedGraphMorphism a b
     -> TypedGraphMorphism a b
     -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
createPairs m1 m2 = map (mountTGMBoth m1 m2) (genEqClass (mixTGM m1 m2))
