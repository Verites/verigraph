{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.FindMorphismSndOrder () where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.RuleMorphism

instance FindMorphism (RuleMorphism a b) where
  -- | A match between two rules, only considers monomorphic matches morphisms:
  -- (desconsidering the NACs)
  matches prop l g = map (buildPair l g) rightMatch
    where
      matchesK = matches prop (domain (left l)) (domain (left g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch
  
  partInjMatches n m =
    filter
      (\q ->
        (partiallyMonomorphic (mappingLeft n) (mappingLeft q)) &&
        (partiallyMonomorphic (mappingInterface n) (mappingInterface q)) &&
        (partiallyMonomorphic (mappingRight n) (mappingRight q)))
      (matches ALL (codomain n) (codomain m))

-- commutes left side
leftM :: FindMorphism t => PROP -> Production t -> Production t -> t -> [(t, t)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = matches prop (codomain (left l)) (codomain (left g))
    commuting = filter (\m -> compose (left l) m == compose mapK (left g)) matchesL

-- commutes right side
rightM :: FindMorphism t =>  PROP -> Production t -> Production t -> (t, t) -> [(t, t, t)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = matches prop (codomain (right l)) (codomain (right g))
    commuting = filter (\m -> compose (right l) m == compose mapK (right g)) matchesR

-- kind of curry for three arguments
buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3

