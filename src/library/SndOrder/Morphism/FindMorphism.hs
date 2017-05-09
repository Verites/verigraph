module SndOrder.Morphism.FindMorphism where

import           Abstract.DPO
import           Abstract.Morphism
import           TypedGraph.Morphism

import           SndOrder.Morphism.Core

instance FindMorphism (RuleMorphism a b) where
  -- | A match between two first order rules (desconsidering the NACs)
  findMorphisms prop l g = map (buildPair l g) rightMatch
    where
      matchesK = findMorphisms prop (domain (getLHS l)) (domain (getLHS g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch

  partialInjectiveMatches n m =
    filter
      (\q ->
        isPartiallyMonomorphic (mappingLeft n) (mappingLeft q) &&
        isPartiallyMonomorphic (mappingInterface n) (mappingInterface q) &&
        isPartiallyMonomorphic (mappingRight n) (mappingRight q))
      (findAllMorphisms (codomain n) (codomain m))

  induceSpanMorphism = error "induceSpanMorphism not implemented for RuleMorphism"

  findCospanCommuter conf morphismOne morphismTwo = commuterMorphisms
    where
      allMorphisms  = findMorphisms conf (domain morphismOne) (domain morphismTwo)
      commuterMorphisms = filter (\x -> morphismOne == compose x morphismTwo) allMorphisms

leftM :: FindMorphism t => MorphismType -> Production t -> Production t -> t -> [(t, t)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = findMorphisms prop (codomain (getLHS l)) (codomain (getLHS g))
    commuting = filter (\m -> compose (getLHS l) m == compose mapK (getLHS g)) matchesL

-- commutes right side
rightM :: FindMorphism t =>  MorphismType -> Production t -> Production t -> (t, t) -> [(t, t, t)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = findMorphisms prop (codomain (getRHS l)) (codomain (getRHS g))
    commuting = filter (\m -> compose (getRHS l) m == compose mapK (getRHS g)) matchesR

-- kind of curry for three arguments
buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3
