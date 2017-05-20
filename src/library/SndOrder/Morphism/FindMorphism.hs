module SndOrder.Morphism.FindMorphism () where

import           Abstract.Category.DPO
import           Abstract.Category.FinitaryCategory
import           SndOrder.Morphism.Core
import           TypedGraph.Morphism

instance FindMorphism (RuleMorphism a b) where
  -- | A match between two first-order rules (desconsidering the NACs)
  findMorphisms prop l g = map (buildPair l g) rightMatch
    where
      matchesK = findMorphisms prop (domain (getLHS l)) (domain (getLHS g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch

  partialInjectiveMatches n m = map (buildPair (codomain n) (codomain m)) rightMatch
    where
      matchesK = partialInjectiveMatches (mappingInterface n) (mappingInterface m)
      leftMatch = concatMap (leftPartInj n m) matchesK
      rightMatch = concatMap (rightPartInj n m) leftMatch

  induceSpanMorphism = error "induceSpanMorphism not implemented for RuleMorphism"

  findCospanCommuter conf morphismOne morphismTwo = commuterMorphisms
    where
      allMorphisms  = findMorphisms conf (domain morphismOne) (domain morphismTwo)
      commuterMorphisms = filter (\x -> morphismOne == morphismTwo <&> x) allMorphisms

---- leftPartInj and leftM:
-- They receive a match between rule interfaces and build all pairs of
-- left and interface morphisms where the left morphisms form valid rule

leftPartInj :: RuleMorphism a b -> RuleMorphism a b -> TypedGraphMorphism a b -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
leftPartInj nac match mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = partialInjectiveMatches (mappingLeft nac) (mappingLeft match)
    commuting = filter (\m -> m <&> getLHS (codomain nac) == getLHS (codomain match) <&> mapK) matchesL

leftM :: MorphismType -> Production (TypedGraphMorphism a b) -> Production (TypedGraphMorphism a b) -> TypedGraphMorphism a b -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = findMorphisms prop (codomain (getLHS l)) (codomain (getLHS g))
    commuting = filter (\m -> m <&> getLHS l == getLHS g <&> mapK) matchesL

---- rightPartInj and rightM:
-- They receive a pair of left and interface morphisms between rules and
-- add all right morphisms where they respect a valid rule

rightPartInj :: RuleMorphism a b -> RuleMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b) -> [(TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)]
rightPartInj nac match (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = partialInjectiveMatches (mappingRight nac) (mappingRight match)
    commuting = filter (\m -> m <&> getRHS (codomain nac) == getRHS (codomain match) <&> mapK) matchesR

rightM :: MorphismType -> Production (TypedGraphMorphism a b) -> Production (TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b) -> [(TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = findMorphisms prop (codomain (getRHS l)) (codomain (getRHS g))
    commuting = filter (\m -> m <&> getRHS l == getRHS g <&> mapK) matchesR

-- kind of curry for three arguments
buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3
