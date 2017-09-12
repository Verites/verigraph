module Category.TypedGraphRule.FindMorphism () where

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO
import           Category.TypedGraph                (TypedGraphMorphism)
import           Category.TypedGraphRule.Category

type FstOrderMorphismClass n e = MorphismClass (TypedGraphMorphism n e)

instance FindMorphism (RuleMorphism n e) where
  -- | A match between two first-order rules (desconsidering the NACs)
  findMorphisms cls' l g = map (buildPair l g) rightMatch
    where
      cls = toFstOrderMorphismClass cls'
      matchesK = findMorphisms cls (interfaceObject l) (interfaceObject g)
      leftMatch = concatMap (leftM cls l g) matchesK
      rightMatch = concatMap (rightM cls l g) leftMatch

  induceSpanMorphism = error "induceSpanMorphism not implemented for RuleMorphism"

  -- TODO: rewrite in terms of findSpanCommuters of first order morphisms, for efficiency
  findSpanCommuters conf morphismOne morphismTwo = commuterMorphisms
    where
      allMorphisms  = findMorphisms conf (domain morphismOne) (domain morphismTwo)
      commuterMorphisms = filter (\x -> x <&> morphismOne == morphismTwo) allMorphisms

  -- TODO: rewrite in terms of findCospanCommuters of first order morphisms, for efficiency
  findCospanCommuters conf morphismOne morphismTwo = commuterMorphisms
    where
      allMorphisms  = findMorphisms conf (domain morphismOne) (domain morphismTwo)
      commuterMorphisms = filter (\x -> morphismOne == morphismTwo <&> x) allMorphisms

---- leftPartInj and leftM:
-- They receive a match between rule interfaces and build all pairs of
-- left and interface morphisms where the left morphisms form valid rule

leftM :: FstOrderMorphismClass n e -> TypedGraphRule n e -> TypedGraphRule n e -> TypedGraphMorphism n e -> [(TypedGraphMorphism n e, TypedGraphMorphism n e)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = findMorphisms prop (leftObject l) (leftObject g)
    commuting = filter (\m -> m <&> leftMorphism l == leftMorphism g <&> mapK) matchesL

---- rightPartInj and rightM:
-- They receive a pair of left and interface morphisms between rules and
-- add all right morphisms where they respect a valid rule

rightM :: FstOrderMorphismClass n e -> TypedGraphRule n e -> TypedGraphRule n e
        -> (TypedGraphMorphism n e, TypedGraphMorphism n e)
        -> [(TypedGraphMorphism n e, TypedGraphMorphism n e, TypedGraphMorphism n e)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = findMorphisms prop (rightObject l) (rightObject g)
    commuting = filter (\m -> m <&> rightMorphism l == rightMorphism g <&> mapK) matchesR

-- kind of curry for three arguments
buildPair :: TypedGraphRule n e -> TypedGraphRule n e
          -> (TypedGraphMorphism n e, TypedGraphMorphism n e, TypedGraphMorphism n e)
          -> RuleMorphism n e
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3
