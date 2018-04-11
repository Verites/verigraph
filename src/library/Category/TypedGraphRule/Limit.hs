{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Category.TypedGraphRule.Limit where

import           Abstract.Category
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Category.TypedGraph
import           Category.TypedGraph.CommutingSquares
import           Category.TypedGraphRule.Category

instance Complete (RuleMorphism a b) where
  -- TODO: implement missing methods of Config for RuleMorphism

  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  -- @
  calculatePullback (RuleMorphism fA _ fL fK fR) (RuleMorphism gB _ gL gK gR) = (f',g')
    where
      (f'L, g'L) = calculatePullback fL gL
      (f'K, g'K) = calculatePullback fK gK
      (f'R, g'R) = calculatePullback fR gR

      l = commutingMorphism
            (leftMorphism gB <&> f'K) f'L
            (leftMorphism fA <&> g'K) g'L

      r = commutingMorphism
            (rightMorphism gB <&> f'K) f'R
            (rightMorphism fA <&> g'K) g'R

      x = Production l r []
      f' = RuleMorphism x gB f'L f'K f'R
      g' = RuleMorphism x fA g'L g'K g'R



instance Cocomplete (RuleMorphism a b) where

  calculateCoequalizer (RuleMorphism _ ruleB fL fK fR) (RuleMorphism _ _ gL gK gR) =
    RuleMorphism ruleB coequalizerRule eqL eqK eqR

    where
      eqL = coequalizerTGM fL gL
      eqK = coequalizerTGM fK gK
      eqR = coequalizerTGM fR gR

      l = commutingMorphismSameDomain eqK (eqL <&> leftMorphism ruleB) eqK (eqL <&> leftMorphism ruleB)
      r = commutingMorphismSameDomain eqK (eqR <&> rightMorphism ruleB) eqK (eqR <&> rightMorphism ruleB)

      coequalizerRule = Production l r []

  calculateNCoequalizer = error "calculateNCoequalizer for Second-order not implemented"

  calculateCoproduct rule1 rule2 = (m1,m2)
    where
      (l1,l2) = calculateCoproduct (leftObject rule1) (leftObject rule2)
      (k1,k2) = calculateCoproduct (interfaceObject rule1) (interfaceObject rule2)
      (r1,r2) = calculateCoproduct (rightObject rule1) (rightObject rule2)

      l = commutingMorphismSameDomain k1 (l1 <&> leftMorphism rule1) k2 (l2 <&> leftMorphism rule2)
      r = commutingMorphismSameDomain k1 (r1 <&> rightMorphism rule1) k2 (r2 <&> rightMorphism rule2)

      coproductRule = Production l r []

      m1 = RuleMorphism rule1 coproductRule l1 k1 r1
      m2 = RuleMorphism rule2 coproductRule l2 k2 r2

  calculateNCoproduct = error "calculateNCoproduct for Second-order not implemented"

  initialObject = initialRule . leftMorphism . domain

  morphismFromInitialTo rule =
    RuleMorphism (initialRule $ leftMorphism rule) rule
      (morphismFromInitialTo $ leftObject rule)
      (morphismFromInitialTo $ interfaceObject rule)
      (morphismFromInitialTo $ rightObject rule)

initialRule :: TypedGraphMorphism a b -> TypedGraphRule a b
initialRule morph = Production idInitial idInitial []
  where idInitial = identity (initialObject morph)

coequalizerTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
coequalizerTGM = calculateCoequalizer

coproductCod :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductCod a b = calculateCoproduct (codomain a) (codomain b)

coproductDom :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductDom a b = calculateCoproduct (domain a) (domain b)
