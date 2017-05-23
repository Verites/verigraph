{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule.Cocomplete (

  calculateCoequalizer,
  calculateCoproduct,
  calculatePushout

) where

import           Abstract.Category.Cocomplete
import           Abstract.Category.FinitaryCategory as FC
import           Abstract.Rewriting.DPO
import           Category.TypedGraphRule            ()
import           Data.TypedGraph.Morphism
import           Category.TypedGraph.CommutingSquares
import           SndOrder.Morphism.Core

instance Cocomplete (RuleMorphism a b) where

  calculateCoequalizer (RuleMorphism _ ruleB fL fK fR) (RuleMorphism _ _ gL gK gR) =
    RuleMorphism ruleB coequalizerRule eqL eqK eqR

    where
      eqL = coequalizerTGM fL gL
      eqK = coequalizerTGM fK gK
      eqR = coequalizerTGM fR gR

      l = commutingMorphismSameDomain eqK (eqL <&> getLHS ruleB) eqK (eqL <&> getLHS ruleB)
      r = commutingMorphismSameDomain eqK (eqR <&> getRHS ruleB) eqK (eqR <&> getRHS ruleB)

      coequalizerRule = buildProduction l r []

  calculateNCoequalizer = error "calculateNCoequalizer for Second-order not implemented"

  calculateCoproduct rule1 rule2 = (m1,m2)
    where
      (l1,l2) = coproductCod (getLHS rule1) (getLHS rule2)
      (k1,k2) = coproductDom (getLHS rule1) (getLHS rule2)
      (r1,r2) = coproductCod (getRHS rule1) (getRHS rule2)

      l = commutingMorphismSameDomain k1 (l1 <&> getLHS rule1) k2 (l2 <&> getLHS rule2)
      r = commutingMorphismSameDomain k1 (r1 <&> getRHS rule1) k2 (r2 <&> getRHS rule2)

      coproductRule = buildProduction l r []

      m1 = RuleMorphism rule1 coproductRule l1 k1 r1
      m2 = RuleMorphism rule2 coproductRule l2 k2 r2

  calculateNCoproduct = error "calculateNCoproduct for Second-order not implemented"

  initialObject morph = buildProduction (FC.identity initGraph) (FC.identity initGraph) []
    where
      initGraph = initialObject (getLHS (domain morph))

coequalizerTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
coequalizerTGM = calculateCoequalizer

coproductCod :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductCod a b = calculateCoproduct (codomain a) (codomain b)

coproductDom :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductDom a b = calculateCoproduct (domain a) (domain b)
