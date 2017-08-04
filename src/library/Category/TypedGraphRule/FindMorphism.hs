{-# LANGUAGE MonadComprehensions #-}
module Category.TypedGraphRule.FindMorphism () where

import Control.Monad.List

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import qualified Category.TypedGraph as TGraph
import           Category.TypedGraphRule.Category
import Util.Monad


instance FindMorphism (CatM n e) (RuleMorphism n e) where
  -- | A match between two first-order rules (desconsidering the NACs)
  {-
             l1      r1
         L1◀─────K1─────▶R1
         │       │       │
      fL │     fK│     fR│
         ▼       ▼       ▼
         L2◀─────K2─────▶R2
             l2      r2      -}
  findMorphisms cls' p1 p2 = do
    cls <- asTGraphClass cls'
    liftFstOrder . runListT $
      [ RuleMorphism p1 p2 fL fK fR
          | fK <- pickOne $ findMorphisms cls (interfaceObject p1) (interfaceObject p2)
          , fL <- pickOne $ findSpanCommuters cls (leftMorphism p1) (leftMorphism p2 <&> fK)
          , fR <- pickOne $ findSpanCommuters cls (rightMorphism p1) (rightMorphism p2 <&> fK)
      ]

  -- TODO: implement induceSpanMorphism for RuleMorphism
  induceSpanMorphism = error "induceSpanMorphism not implemented for RuleMorphism"

  findSpanCommuters = findRuleCommuters findSpanCommuters (\f g h -> f <&> g == h)
  findCospanCommuters = findRuleCommuters findCospanCommuters (\f g h -> h <&> f == g)

findRuleCommuters :: (MorphismClass (TGraph.CatM n e) -> TGraph.Morphism n e -> TGraph.Morphism n e -> TGraph.CatM n e [TGraph.Morphism n e])
                  -> (TGraph.Morphism n e -> TGraph.Morphism n e -> TGraph.Morphism n e -> Bool)
                  -> MorphismClass (CatM n e) -> RuleMorphism n e -> RuleMorphism n e -> CatM n e [RuleMorphism n e]
findRuleCommuters findGraphCommuters checkCommutativity cls' g h = do
    cls <- asTGraphClass cls'
    let (p2, p3) = (codomain g, codomain h)
    liftFstOrder . runListT $
      [ RuleMorphism p2 p3 fL fK fR
          | fK <- pickOne $ findGraphCommuters cls (mappingInterface g) (mappingInterface h)
          , fL <- pickOne $ findSpanCommuters cls (leftMorphism p2) (leftMorphism p3 <&> fK)
          , checkCommutativity fL (mappingLeft g) (mappingLeft h)
          , fR <- pickOne $ findSpanCommuters cls (rightMorphism p2) (rightMorphism p3 <&> fK)
          , checkCommutativity fR (mappingRight g) (mappingRight h)
      ]