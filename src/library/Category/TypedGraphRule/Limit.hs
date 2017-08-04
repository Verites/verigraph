{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Category.TypedGraphRule.Limit where

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Category.TypedGraph ()
import           Category.TypedGraph.CommutingSquares
import           Category.TypedGraphRule.Category

instance Cocomplete (CatM n e) (RuleMorphism n e) where

  getInitialObject = do
    initialGraph <- liftFstOrder getInitialObject
    return $ buildProduction (identity initialGraph) (identity initialGraph) []

  getMorphismFromInitialObjectTo rule = do
    fL <- liftFstOrder $ getMorphismFromInitialObjectTo (leftObject rule)
    fK <- liftFstOrder $ getMorphismFromInitialObjectTo (interfaceObject rule)
    fR <- liftFstOrder $ getMorphismFromInitialObjectTo (rightObject rule)
    let initialGraph = domain fL
    let initialRule = buildProduction (identity initialGraph) (identity initialGraph) []
    return (RuleMorphism initialRule rule fL fK fR)

  calculateCoequalizer (RuleMorphism _ ruleB fL fK fR) (RuleMorphism _ _ gL gK gR) = do
    eqL <- liftFstOrder $ calculateCoequalizer fL gL
    eqK <- liftFstOrder $ calculateCoequalizer fK gK
    eqR <- liftFstOrder $ calculateCoequalizer fR gR

    l <- liftFstOrder $ commutingMorphismSameDomain eqK (eqL <&> leftMorphism ruleB) eqK (eqL <&> leftMorphism ruleB)
    r <- liftFstOrder $ commutingMorphismSameDomain eqK (eqR <&> rightMorphism ruleB) eqK (eqR <&> rightMorphism ruleB)
    return (RuleMorphism ruleB (buildProduction l r []) eqL eqK eqR)

  calculateCoproduct rule1 rule2 = do
    (l1, l2) <- liftFstOrder $ calculateCoproduct (leftObject rule1) (leftObject rule2)
    (k1, k2) <- liftFstOrder $ calculateCoproduct (interfaceObject rule1) (interfaceObject rule2)
    (r1, r2) <- liftFstOrder $ calculateCoproduct (rightObject rule1) (rightObject rule2)

    l <- liftFstOrder $ commutingMorphismSameDomain k1 (l1 <&> leftMorphism rule1) k2 (l2 <&> leftMorphism rule2)
    r <- liftFstOrder $ commutingMorphismSameDomain k1 (r1 <&> rightMorphism rule1) k2 (r2 <&> rightMorphism rule2)

    let coproductRule = buildProduction l r []
    let m1 = RuleMorphism rule1 coproductRule l1 k1 r1
    let m2 = RuleMorphism rule2 coproductRule l2 k2 r2
    return (m1, m2)
 
instance Complete (CatM n e) (RuleMorphism n e) where
  getFinalObject = do
    finalGraph <- liftFstOrder getFinalObject
    return $ buildProduction (identity finalGraph) (identity finalGraph) []

  getMorphismToFinalObjectFrom rule = do
    fL <- liftFstOrder $ getMorphismToFinalObjectFrom (leftObject rule)
    fK <- liftFstOrder $ getMorphismToFinalObjectFrom (interfaceObject rule)
    fR <- liftFstOrder $ getMorphismToFinalObjectFrom (rightObject rule)
    let finalGraph = domain fL
    let finalRule = buildProduction (identity finalGraph) (identity finalGraph) []
    return (RuleMorphism finalRule rule fL fK fR)

  -- TODO: implement calculateEqualizer

  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  -- @
  calculatePullback (RuleMorphism fA _ fL fK fR) (RuleMorphism gB _ gL gK gR) = liftFstOrder $ do
    (f'L, g'L) <- calculatePullback fL gL
    (f'K, g'K) <- calculatePullback fK gK
    (f'R, g'R) <- calculatePullback fR gR

    l <- commutingMorphism
          (leftMorphism gB <&> f'K) f'L
          (leftMorphism fA <&> g'K) g'L
    r <- commutingMorphism
          (rightMorphism gB <&> f'K) f'R
          (rightMorphism fA <&> g'K) g'R
    let
      x = buildProduction l r []
      f' = RuleMorphism x gB f'L f'K f'R
      g' = RuleMorphism x fA g'L g'K g'R
    return (f', g')
