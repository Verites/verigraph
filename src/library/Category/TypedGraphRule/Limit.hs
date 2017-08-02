{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Category.TypedGraphRule.Limit where

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Category.TypedGraph ()
import           Category.TypedGraph.CommutingSquares
import           Category.TypedGraphRule.Category

instance Cocomplete (TGRuleCat n e) (RuleMorphism n e) where

  getInitialObject = do
    initialGraph <- liftTGraph getInitialObject
    return $ buildProduction (identity initialGraph) (identity initialGraph) []

  getMorphismFromInitialObjectTo rule = do
    fL <- liftTGraph $ getMorphismFromInitialObjectTo (leftObject rule)
    fK <- liftTGraph $ getMorphismFromInitialObjectTo (interfaceObject rule)
    fR <- liftTGraph $ getMorphismFromInitialObjectTo (rightObject rule)
    let initialGraph = domain fL
    let initialRule = buildProduction (identity initialGraph) (identity initialGraph) []
    return (RuleMorphism initialRule rule fL fK fR)

  calculateCoequalizer (RuleMorphism _ ruleB fL fK fR) (RuleMorphism _ _ gL gK gR) = do
    eqL <- liftTGraph $ calculateCoequalizer fL gL
    eqK <- liftTGraph $ calculateCoequalizer fK gK
    eqR <- liftTGraph $ calculateCoequalizer fR gR

    l <- liftTGraph $ commutingMorphismSameDomain eqK (eqL <&> leftMorphism ruleB) eqK (eqL <&> leftMorphism ruleB)
    r <- liftTGraph $ commutingMorphismSameDomain eqK (eqR <&> rightMorphism ruleB) eqK (eqR <&> rightMorphism ruleB)
    return (RuleMorphism ruleB (buildProduction l r []) eqL eqK eqR)

  calculateCoproduct rule1 rule2 = do
    (l1, l2) <- liftTGraph $ calculateCoproduct (leftObject rule1) (leftObject rule2)
    (k1, k2) <- liftTGraph $ calculateCoproduct (interfaceObject rule1) (interfaceObject rule2)
    (r1, r2) <- liftTGraph $ calculateCoproduct (rightObject rule1) (rightObject rule2)

    l <- liftTGraph $ commutingMorphismSameDomain k1 (l1 <&> leftMorphism rule1) k2 (l2 <&> leftMorphism rule2)
    r <- liftTGraph $ commutingMorphismSameDomain k1 (r1 <&> rightMorphism rule1) k2 (r2 <&> rightMorphism rule2)

    let coproductRule = buildProduction l r []
    let m1 = RuleMorphism rule1 coproductRule l1 k1 r1
    let m2 = RuleMorphism rule2 coproductRule l2 k2 r2
    return (m1, m2)
 
