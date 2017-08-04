{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraphRule
( TypedGraphRule
, Production(..)
, RuleMorphism(RuleMorphism)
, mappingLeft
, mappingRight
, mappingInterface
, TGRuleCat
, TGRuleConfig(..)
, MatchRestriction(..)
, runCat
, liftTGraph
) where

import Abstract.Category.NewClasses
import           Base.Valid
import           Category.TypedGraph (TGraphCat)
import           Category.TypedGraphRule.Category
import           Category.TypedGraphRule.FindMorphism ()
import           Category.TypedGraphRule.Limit ()
import           Category.TypedGraphRule.Adhesive ()
import           Rewriting.DPO.TypedGraph 

instance Valid (TGRuleCat n e) (TypedGraphRule n e) where
  validator = mapValidator liftTGraph . validator

instance Valid (TGRuleCat n e) (RuleMorphism n e) where
  validator (RuleMorphism dom cod mapL mapK mapR) = do
    withContext "domain" (validateTGraph dom)
    withContext "codomain" (validateTGraph cod)
    withContext "left-hand graph morphism" (validateTGraph mapL)
    withContext "interface graph morphism" (validateTGraph mapK)
    withContext "right-hand graph morphism" (validateTGraph mapR)
    ensure (leftMorphism cod <&> mapK == mapL <&> leftMorphism dom) "Left square doesn't commute"
    ensure (rightMorphism cod <&> mapK == mapR <&> rightMorphism dom) "Right square doesn't commute"
    where
      validateTGraph :: Valid (TGraphCat n e) a => a -> Validator (TGRuleCat n e)
      validateTGraph = mapValidator liftTGraph . validator