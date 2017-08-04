{-| Defines the category of typed graph rules and their morphisms.

This module is inteded to be imported qualified, to avoid name clashes, as follows:

> import Category.TypedGraphRule (liftFstOrder, TypedGraphRule, Production(..), RuleMorphism(RuleMorphism), mappingLeft, mappingRight, mappingInterface)
> import qualified Category.TypedGraphRule as TGRule
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraphRule
( TypedGraphRule
, Production(..)
, Morphism
, RuleMorphism(RuleMorphism)
, mappingLeft
, mappingRight
, mappingInterface
, CatM
, Config(..)
, MatchRestriction(..)
, runCat
, liftFstOrder
) where

import Abstract.Category.NewClasses
import           Base.Valid
import qualified Category.TypedGraph as TGraph
import           Category.TypedGraphRule.Category
import           Category.TypedGraphRule.FindMorphism ()
import           Category.TypedGraphRule.Limit ()
import           Category.TypedGraphRule.Adhesive ()
import           Rewriting.DPO.TypedGraph 

instance Valid (CatM n e) (TypedGraphRule n e) where
  validator = mapValidator liftFstOrder . validator

instance Valid (CatM n e) (RuleMorphism n e) where
  validator (RuleMorphism dom cod mapL mapK mapR) = do
    withContext "domain" (validateTGraph dom)
    withContext "codomain" (validateTGraph cod)
    withContext "left-hand graph morphism" (validateTGraph mapL)
    withContext "interface graph morphism" (validateTGraph mapK)
    withContext "right-hand graph morphism" (validateTGraph mapR)
    ensure (leftMorphism cod <&> mapK == mapL <&> leftMorphism dom) "Left square doesn't commute"
    ensure (rightMorphism cod <&> mapK == mapR <&> rightMorphism dom) "Right square doesn't commute"
    where
      validateTGraph :: Valid (TGraph.CatM n e) a => a -> Validator (CatM n e)
      validateTGraph = mapValidator liftFstOrder . validator