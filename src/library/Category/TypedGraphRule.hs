module Category.TypedGraphRule
( RuleMorphism(RuleMorphism)
, mappingLeft
, mappingRight
, mappingInterface
, TGRuleCat
, TGRuleConfig(..)
, runCat
, getTypeGraph
) where

import           Category.TypedGraphRule.Category
import           Category.TypedGraphRule.FindMorphism ()