{-# LANGUAGE TypeFamilies #-}

module GraphRule (GraphRule) where

import GraphRuleClass
import TypedMorphism (TypedMorphism)
import TypedMorphismClass
import MorphismClass
import Morphism

data GraphRule a b = GraphRule {
                          leftSide  :: TypedMorphism a b
                        , rightSide :: TypedMorphism a b
                     }

instance GraphRuleClass (GraphRule a b) where
    type T (GraphRule a b) = TypedMorphism a b

    left  = leftSide
    right = rightSide

    graphRule = GraphRule
