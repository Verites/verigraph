{-# LANGUAGE TypeFamilies #-}

module GraphRule (GraphRule) where

import GraphRuleClass
import TypedMorphism (TypedMorphism)
import qualified TypedMorphismClass as T
import qualified MorphismClass as M
import Morphism
import Valid

data GraphRule a b = GraphRule {
                          leftSide  :: TypedMorphism a b
                        , rightSide :: TypedMorphism a b
                        , getNacs   :: [TypedMorphism a b]
                     }

instance GraphRuleClass (GraphRule a b) where
    type T (GraphRule a b) = TypedMorphism a b

    left  = leftSide
    right = rightSide
    nacs  = getNacs

    graphRule = GraphRule

instance (Eq a, Eq b) => Valid (GraphRule a b) where
    valid r = let lside = left r
                  rside = right r
              in valid lside &&
                 valid rside &&
                 (M.domain $ T.domain lside) ==
                 (M.domain $ T.domain rside)
