module GraphRule (
      graphRule
    , GraphRule
    , left
    , right
    , nacs
) where

import TypedGraphMorphism (TypedGraphMorphism)
import Morphism
import Valid

data GraphRule a b = GraphRule {
                          leftSide  :: TypedGraphMorphism a b
                        , rightSide :: TypedGraphMorphism a b
                        , getNacs   :: [TypedGraphMorphism a b]
                     } deriving (Show, Read)

left  = leftSide
right = rightSide
nacs  = getNacs
graphRule = GraphRule

instance (Eq a, Eq b) => Valid (GraphRule a b) where
    valid (GraphRule lside rside nacs) =
        valid lside &&
        valid rside &&
        all valid nacs &&
        (domain lside) == (domain rside)
