module Graph.GraphRule (
    -- * Types
      GraphRule
    -- * Basic Functions
    , graphRule
    , left
    , right
    , nacs
    , inverseGR
) where

import Graph.TypedGraphMorphism (TypedGraphMorphism)
import Abstract.Morphism
import Abstract.Valid

data GraphRule a b = GraphRule {
                          leftSide  :: TypedGraphMorphism a b
                        , rightSide :: TypedGraphMorphism a b
                        , getNacs   :: [TypedGraphMorphism a b]
                     } deriving (Show, Read)

-- | Return the left-side (deletion) typed graph morphism of the rule.
left  = leftSide
-- | Return the right-side (creation) typed graph morphism of the rule.
right = rightSide
-- | Return a list of all NAC's.
nacs  = getNacs
-- | Create a rule based on both typed graph morphisms and a list of NAC's.
graphRule = GraphRule

-- | Revert a Rule
inverseGR :: GraphRule a b -> GraphRule a b
inverseGR x = GraphRule (right x) (left x) []--(nacs x)

instance Valid (GraphRule a b) where
    valid (GraphRule lside rside nacs) =
        valid lside &&
        valid rside &&
        all valid nacs &&
        (domain lside) == (domain rside)
