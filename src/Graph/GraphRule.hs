module Graph.GraphRule (
    -- * Types
      GraphRule
    -- * Basic Functions
    , graphRule
    , left
    , right
    , nacs
    , inverseWithoutNacs
    , deletedNodes
    , deletedEdges
    , createdNodes
    , createdEdges
) where

import           Abstract.Morphism
import           Abstract.Valid
import           Graph.Graph              as G
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.TypedGraphMorphism as TGM

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
inverseWithoutNacs :: GraphRule a b -> GraphRule a b
inverseWithoutNacs x = GraphRule (right x) (left x) []--(nacs x)

-- | Return the nodes deleted by a rule
deletedNodes :: GraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanNodesTyped (left r)

-- | Return the nodes created by a rule
createdNodes :: GraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanNodesTyped (right r)

-- | Return the edges deleted by a rule
deletedEdges :: GraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanEdgesTyped (left r)

-- | Return the edges created by a rule
createdEdges :: GraphRule a b -> [G.EdgeId]
createdEdges r = TGM.orphanEdgesTyped (right r)

instance Valid (GraphRule a b) where
    valid (GraphRule lside rside nacs) =
        valid lside &&
        valid rside &&
        all valid nacs &&
        domain lside == domain rside
