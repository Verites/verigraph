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
import           Abstract.DPO
import           Abstract.Valid
import           Graph.Graph              as G
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.TypedGraphMorphism as TGM

type GraphRule a b = Production (TypedGraphMorphism a b)

-- | Create a rule based on both typed graph morphisms and a list of NAC's.
graphRule :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b] -> GraphRule a b
graphRule = production

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
