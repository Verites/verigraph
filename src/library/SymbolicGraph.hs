{-|
An implementation of symbolic graphs.

Symbolic graphs are graphs with variables as attributes, accompanied by a set of restrictions that
describes the possible values of these variables. The variables and restrictions are interpreted
with respect to the algebra of integers with addition, subtraction and multiplication.

The implementation is based on the @Graph.Graph@ module. Currently, it only supports untyped graphs
where nodes have a single attribute.

Operation comments contain the operation time complexity in the Big-O notation
<http://en.wikipedia.org/wiki/Big_O_notation>, denoting by /v/ the number of nodes in a graph, and
by /e/ the number of edges in a graph.
-}
module SymbolicGraph
  (
  -- * Symbolic Graph type
    SymbolicGraph
  , NodeId
  , Node
  , node
  , nodeId
  , nodeAttribute
  , EdgeId
  , Edge
  , edge
  , edgeId
  , sourceId
  , targetId

  -- * Contexts and graph traversal
  , NodeInContext
  , EdgeInContext
  , NodeContext
  , incidentEdges
  , incomingEdges
  , outgoingEdges

  -- * Query
  , isEmpty
  , isNodeOf
  , isEdgeOf
  , lookupNode
  , lookupNodeInContext
  , lookupEdge
  , lookupEdgeInContext


  -- * Construction
  , empty
  , fromNodesAndEdges

  -- * Insertion
  , insertNode
  , insertEdge

  -- * Delete
  , deleteNode
  , deleteEdge

  -- * Update
  , updateNodeAttribute
  , updateNodeAttributes

  -- * Conversion
  -- ** Lists
  , nodes
  , edges
  , restrictions
  , nodeIds
  , edgeIds
  , nodesInContext
  , edgesInContext
  ) where

import           SymbolicGraph.Internal
