module SymbolicGraph
  (
  -- * Symbolic Graph type
    SymbolicGraph
  , restrictions
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
  , SymbolicGraph.Core.null
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

  -- * Conversion
  -- ** Lists
  , nodes
  , edges
  , nodeIds
  , edgeIds
  , nodesInContext
  , edgesInContext

  -- * Map
  , mapNodeAttributes
  ) where

import           SymbolicGraph.Core
