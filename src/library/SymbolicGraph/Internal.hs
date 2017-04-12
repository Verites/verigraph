{- |
= WARNING

This module is considered __internal__.

It should _not_ be imported anywhere except in other `SymbolicGraph` modules.
-}
module SymbolicGraph.Internal
  (
  -- * Symbolic Graph type
    SymbolicGraph(..)
  , NodeId(..)
  , Node(..)
  , node
  , nodeId
  , nodeAttribute
  , EdgeId(..)
  , Edge(..)
  , edge
  , edgeId
  , sourceId
  , targetId

  -- * Contexts and graph traversal
  , NodeContext
  , NodeInContext
  , EdgeInContext
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
  , nodeIds
  , edgeIds
  , nodesInContext
  , edgesInContext
  ) where

import           SymbolicGraph.DataAlgebra

import           Abstract.Valid
import           Graph.Graph               (EdgeId, Graph, NodeId)
import qualified Graph.Graph               as Graph

import qualified Data.Map                  as Map
import qualified Data.Set                  as Set



type InnerGraph =
  Graph (Maybe Variable) ()


-- | A graph with variables as attributes, as well a set of restrictions constraining the possible
-- values of these variables.
--
-- Every node and edge is identified by a unique integer. The "namespaces" of nodes and edges are
-- independent, i.e. the same number may be used to identify both a node and an edge within the
-- same graph.
--
-- Equality tests cost /O(v² + e²)/.
--
-- __Note:__ valid symbolic graphs may have unsatisfiable restrictions.
data SymbolicGraph =
  SymbGraph
    { graphPart    :: InnerGraph
    , restrictions :: [Restriction]
      -- ^ List of restrictions of the graph, constraining the values that its variables may take. /O(1)/.
    }
  deriving Eq


modifyGraphPart :: (InnerGraph -> InnerGraph) -> SymbolicGraph -> SymbolicGraph
{-# INLINE modifyGraphPart #-}
modifyGraphPart f graph =
  graph { graphPart = f (graphPart graph) }


-- | Nodes from within a symbolic graph, also containing their attribute.
newtype Node =
  Node { unNode :: Graph.Node (Maybe Variable) }


-- | Construct a node. Used to insert nodes into symbolic graphs.
node :: NodeId -> Maybe Variable -> Node
node n v =
  Node (Graph.Node n v)


nodeId :: Node -> NodeId
nodeId =
  Graph.nodeId . unNode


nodeAttribute :: Node -> Maybe Variable
nodeAttribute =
  Graph.nodeInfo . unNode


-- | Edges from within a symbolic graph, also containing their source and target.
newtype Edge =
  Edge { unEdge :: Graph.Edge () }


-- | Construct an edge. Used to insert edges into symbolic graphs.
edge :: EdgeId -> NodeId -> NodeId -> Edge
edge e src tgt =
  Edge (Graph.Edge e src tgt ())


edgeId :: Edge -> EdgeId
edgeId =
  Graph.edgeId . unEdge


sourceId :: Edge -> NodeId
sourceId =
  Graph.sourceId . unEdge


targetId :: Edge -> NodeId
targetId =
  Graph.targetId . unEdge



instance FreeVariables Node where

  {-# INLINE freeVariablesOf #-}
  freeVariablesOf (Node n) =
    case Graph.nodeInfo n of
      Nothing ->
        Set.empty

      Just v ->
        Set.singleton v

  {-# INLINE renameVariables #-}
  renameVariables subst (Node n) =
    let
      v =
        Graph.nodeInfo n

      v' =
        Map.findWithDefault <$> v <*> v <*> pure subst
    in
      Node $ n { Graph.nodeInfo = v' }



instance FreeVariables SymbolicGraph where

  {-# INLINE freeVariablesOf #-}
  freeVariablesOf graph =
    Set.union
      (freeVariablesOf $ restrictions graph)
      (freeVariablesOf $ nodes graph)

  {-# INLINE renameVariables #-}
  renameVariables subst (SymbGraph graph restrictions) =
      SymbGraph
        { graphPart = Graph.mapNodes (fmap renameVariable . Graph.nodeInfo) graph
        , restrictions = renameVariables subst restrictions
        }

    where
      renameVariable v =
        Map.findWithDefault v v subst



instance Valid SymbolicGraph where

  validate =
    validate . graphPart



-- * Context and graph traversal
{- $contexts
See the discussion of contexts and graph traversal in module 'Graph.Graph'.
-}


-- | Provides access to a node's incident edges.
newtype NodeContext =
  NodeCtx { unNodeCtx :: Graph.NodeContext (Maybe Variable) () }


-- | Shorthand for having a node along with its context.
type NodeInContext =
  (Node, NodeContext)


-- | Shorthand for having an edge along with its source and target in context.
--
-- Because of lazyness, constructing a value of this type does __not__ evaluate the node lookup.
-- Thus, forcing the evaluation of the nodes costs /O(v)/. Keep this in mind when using values
-- of this type.
type EdgeInContext =
  (NodeInContext, Edge, NodeInContext)


-- | Get the edges that are incident on the current node.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incidentEdges :: NodeContext -> [EdgeInContext]
incidentEdges =
  map wrapEdgeInContext . Graph.incidentEdges . unNodeCtx


-- | Get the edges that have the current node as target.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incomingEdges :: NodeContext -> [EdgeInContext]
incomingEdges =
  map wrapEdgeInContext . Graph.incomingEdges . unNodeCtx


-- | Get the edges that have the current node as source.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
outgoingEdges :: NodeContext -> [EdgeInContext]
outgoingEdges =
  map wrapEdgeInContext . Graph.outgoingEdges . unNodeCtx



wrapNodeInContext :: Graph.NodeInContext (Maybe Variable) () -> NodeInContext
wrapNodeInContext (n, ctx) =
  (Node n, NodeCtx ctx)


wrapEdgeInContext :: Graph.EdgeInContext (Maybe Variable) () -> EdgeInContext
wrapEdgeInContext (src, e, tgt) =
  (wrapNodeInContext src, Edge e, wrapNodeInContext tgt)



-- * Query


-- | True if the given graph has no nodes, edges or restrictions. /O(1)/.
isEmpty :: SymbolicGraph -> Bool
isEmpty (SymbGraph graph restrictions) =
  Graph.null graph && Prelude.null restrictions


-- | True if there is a node with given identifier in the given graph. /O(v)/.
isNodeOf :: NodeId -> SymbolicGraph -> Bool
isNodeOf nodeId graph =
  Graph.isNodeOf (graphPart graph) nodeId


-- | True if there is an edge with given identifier in the given graph. /O(e)/.
isEdgeOf :: EdgeId -> SymbolicGraph -> Bool
isEdgeOf edgeId graph =
  Graph.isEdgeOf (graphPart graph) edgeId


-- | Look up the node with given identifier in the graph. /O(v)/.
lookupNode :: NodeId -> SymbolicGraph -> Maybe Node
lookupNode nodeId graph =
  Node <$> Graph.lookupNode nodeId (graphPart graph)


-- | Look up the node with given identifier, along with its context, in the graph. /O(v)/.
lookupNodeInContext :: NodeId -> SymbolicGraph -> Maybe NodeInContext
lookupNodeInContext nodeId graph =
  wrapNodeInContext <$> Graph.lookupNodeInContext nodeId (graphPart graph)


-- | Look up the edge with given identifier in the graph. /O(e)/.
lookupEdge :: EdgeId -> SymbolicGraph -> Maybe Edge
lookupEdge edgeId graph =
  Edge <$> Graph.lookupEdge edgeId (graphPart graph)


-- | Look up the edge with given identifier, along with its context, in the graph.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
lookupEdgeInContext :: EdgeId -> SymbolicGraph -> Maybe EdgeInContext
lookupEdgeInContext edgeId graph =
  wrapEdgeInContext <$> Graph.lookupEdgeInContext edgeId (graphPart graph)



-- * Construction


-- | A symbolic graph with no nodes, edges or restrictions.
empty :: SymbolicGraph
empty =
  SymbGraph Graph.empty []


-- | Build a graph from lists of nodes, edges and restrictions. Edges with undefined source or
-- target are ignored and omitted from the resulting graph. /O(v + e*v)/.
--
-- /Note:/ the restrictions of the resulting graph may be unsatisfiable.
fromNodesAndEdges :: [Node] -> [Edge] -> [Restriction] -> SymbolicGraph
fromNodesAndEdges nodes edges restrictions =
  let
    graph =
      Graph.fromNodesAndEdges (map unNode nodes) (map unEdge edges)
  in
    SymbGraph graph restrictions



-- * Insertion


-- | Insert a node into the graph. If a node with the given identifier aready exists, its attribute
-- is updated. /O(v)/.
insertNode :: Node -> SymbolicGraph -> SymbolicGraph
insertNode n =
  modifyGraphPart $ Graph.insertNodeWithPayload (nodeId n) (nodeAttribute n)


-- | Insert an edge into the graph. If an edge with the given identifier aready exists, its source
-- and target are updated. /O(v)/.
insertEdge :: Edge -> SymbolicGraph -> SymbolicGraph
insertEdge e =
  modifyGraphPart $ Graph.insertEdgeWithPayload (edgeId e) (sourceId e) (targetId e) ()



-- * Delete


-- | Removes the given node from the graph, unless it has any incident edges. /O(v + e²)/.
deleteNode :: NodeId -> SymbolicGraph -> SymbolicGraph
deleteNode n =
  modifyGraphPart (Graph.removeNode n)


-- | Remove the given edge from the graph. /O(e)/.
deleteEdge :: EdgeId -> SymbolicGraph -> SymbolicGraph
deleteEdge e =
  modifyGraphPart (Graph.removeEdge e)



-- * Update


-- | Update the node's attribute, applying the given function on it. /O(v)/.
updateNodeAttribute :: (Maybe Variable -> Maybe Variable) -> NodeId -> SymbolicGraph -> SymbolicGraph
updateNodeAttribute f nodeId =
  modifyGraphPart (Graph.updateNodePayload f nodeId)


-- | Update the attributes of all nodes. /O(v)/.
updateNodeAttributes :: (Node -> Maybe Variable) -> SymbolicGraph -> SymbolicGraph
updateNodeAttributes f =
  modifyGraphPart $ Graph.mapNodes (f . Node)



-- * Conversion
-- ** Lists


-- | List of all nodes from the graph. /O(v)/.
nodes :: SymbolicGraph -> [Node]
nodes =
  map Node . Graph.nodes . graphPart


-- | List of all edges from the graph. /O(e)/.
edges :: SymbolicGraph -> [Edge]
edges =
  map Edge . Graph.edges . graphPart


-- | List of all node id's from from the graph. /O(v)/.
nodeIds :: SymbolicGraph -> [NodeId]
nodeIds =
  Graph.nodeIds . graphPart


-- | List of all edge id's from from the graph. /O(e)/.
edgeIds :: SymbolicGraph -> [EdgeId]
edgeIds =
  Graph.edgeIds . graphPart


-- | List of all nodes from the graph, along with their contexts. /O(v)/.
nodesInContext :: SymbolicGraph -> [NodeInContext]
nodesInContext =
  map wrapNodeInContext . Graph.nodesInContext . graphPart


-- | List of all edges from the graph, along with their contexts.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
edgesInContext :: SymbolicGraph -> [EdgeInContext]
edgesInContext =
  map wrapEdgeInContext . Graph.edgesInContext . graphPart
