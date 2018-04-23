module Data.TypedGraph (
  -- * Graph Type
    TypedGraph
  , NodeId(..)
  , EdgeId(..)
  , Node(..)
  , Edge(..)

  -- * Contexts and Graph Traversal
  , NodeContext
  , NodeInContext
  , EdgeInContext

  , incidentEdges
  , incomingEdges
  , outgoingEdges

  -- ** Neighbour nodes
  -- $neighbours

  -- * Query
  , Data.TypedGraph.null
  , typeGraph
  , isNodeOf
  , isEdgeOf
  , lookupNode
  , lookupNodeInContext
  , lookupEdge
  , lookupEdgeInContext
  , isAdjacentTo
  , isIncidentTo
  , extractNodeType
  , extractEdgeType

  -- * Construction
  , empty
  , fromNodesAndEdges

  -- ** Delete
  , removeIsolatedNode
  , removeNodeAndIncidentEdges
  , removeEdge

  -- * Conversion
  -- ** Untyped Graphs and Morphisms
  , toGraphMorphism
  , fromGraphMorphism
  , toUntypedGraph

  -- ** Lists
  , nodes
  , edges
  , nodeIds
  , edgeIds
  , typedNodeIds
  , typedEdgeIds
  , nodesInContext
  , edgesInContext
  , newNodes
  , newEdges
  ) where


import qualified Data.List            as List
import           Data.Maybe           (fromMaybe, isJust)

import           Base.Cardinality
import           Data.Graphs          (Edge (..), EdgeId, Graph, Node (..), NodeId)
import qualified Data.Graphs          as Graph
import           Data.Graphs.Morphism
import qualified Data.Relation        as Relation

-- | A typed graph is a morphism whose codomain is the type graph.
type TypedGraph n e = GraphMorphism (Maybe n) (Maybe e)

-- | Reinterpret a typed graph as a graph morphism.
toGraphMorphism :: TypedGraph n e -> GraphMorphism (Maybe n) (Maybe e)
toGraphMorphism = id

-- | Reinterpret a graph morphism as a typed graph.
fromGraphMorphism :: GraphMorphism (Maybe n) (Maybe e) -> TypedGraph n e
fromGraphMorphism = id

-- | Obtain the untyped version of the typed graph
toUntypedGraph :: TypedGraph n e -> Graph (Maybe n) (Maybe e)
toUntypedGraph = domainGraph

-- | Obtain the type graph from a typed graph
typeGraph :: TypedGraph n e -> Graph (Maybe n) (Maybe e)
typeGraph = codomainGraph

-- | Build a graph from lists of nodes and edges. Edges with undefined source or target are ignored
-- and omitted from the resulting graph. Elements with undefined types are also ignored. /O(v + e*v)/
fromNodesAndEdges :: Graph (Maybe n) (Maybe e) -> [(Node (Maybe n), NodeId)] -> [(Edge (Maybe e), EdgeId)] -> TypedGraph n e
fromNodesAndEdges typeGraph nodes edges =
  let
    untypedGraph = Graph.fromNodesAndEdges (map fst nodes) (map fst edges)
  in fromGraphMorphism $ fromGraphsAndLists untypedGraph typeGraph
    [ (nodeId n, nt) | (n, nt) <- nodes, Graph.isNodeOf typeGraph nt ]
    [ (edgeId e, et) | (e, et) <- edges, Graph.isEdgeOf typeGraph et ]

instance Cardinality (GraphMorphism n e) where
  cardinality = cardinality . domainGraph

nodeTypeOf :: Node (Maybe n) -> TypedGraph n e -> Node (Maybe n)
nodeTypeOf node graph = fromMaybe (error "nodeTypeOf: malformed graph") $ do
  typeId <- applyNodeId graph (nodeId node)
  Graph.lookupNode typeId (typeGraph graph)

edgeTypeOf :: Edge (Maybe e) -> TypedGraph n e -> Edge (Maybe e)
edgeTypeOf edge graph = fromMaybe (error "edgeTypeOf: malformed graph") $ do
  typeId <- applyEdgeId graph (edgeId edge)
  Graph.lookupEdge typeId (typeGraph graph)

-- * Contexts and graph traversal
{- $contexts
In order to traverse or navigate through the graph, 'NodeContext's are provided. The context of a
node provides access to its incident edges and neighbour nodes without explicitly looking them up
in the graph. Internally, nodes and edges are still being looked up, so keep in mind there's no
performance benefit. This module, however, can guarantee that this implicit lookup will work, so
there's no need to deal with `Maybe` values.

An example is presented in the documentation for 'Data.Graphs'.
-}

-- | Provides access to a node's incident edges.
data NodeContext n e =
  NodeCtx NodeId (TypedGraph n e)

-- | Shorthand for having a node along with its type and context.
type NodeInContext n e =
  (Node (Maybe n), Node (Maybe n), NodeContext n e)

-- | Shorthand for having an edge along with its type, as well as source and target in context.
--
-- Because of laziness, constructing a value of this type does __not__ evaluate the node lookup.
-- Thus, forcing the evaluation of the nodes costs /O(v)/. Keep this in mind when using values
-- of this type.
type EdgeInContext n e =
  (NodeInContext n e, Edge (Maybe e), Edge (Maybe e), NodeInContext n e)

nodeInContext :: TypedGraph n e -> Node (Maybe n) -> NodeInContext n e
nodeInContext graph node = (node, nodeTypeOf node graph, NodeCtx (nodeId node) graph)

edgeInContext :: TypedGraph n e -> Edge (Maybe e) -> EdgeInContext n e
edgeInContext graph edge =
  ( nodeInContext graph (getNode (sourceId edge))
  , edge, edgeTypeOf edge graph
  , nodeInContext graph (getNode (targetId edge))
  )
  where
    getNode id = fromMaybe (error "edgeInContext: malformed graph") $ lookup id nodes
    nodes = Graph.nodeMap (toUntypedGraph graph)

-- | Get the edges that are incident on the current node.
-- /O(e*log(e))/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incidentEdges :: NodeContext n e -> [EdgeInContext n e]
incidentEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> sourceId edge == nodeId || targetId edge == nodeId)
  . map snd
  $ Graph.edgeMap (toUntypedGraph graph)

-- | Get the edges that have the current node as target.
-- /O(e*log(e))/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incomingEdges :: NodeContext n e -> [EdgeInContext n e]
incomingEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> targetId edge == nodeId)
  . map snd
  $ Graph.edgeMap (toUntypedGraph graph)


-- | Get the edges that have the current node as source.
-- /O(e*log(e))/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
outgoingEdges :: NodeContext n e -> [EdgeInContext n e]
outgoingEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> sourceId edge == nodeId)
  . map snd
  $ Graph.edgeMap (toUntypedGraph graph)



{- $neighbours

In order to access the neighbour nodes of the edges, one may simply go through the edges, e.g.

>   map (\(_, _, target) -> target) . outgoingEdges
>   map (\(source, _, _) -> source) . incomingEdges

These operations were not added to the graph API because it is not yet clear which of them are commonly used, and which names would be appropriate. If you identify the need for such an operation, submit an issue on github.
-}

-- | Test if the typed graph is empty. /O(1)/.
null :: TypedGraph n e -> Bool
null = Graph.null . toUntypedGraph

-- | Test if a node identifier is contained in the graph. /O(v)/.
isNodeOf :: TypedGraph n e -> NodeId -> Bool
isNodeOf = Graph.isNodeOf . toUntypedGraph

-- | Test if an edge identifier is contained in the graph. /O(e)/.
isEdgeOf :: TypedGraph n e -> EdgeId -> Bool
isEdgeOf = Graph.isEdgeOf . toUntypedGraph

-- | Test if the given nodes are adjacent. /O(e²)/
isAdjacentTo :: TypedGraph n e -> NodeId -> NodeId -> Bool
isAdjacentTo = Graph.isAdjacentTo . toUntypedGraph

-- | Test if the given edge has given node as source or target. /O(e)/.
isIncidentTo :: TypedGraph n e -> NodeId -> EdgeId -> Bool
isIncidentTo = Graph.isIncidentTo . toUntypedGraph

-- | Look up the node with given identifier in the graph. /O(v)/.
lookupNode :: NodeId -> TypedGraph n e -> Maybe (Node (Maybe n))
lookupNode id = Graph.lookupNode id . toUntypedGraph

-- | Look up the edge with given identifier in the graph. /O(e)/.
lookupEdge :: EdgeId -> TypedGraph n e -> Maybe (Edge (Maybe e))
lookupEdge id = Graph.lookupEdge id . toUntypedGraph

-- | Look up the node with given identifier, along with its context, in the graph. /O(v*log(v))/.
lookupNodeInContext :: NodeId -> TypedGraph n e -> Maybe (NodeInContext n e)
lookupNodeInContext id graph = nodeInContext graph <$> lookupNode id graph

-- | Look up the edge with given identifier, along with its context, in the graph.
-- /O(e*log(e))/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
lookupEdgeInContext :: EdgeId -> TypedGraph n e -> Maybe (EdgeInContext n e)
lookupEdgeInContext id graph = edgeInContext graph <$> lookupEdge id graph

-- | List of all node id's from from the graph. /O(v)/.
nodeIds :: TypedGraph n e -> [NodeId]
nodeIds = Graph.nodeIds . toUntypedGraph

-- | List of all edge id's from from the graph. /O(e)/.
edgeIds :: TypedGraph n e -> [EdgeId]
edgeIds = Graph.edgeIds . toUntypedGraph

-- | Obtain a list of tuples @(nodeId, typeId)@ for nodes in the graph. /O(v*log(v))/.
typedNodeIds :: TypedGraph n e -> [(NodeId, NodeId)]
typedNodeIds graph = map withType . Graph.nodeIds $ toUntypedGraph graph
  where withType n = (n, applyNodeIdUnsafe graph n)

-- | Obtain a list of tuples @(edgeId, srcId, tgtId, typeId)@ for edges in the graph. /O(e*log(e))/.
typedEdgeIds :: TypedGraph n e -> [(EdgeId, EdgeId)]
typedEdgeIds graph = map withType . Graph.edgeIds $ toUntypedGraph graph
  where withType e = (e, applyEdgeIdUnsafe graph e)

-- | List of all nodes from the graph, along with their types. /O(v*log(v))/.
nodes :: TypedGraph n e -> [(Node (Maybe n), NodeId)]
nodes graph = map withType . Graph.nodes . toUntypedGraph $ graph
  where withType node = (node, nodeId (nodeTypeOf node graph))

-- | List of all edges from the graph, along with their types. /O(e*log(e))/.
edges :: TypedGraph n e -> [(Edge (Maybe e), EdgeId)]
edges graph = map withType . Graph.edges . toUntypedGraph $ graph
  where withType edge = (edge, edgeId (edgeTypeOf edge graph))

-- | List of all nodes from the graph, along with their contexts. /O(v*log(v))/.
nodesInContext :: TypedGraph n e -> [NodeInContext n e]
nodesInContext graph = map (nodeInContext graph) (Graph.nodes $ toUntypedGraph graph)

-- | List of all edges from the graph, along with their contexts.
-- /O(e*log(e))/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
edgesInContext :: TypedGraph n e -> [EdgeInContext n e]
edgesInContext graph = map (edgeInContext graph) (Graph.edges $ toUntypedGraph graph)


{-# DEPRECATED extractNodeType "This function performs unnecessary dictionary lookups. Try using lookupNodeInContext, typedNodeIds, nodes or nodesInContext instead." #-}
extractNodeType :: TypedGraph n e -> NodeId -> NodeId
extractNodeType gm n = fromMaybe (error "Node not typed") $ applyNodeId gm n

{-# DEPRECATED extractEdgeType "This function performs unnecessary dictionary lookups. Try using lookupEdgeInContext, typedEdgeIds, edges or edgesInContext instead." #-}
extractEdgeType :: TypedGraph n e -> EdgeId -> EdgeId
extractEdgeType gm e = fromMaybe (error "edge not typed") $ applyEdgeId gm e

-- | Infinite list of node IDs that are not in a typed graph
newNodes :: TypedGraph n e -> [NodeId]
newNodes = Graph.newNodes . toUntypedGraph

-- | Infinite list of edge IDs that are not in a typed graph
newEdges :: TypedGraph n e -> [EdgeId]
newEdges = Graph.newEdges . toUntypedGraph

-- | Removes the given node from the graph, unless it has any incident edges. /O(v + e²)/.
removeIsolatedNode :: NodeId -> TypedGraph n e -> TypedGraph n e
removeIsolatedNode nodeId g@(GraphMorphism dom cod nodeMap edgeMap) =
  case lookupNodeInContext nodeId g of
    Nothing -> g
    Just (_,_,nodeCtx)
      | List.null (incidentEdges nodeCtx) ->
        GraphMorphism (Graph.removeNodeForced nodeId dom) cod (Relation.removeFromDomain nodeId nodeMap) edgeMap
      | otherwise -> g

-- | Removes the given node and all incident edges from the graph. /O(v + e²)/
removeNodeAndIncidentEdges :: NodeId -> TypedGraph n e -> TypedGraph n e
removeNodeAndIncidentEdges nodeId g@(GraphMorphism dom cod nodeMap edgeMap) =
  case lookupNodeInContext nodeId g of
    Nothing -> g
    Just (_,_,nodeCtx) ->
      let
        dom' = Graph.removeNodeAndIncidentEdges nodeId dom
        notRemoved e = isJust (Graph.lookupEdge e dom')
      in GraphMorphism dom' cod
        (Relation.removeFromDomain nodeId nodeMap)
        (Relation.filterDomain notRemoved edgeMap)

-- | Remove the given edge from the graph. /O(e)/.
removeEdge :: EdgeId -> TypedGraph n e -> TypedGraph n e
removeEdge e g@(GraphMorphism dom cod nodeMap edgeMap) =
  GraphMorphism (Graph.removeEdge e dom) cod nodeMap (Relation.removeFromDomain e edgeMap)
