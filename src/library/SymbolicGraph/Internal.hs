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
  , NodeInContext
  , EdgeInContext
  , NodeContext
  , incidentEdges
  , incomingEdges
  , outgoingEdges

  -- * Query
  , SymbolicGraph.Internal.null
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

import           SymbolicGraph.DataAlgebra

import           Abstract.Valid
import           Graph.Graph               (EdgeId, Graph, NodeId)
import qualified Graph.Graph               as Graph

import qualified Data.Map                  as Map
import qualified Data.Set                  as Set



type InnerGraph =
  Graph (Maybe Variable) ()


data SymbolicGraph =
  SymbGraph
    { graphPart    :: InnerGraph
    , restrictions :: [Restriction]
    }
  deriving Eq


modifyGraphPart :: (InnerGraph -> InnerGraph) -> SymbolicGraph -> SymbolicGraph
{-# INLINE modifyGraphPart #-}
modifyGraphPart f graph =
  graph { graphPart = f (graphPart graph) }


newtype Node =
  Node { unNode :: Graph.Node (Maybe Variable) }


node :: NodeId -> Maybe Variable -> Node
node n v =
  Node (Graph.Node n v)


nodeId :: Node -> NodeId
nodeId =
  Graph.nodeId . unNode


nodeAttribute :: Node -> Maybe Variable
nodeAttribute =
  Graph.nodeInfo . unNode


newtype Edge =
  Edge { unEdge :: Graph.Edge () }


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


type NodeInContext =
  (Node, NodeContext)


type EdgeInContext =
  (NodeInContext, Edge, NodeInContext)


newtype NodeContext =
  NodeCtx { unNodeCtx :: Graph.NodeContext (Maybe Variable) () }


incidentEdges :: NodeContext -> [EdgeInContext]
incidentEdges =
  map wrapEdgeInContext . Graph.incidentEdges . unNodeCtx


incomingEdges :: NodeContext -> [EdgeInContext]
incomingEdges =
  map wrapEdgeInContext . Graph.incomingEdges . unNodeCtx


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


null :: SymbolicGraph -> Bool
null (SymbGraph graph restrictions) =
  Graph.null graph && Set.null (freeVariablesOf restrictions)


isNodeOf :: NodeId -> SymbolicGraph -> Bool
isNodeOf nodeId graph =
  Graph.isNodeOf (graphPart graph) nodeId


isEdgeOf :: EdgeId -> SymbolicGraph -> Bool
isEdgeOf edgeId graph =
  Graph.isEdgeOf (graphPart graph) edgeId


lookupNode :: NodeId -> SymbolicGraph -> Maybe Node
lookupNode nodeId graph =
  Node <$> Graph.lookupNode nodeId (graphPart graph)


lookupNodeInContext :: NodeId -> SymbolicGraph -> Maybe NodeInContext
lookupNodeInContext nodeId graph =
  wrapNodeInContext <$> Graph.lookupNodeInContext nodeId (graphPart graph)


lookupEdge :: EdgeId -> SymbolicGraph -> Maybe Edge
lookupEdge edgeId graph =
  Edge <$> Graph.lookupEdge edgeId (graphPart graph)


lookupEdgeInContext :: EdgeId -> SymbolicGraph -> Maybe EdgeInContext
lookupEdgeInContext edgeId graph =
  wrapEdgeInContext <$> Graph.lookupEdgeInContext edgeId (graphPart graph)



-- * Construction


empty :: SymbolicGraph
empty =
  SymbGraph Graph.empty []


fromNodesAndEdges :: [Node] -> [Edge] -> [Restriction] -> SymbolicGraph
fromNodesAndEdges nodes edges restrictions =
  let
    graph =
      Graph.fromNodesAndEdges (map unNode nodes) (map unEdge edges)
  in
    SymbGraph graph restrictions



-- * Insertion


insertNode :: Node -> SymbolicGraph -> SymbolicGraph
insertNode n =
  modifyGraphPart $ Graph.insertNodeWithPayload (nodeId n) (nodeAttribute n)


insertEdge :: Edge -> SymbolicGraph -> SymbolicGraph
insertEdge e =
  modifyGraphPart $ Graph.insertEdgeWithPayload (edgeId e) (sourceId e) (targetId e) ()



-- * Delete


deleteNode :: NodeId -> SymbolicGraph -> SymbolicGraph
deleteNode n =
  modifyGraphPart (Graph.removeNode n)


deleteEdge :: EdgeId -> SymbolicGraph -> SymbolicGraph
deleteEdge e =
  modifyGraphPart (Graph.removeEdge e)



-- * Update


updateNodeAttribute :: (Maybe Variable -> Maybe Variable) -> NodeId -> SymbolicGraph -> SymbolicGraph
updateNodeAttribute f nodeId =
  modifyGraphPart (Graph.updateNodePayload f nodeId)



-- * Conversion
-- ** Lists


nodes :: SymbolicGraph -> [Node]
nodes =
  map Node . Graph.nodes . graphPart


edges :: SymbolicGraph -> [Edge]
edges =
  map Edge . Graph.edges . graphPart


nodeIds :: SymbolicGraph -> [NodeId]
nodeIds =
  Graph.nodeIds . graphPart


edgeIds :: SymbolicGraph -> [EdgeId]
edgeIds =
  Graph.edgeIds . graphPart


nodesInContext :: SymbolicGraph -> [NodeInContext]
nodesInContext =
  map wrapNodeInContext . Graph.nodesInContext . graphPart


edgesInContext :: SymbolicGraph -> [EdgeInContext]
edgesInContext =
  map wrapEdgeInContext . Graph.edgesInContext . graphPart



-- * Map


mapNodeAttributes :: (Node -> Maybe Variable) -> SymbolicGraph -> SymbolicGraph
mapNodeAttributes f =
  modifyGraphPart $ Graph.mapNodes (f . Node)
