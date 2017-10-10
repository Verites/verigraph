module Data.TypedGraph
  ( TypedGraph
  , untypedGraph
  , extractNodeType
  , extractEdgeType
  , typeGraph
  , Data.TypedGraph.null
  , fromNodesAndEdges
  , newTypedNodes
  , newTypedEdges
  , typedNodes
  , typedEdges
  , untypedNodes
  , untypedEdges
  ) where

import           Base.Cardinality
import           Data.Graphs          hiding (fromNodesAndEdges)
import qualified Data.Graphs          as G
import           Data.Graphs.Morphism
import           Data.Maybe           (fromMaybe)


-- | A typed graph is a morphism whose codomain is the type graph.
type TypedGraph a b = GraphMorphism (Maybe a) (Maybe b)

instance Cardinality (GraphMorphism a b) where
  cardinality = cardinality . domainGraph

-- | Obtain the untyped version of the typed graph
untypedGraph :: TypedGraph a b -> Graph (Maybe a) (Maybe b)
untypedGraph = domainGraph

-- | Obtain the type graph from a typed graph
typeGraph :: TypedGraph a b -> Graph (Maybe a) (Maybe b)
typeGraph = codomainGraph

-- | Test if the typed graph is empty
null :: TypedGraph a b -> Bool
null = G.null . untypedGraph

extractNodeType :: TypedGraph a b -> NodeId -> NodeId
extractNodeType gm n = fromMaybe (error "Node not typed") $ applyNodeId gm n

extractEdgeType :: TypedGraph a b -> EdgeId -> EdgeId
extractEdgeType gm e = fromMaybe (error "edge not typed") $ applyEdgeId gm e

-- | Infinite list of new node instances of a typed graph
newTypedNodes :: TypedGraph a b -> [NodeId]
newTypedNodes tg = newNodes $ untypedGraph tg

-- | Infinite list of new edge instances of a typed graph
newTypedEdges :: TypedGraph a b -> [EdgeId]
newTypedEdges tg = newEdges $ untypedGraph tg

-- | Obtain a list of tuples @(nodeId, typeId)@ for nodes in the graph.
typedNodes :: TypedGraph a b -> [(NodeId, NodeId)]
typedNodes tg = map withType $ nodeIds (untypedGraph tg)
  where withType node = (node, extractNodeType tg node)

-- | Obtain a list of tuples @(edgeId, srcId, tgtId, typeId)@ for edges in the graph.
typedEdges :: TypedGraph a b -> [(EdgeId, NodeId, NodeId, EdgeId)]
typedEdges tg = map withType $ edges graph
  where graph = untypedGraph tg
        withType edge = (edgeId edge, sourceId edge, targetId edge, extractEdgeType tg (edgeId edge))

-- | Obtain the list of untyped nodes, i.e., the list of node ids from the typed graph domain
untypedNodes :: TypedGraph a b -> [NodeId]
untypedNodes tg = nodeIds $ untypedGraph tg

-- | Obtain the list of untyped edges, i.e., the list of edge ids from the typed graph domain
untypedEdges :: TypedGraph a b -> [EdgeId]
untypedEdges tg = edgeIds $ untypedGraph tg

-- | Build a graph from lists of nodes and edges. Edges with undefined source or target are ignored
-- and omitted from the resulting graph. Elements with undefined types are also ignored. /O(v + e*v)/
fromNodesAndEdges :: Graph (Maybe n) (Maybe e) -> [(Node (Maybe n), NodeId)] -> [(Edge (Maybe e), EdgeId)] -> TypedGraph n e
fromNodesAndEdges typeGraph nodes edges =
  let
    untypedGraph = G.fromNodesAndEdges (map fst nodes) (map fst edges)
  in fromGraphsAndLists untypedGraph typeGraph
    [ (nodeId n, nt) | (n, nt) <- nodes, G.isNodeOf typeGraph nt ]
    [ (edgeId e, et) | (e, et) <- edges, G.isEdgeOf typeGraph et ]