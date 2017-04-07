module TypedGraph.Graph
  ( TypedGraph
  , untypedGraph
  , extractNodeType
  , extractEdgeType
  , typeGraph
  , TypedGraph.Graph.null
  , newTypedNodes
  , newTypedEdges
  , nodesWithType
  , edgesWithType
  ) where

import           Abstract.Cardinality
import           Abstract.Morphism
import           Data.Maybe           (fromMaybe)
import           Graph.Graph          as G
import           Graph.GraphMorphism


-- | A typed graph is a morphism whose codomain is the type graph.
type TypedGraph a b = GraphMorphism (Maybe a) (Maybe b)

instance Cardinality (GraphMorphism a b) where
  cardinality = cardinality . domain

-- | Obtain the untyped version of the typed graph
untypedGraph :: TypedGraph a b -> Graph (Maybe a) (Maybe b)
untypedGraph = domain

-- | Obtain the type graph from a typed graph
typeGraph :: TypedGraph a b -> Graph (Maybe a) (Maybe b)
typeGraph = codomain

-- | Test if the typed graph is empty
null :: TypedGraph a b -> Bool
null = G.null . untypedGraph

extractNodeType :: TypedGraph a b -> NodeId -> NodeId
extractNodeType gm n = fromMaybe (error "Node not typed") $ applyNode gm n

extractEdgeType :: TypedGraph a b -> EdgeId -> EdgeId
extractEdgeType gm e = fromMaybe (error "edge not typed") $ applyEdge gm e

-- | Infinite list of new node instances of a typed graph
newTypedNodes :: TypedGraph a b -> [NodeId]
newTypedNodes tg = newNodes $ untypedGraph tg

-- | Infinite list of new edge instances of a typed graph
newTypedEdges :: TypedGraph a b -> [EdgeId]
newTypedEdges tg = newEdges $ untypedGraph tg

-- | Obtain a list of tuples @(nodeId, typeId)@ for nodes in the graph.
nodesWithType :: TypedGraph a b -> [(NodeId, NodeId)]
nodesWithType tg = map withType $ nodeIds (untypedGraph tg)
  where withType node = (node, extractNodeType tg node)

-- | Obtain a list of tuples @(edgeId, srcId, tgtId, typeId)@ for edges in the graph.
edgesWithType :: TypedGraph a b -> [(EdgeId, NodeId, NodeId, EdgeId)]
edgesWithType tg = map withType $ edgeIds graph
  where graph = untypedGraph tg
        withType edge = (edge, sourceOfUnsafe graph edge, targetOfUnsafe graph edge, extractEdgeType tg edge)
