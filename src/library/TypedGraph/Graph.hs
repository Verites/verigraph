module TypedGraph.Graph
  ( TypedGraph
  , untypedGraph
  , typeGraph

  , TypedGraph.Graph.null

  , newNodesTyped
  , newEdgesTyped
  ) where

import Abstract.Morphism
import Graph.Graph as G
import Graph.GraphMorphism

-- | A typed graph is a morphism whose codomain is the type graph.
type TypedGraph a b = GraphMorphism a b

-- | Obtain the untyped version of the typed graph
untypedGraph :: TypedGraph a b -> Graph a b
untypedGraph = domain

-- | Obtain the type graph from a typed graph
typeGraph :: TypedGraph a b -> Graph a b
typeGraph = codomain

-- | Test if the typed graph is empty
null :: TypedGraph a b -> Bool
null = G.null . untypedGraph

-- | Infinite list of new node instances of a typed graph
newNodesTyped :: TypedGraph a b -> [NodeId]
newNodesTyped tg = newNodes $ untypedGraph tg

-- | Infinite list of new edge instances of a typed graph
newEdgesTyped :: TypedGraph a b -> [EdgeId]
newEdgesTyped tg = newEdges $ untypedGraph tg
