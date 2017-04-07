module TypedGraph.Subgraph (subgraphs, inducedSubgraphs) where

import           Abstract.Morphism
import           Graph.Graph
import           Graph.GraphMorphism
import           TypedGraph.Graph
import           TypedGraph.Morphism hiding (createEdgeOnDomain,
                                      createNodeOnDomain)

-- | Generates all subgraphs of a typed graph.
subgraphs :: TypedGraph a b -> [TypedGraph a b]
subgraphs g = subEdges
  where
    graph = domain g
    emptyGraph = Graph.GraphMorphism.empty Graph.Graph.empty (codomain g)

    listNodesToAdd = [(n, extractNodeType g n) | n <- nodeIds graph]

    subNodes = decisionTreeNodes listNodesToAdd emptyGraph

    listEdgesToAdd =
      [ (e, srcId, tgtId, extractEdgeType g e)
          | (Edge e srcId tgtId _) <- edges graph
      ]

    subEdges = concatMap (decisionTreeEdges listEdgesToAdd) subNodes

-- | Considering /m : X -> Y/,
-- generates all subgraphs of /Y/ containing the graph /X/ via m.
inducedSubgraphs :: TypedGraphMorphism a b -> [TypedGraphMorphism a b]
inducedSubgraphs m = map (idMap (domain m)) subEdges
  where
    g = codomain m
    graph = domain g

    listNodesToAdd = [(n, extractNodeType g n) | n <- orphanTypedNodes m]

    subNodes = decisionTreeNodes listNodesToAdd (domain m)

    listEdgesToAdd = [(e,
                       sourceOfUnsafe graph e,
                       targetOfUnsafe graph e,
                       extractEdgeType g e)
                       | e <- orphanTypedEdges m]

    subEdges = concatMap (decisionTreeEdges listEdgesToAdd) subNodes

decisionTreeNodes :: [(NodeId,NodeId)] -> TypedGraph a b -> [TypedGraph a b]
decisionTreeNodes [] g = [g]
decisionTreeNodes ((n,tp):ns) g = decisionTreeNodes ns g ++ decisionTreeNodes ns added
  where
    added = createNodeOnDomain n tp g

decisionTreeEdges :: [(EdgeId,NodeId,NodeId,EdgeId)] -> TypedGraph a b -> [TypedGraph a b]
decisionTreeEdges [] g = [g]
decisionTreeEdges ((e,s,t,tp):es) g = decisionTreeEdges es g ++
  if isNodeOf graph s && isNodeOf graph t
    then decisionTreeEdges es added else []
  where
    graph = domain g
    added = createEdgeOnDomain e s t tp g
