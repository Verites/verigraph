module Data.TypedGraph.Subgraph (subgraphs, inducedSubgraphs) where

import           Data.Graphs
import           Data.Graphs.Morphism
import           Data.TypedGraph
import           Data.TypedGraph.Morphism as TGM hiding (createEdgeOnDomain, createNodeOnDomain)

-- | Generates all subgraphs of a typed graph.
subgraphs :: TypedGraph a b -> [TypedGraph a b]
subgraphs g = subEdges
  where
    graph = untypedGraph g
    emptyGraph = Data.Graphs.Morphism.empty Data.Graphs.empty (typeGraph g)

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
inducedSubgraphs m = map (idMap (TGM.domainGraph m)) subEdges
  where
    g = TGM.codomainGraph m

    listNodesToAdd = [(n, extractNodeType g n) | n <- orphanTypedNodeIds m]

    subNodes = decisionTreeNodes listNodesToAdd (TGM.domainGraph m)


    listEdgesToAdd = [(edgeId e,
                       sourceId e,
                       targetId e,
                       extractEdgeType g (edgeId e))
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
    graph = untypedGraph g
    added = createEdgeOnDomain e s t tp g
