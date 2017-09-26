module Data.TypedGraph.Subgraph (subgraphs, inducedSubgraphs) where

import qualified Data.Graphs as G
import qualified Data.Graphs.Morphism as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism as TGM hiding (createEdgeOnDomain, createNodeOnDomain)

-- | Generates all subgraphs of a typed graph.
subgraphs :: TypedGraph a b -> [TypedGraph a b]
subgraphs g = subEdges
  where
    emptyGraph = GM.empty G.empty (typeGraph g)

    listNodesToAdd = [(n, nt) | (Node n _, nt) <- nodes g]
    subNodes = decisionTreeNodes listNodesToAdd emptyGraph

    listEdgesToAdd = [ (e, srcId, tgtId, et) | (Edge e srcId tgtId _, et) <- edges g ]
    subEdges = concatMap (decisionTreeEdges listEdgesToAdd) subNodes

-- | Considering /m : X -> Y/,
-- generates all subgraphs of /Y/ containing the graph /X/ via m.
inducedSubgraphs :: TypedGraphMorphism a b -> [TypedGraphMorphism a b]
inducedSubgraphs m = map (makeInclusion (TGM.domainGraph m)) subEdges
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
    added = GM.createNodeOnDomain n tp g

decisionTreeEdges :: [(EdgeId,NodeId,NodeId,EdgeId)] -> TypedGraph a b -> [TypedGraph a b]
decisionTreeEdges [] g = [g]
decisionTreeEdges ((e,s,t,tp):es) g = decisionTreeEdges es g ++
  if isNodeOf g s && isNodeOf g t
    then decisionTreeEdges es added else []
  where
    added = GM.createEdgeOnDomain e s t tp g
