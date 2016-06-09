module Graph.Subgraph (induzedSubgraphs) where

import           Abstract.Morphism
import           Graph.Graph
import           Graph.GraphMorphism
import           Graph.TypedGraphMorphism

-- | Considering /m : X -> Y/,
-- generates all subgraphs of /Y/ containing the graph /X/ via m.
induzedSubgraphs :: TypedGraphMorphism a b -> [TypedGraphMorphism a b]
induzedSubgraphs m = map (idMap (domain m)) subEdges
  where
    g = codomain m
    graph = domain g
    
    listNodesToAdd = [(n, applyNodeUnsafe g n) | n <- orphanNodesTyped m]
    
    subNodes = decisionTreeNodes (domain m) listNodesToAdd
    
    listEdgesToAdd = [(e,
                       sourceOfUnsafe graph e,
                       targetOfUnsafe graph e,
                       applyEdgeUnsafe g e)
                       | e <- orphanEdgesTyped m]
    
    subEdges =
      concatMap
        (\g -> decisionTreeEdges g listEdgesToAdd)
        subNodes

decisionTreeNodes :: TypedGraph a b -> [(NodeId,NodeId)] -> [TypedGraph a b]
decisionTreeNodes g [] = [g]
decisionTreeNodes g ((n,tp):ns) = (decisionTreeNodes g ns) ++ (decisionTreeNodes added ns)
  where
    added = createNodeDom n tp g

decisionTreeEdges :: TypedGraph a b -> [(EdgeId,NodeId,NodeId,EdgeId)] -> [TypedGraph a b]
decisionTreeEdges g [] = [g]
decisionTreeEdges g ((e,s,t,tp):es) = (decisionTreeEdges g es) ++
  if isNodeOf graph s && isNodeOf graph t
    then (decisionTreeEdges added es) else []
  where
    graph = domain g
    added = createEdgeDom e s t tp g
