module TypedGraph.Partitions.GraphPartitionToVerigraph (
   mountTypedGraphMorphisms
   ) where

{-Converts from GraphPartition to Verigraph structures-}

import           Abstract.Morphism
import qualified Graph.Graph                          as G
import qualified Graph.GraphMorphism                  as GM
import           TypedGraph.Graph
import qualified TypedGraph.MorphismCore              as TGM
import           TypedGraph.Partitions.GraphPartition as GP

-- | For two typed graphs and a EpiPair (in GraphPartition format) return two TypedGraphMorphism for the graph in verigraph format
mountTypedGraphMorphisms :: TypedGraph a b -> TypedGraph a b -> GP.GraphPartition -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
mountTypedGraphMorphisms tg1 tg2 graphPartition = (mountTGM True tg1, mountTGM False tg2)
  where
    typeGraph = codomain tg1
    typedGraph = mountTypedGraph graphPartition typeGraph
    mountTGM side match = TGM.typedMorphism match typedGraph (mountMapping side graphPartition match)

mountGraph :: GP.GraphPartition -> G.Graph a b
mountGraph (nodes,edges) = G.build nods edgs
  where
    nods = map (\(n:_) -> GP.nodeId n) nodes
    edgs = map (\(e:_) -> (GP.edgeId e, nodeSrc e, nodeTgt e)) edges
    nodeSrc e = GP.nodeId $ GP.getNode (nodeNameAndSource (GP.source e)) nodes
    nodeTgt e = GP.nodeId $ GP.getNode (nodeNameAndSource (GP.target e)) nodes
    nodeNameAndSource node = (nodeName node, nodeFromLeft node)

mountTypedGraph :: GP.GraphPartition -> G.Graph a b -> TypedGraph a b
mountTypedGraph graphPartition typeGraph = GM.buildGraphMorphism graph typeGraph nodes edges
  where
    nodes = map (\(n:_) -> (GP.nodeId n, GP.nodeType n)) (fst graphPartition)
    edges = map (\(e:_) -> (GP.edgeId e, GP.edgeType e)) (snd graphPartition)
    graph = mountGraph graphPartition

mountMapping :: Bool -> GP.GraphPartition -> GM.GraphMorphism a b -> GM.GraphMorphism a b
mountMapping side g@(nodes,edges) m = GM.buildGraphMorphism (domain m) (mountGraph g) nods edgs
  where
    nods = map (\(G.NodeId n) -> (n, nodeId n)) (G.nodes (domain m))
    nodeId n = GP.nodeId $ head $ getListNodeName (side,n) nodes
    edgs = map (\(G.EdgeId e) -> (e, edgeId e)) (G.edges (domain m))
    edgeId e = GP.edgeId $ head $ getListEdgeName (side,e) edges

-- | Returns the list which Node is in [[Node]]
getListNodeName :: (Bool,Int) -> [[Node]] -> [Node]
getListNodeName p@(side,a) (x:xs) = if any (\(Node _ name _ _ src) -> name == a && src == side) x then x else getListNodeName p xs
getListNodeName _ [] = error "error when mounting overlapping pairs (getListNodeName)" -- There must be at least one equivalence class of nodes


-- | Returns the list which Edge is in [[Edge]]
getListEdgeName :: (Bool,Int) -> [[Edge]] -> [Edge]
getListEdgeName p@(side,a) (x:xs) = if any (\e -> (label e == a) && (edgeFromLeft e == side)) x then x else getListEdgeName p xs
getListEdgeName _ [] = error "error when mounting overlapping pairs (getListNodeName)" -- There must be at least one equivalence class of edges
