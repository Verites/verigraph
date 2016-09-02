module TypedGraph.Partitions.GPToVeri (
   mountTypedGraphMorphisms
   ) where

{-Converts from GraphPart to Verigraph structures-}

import           Abstract.Morphism
import qualified Graph.Graph                     as G
import qualified Graph.GraphMorphism             as GM
import           TypedGraph.Graph
import qualified TypedGraph.MorphismCore         as TGM
import           TypedGraph.Partitions.GraphPart as GP

-- | For two typed graphs and a EpiPair (in GraphPart format) return two TypedGraphMorphism for the graph in verigraph format
mountTypedGraphMorphisms :: TypedGraph a b -> TypedGraph a b -> GP.EqClassGraph -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
mountTypedGraphMorphisms tg1 tg2 graphPartition = (mountTGM True tg1, mountTGM False tg2)
  where
    typeGraph = codomain tg1
    typedGraph = mountTypedGraph graphPartition typeGraph
    mountTGM side match = TGM.typedMorphism match typedGraph (mountMapping side graphPartition match)

mountGraph :: GP.EqClassGraph -> G.Graph a b
mountGraph (nodes,edges) = G.build nods edgs
  where
    nods = map (\(n:_) -> GP.nid n) nodes
    edgs = map (\(e:_) -> (GP.eid e, nodeSrc e, nodeTgt e)) edges
    nodeSrc e = GP.nid $ GP.getNode (nameAndSrc (GP.source e)) nodes
    nodeTgt e = GP.nid $ GP.getNode (nameAndSrc (GP.target e)) nodes
    nameAndSrc node = (nname node, inLeftn node)

mountTypedGraph :: GP.EqClassGraph -> G.Graph a b -> TypedGraph a b
mountTypedGraph graphPartition typeGraph = GM.buildGraphMorphism graph typeGraph nodes edges
  where
    nodes = map (\(n:_) -> (GP.nid n, GP.ntype n)) (fst graphPartition)
    edges = map (\(e:_) -> (GP.eid e, GP.etype e)) (snd graphPartition)
    graph = mountGraph graphPartition

mountMapping :: Bool -> GP.EqClassGraph -> GM.GraphMorphism a b -> GM.GraphMorphism a b
mountMapping side g@(nodes,edges) m = GM.buildGraphMorphism (domain m) (mountGraph g) nods edgs
  where
    nods = map (\(G.NodeId n) -> (n, nodeId n)) (G.nodes (domain m))
    nodeId n = GP.nid $ head $ getListNodeName (side,n) nodes
    edgs = map (\(G.EdgeId e) -> (e, edgeId e)) (G.edges (domain m))
    edgeId e = GP.eid $ head $ getListEdgeName (side,e) edges

-- | Returns the list which Node is in [[Node]]
getListNodeName :: (Bool,Int) -> [[Node]] -> [Node]
getListNodeName p@(side,a) (x:xs) = if any (\(Node _ name _ _ src) -> name == a && src == side) x then x else getListNodeName p xs
getListNodeName _ [] = error "error when mounting overlapping pairs (getListNodeName)" -- There must be at least one equivalence class of nodes


-- | Returns the list which Edge is in [[Edge]]
getListEdgeName :: (Bool,Int) -> [[Edge]] -> [Edge]
getListEdgeName p@(side,a) (x:xs) = if any (\e -> (label e == a) && (inLefte e == side)) x then x else getListEdgeName p xs
getListEdgeName _ [] = error "error when mounting overlapping pairs (getListNodeName)" -- There must be at least one equivalence class of edges
