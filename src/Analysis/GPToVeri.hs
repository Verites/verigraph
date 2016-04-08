module Analysis.GPToVeri {-(
   setType,
   mountTGM,
   mountTGMBoth,
   toMorphism
   ) -}where

{-Converts from GraphPart to Verigraph structures-}

import qualified Abstract.Morphism        as M
import qualified Analysis.GraphPart       as GP
import           Data.Maybe               (fromJust)
import           Graph.Graph              as G
import qualified Graph.GraphMorphism      as GM
import           Graph.GraphRule
import qualified Graph.TypedGraphMorphism as TGM

mountTGMBoth :: GM.GraphMorphism a b -> GM.GraphMorphism a b
             -> GP.EqClassGraph
             -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
mountTGMBoth l r g = (mountTGM "Left" l, mountTGM "Right" r)
  where
    typeGraph = M.codomain l
    typedG = mountTypeGraph g (mountG g) typeGraph
    mountTGM side match = TGM.typedMorphism match typedG (mountMapping side g match)

mountG :: GP.EqClassGraph -> Graph a b
mountG (nodes,edges) = build nods edgs
  where
    nods = map (\(n:_) -> GP.nid n) nodes
    edgs = map (\(e:_) -> (GP.eid e, nodeSrc e, nodeTgt e)) edges
    nodeSrc e = GP.nid $ head $ GP.getListNode nodes (GP.source e)
    nodeTgt e = GP.nid $ head $ GP.getListNode nodes (GP.target e)

mountTypeGraph :: GP.EqClassGraph -> Graph a b -> Graph a b -> GM.TypedGraph a b
mountTypeGraph gp g typeG = GM.gmbuild g typeG nodes edges
  where
    nodes = map (\(n:_) -> (GP.nid n, GP.ntype n)) (fst gp)
    edges = map (\(e:_) -> (GP.eid e, GP.etype e)) (snd gp)

mountMapping :: String -> GP.EqClassGraph -> GM.GraphMorphism a b -> GM.GraphMorphism a b
mountMapping side g@(nodes,edges) m = GM.gmbuild (M.domain m) (mountG g) nods edgs
  where
    nods = map (\(NodeId n) -> (n, nodeId n)) (G.nodes (M.domain m))
    nodeId n = GP.nid $ head $ GP.getListNodeName side nodes n
    edgs = map (\(EdgeId e) -> (e, edgeId e)) (G.edges (M.domain m))
    edgeId e = GP.eid $ head $ GP.getListEdgeName side edges e
