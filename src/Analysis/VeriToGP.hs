module Analysis.VeriToGP (
   mixLeftRule,
   mixTGM
   ) where

import qualified Analysis.GraphPart       as GP
import           Graph.Graph
import qualified Graph.GraphMorphism      as GM
import           Graph.GraphRule
import           Graph.TypedGraphMorphism
import qualified Abstract.Morphism        as M

--from verigraph to GraphPart

type Map a = [(a,Int)]
type Maps = (Map GP.Node, Map GP.Edge)

uniqueId :: [GP.Node] -> [GP.Edge] -> Maps
uniqueId nodes edges = (zip nodes [0..], zip edges [0..])

setUniqueId :: GP.Graph -> GP.Graph
setUniqueId (GP.Graph nodes edges) = GP.Graph n e
  where
    n = map (\(id,(GP.Node a b _ d)) -> (GP.Node a b id d)) (zip [0..] nodes)
    e = map (\(id,(GP.Edge a b _ d e f)) -> (GP.Edge a b id d e f)) (zip [0..] edges)

mixLeftRule :: GraphRule a b -> GraphRule a b -> GP.Graph
mixLeftRule l r = setUniqueId $ mixTGM (M.codomain (left l)) (M.codomain (left r))

mixTGM :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> GP.Graph
mixTGM l r = setUniqueId $ disjUnionGraphs (tgmToGP l "Left") (tgmToGP r "Right")
   where
      disjUnionGraphs a b = GP.Graph (GP.nodes a ++ GP.nodes b) (GP.edges a ++ GP.edges b)

tgmToGP :: GM.GraphMorphism a b -> String -> GP.Graph
tgmToGP morfL side = GP.Graph nods edgs
   where
      nods   = nodesToGP morfL side $ nodes graphL
      edgs   = edgesToGP morfL side graphL $ edges graphL
      graphL = M.domain morfL

nodesToGP :: GM.TypedGraph a b -> String -> [NodeId] -> [GP.Node]
nodesToGP _  _    []            = []
nodesToGP tg side (NodeId b:xs) = GP.Node n b 0 side : (nodesToGP tg side xs)
   where
     Just (NodeId n) = GM.applyNode tg (NodeId b)

edgesToGP :: GM.TypedGraph a b -> String -> Graph a b -> [EdgeId] -> [GP.Edge]
edgesToGP _  _    _ []            = []
edgesToGP tg side g (EdgeId b:xs) = (GP.Edge typ b 0 src tgt side) : edgesToGP tg side g xs
   where
      Just (EdgeId typ) = GM.applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ 0 side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ 0 side
      Just (NodeId n1) = GM.applyNode tg (NodeId src_)
      Just (NodeId n2) = GM.applyNode tg (NodeId tgt_)
