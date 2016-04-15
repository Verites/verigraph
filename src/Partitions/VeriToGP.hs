module Partitions.VeriToGP (
   mixLeftRule,
   mixGM
   ) where

import qualified Abstract.Morphism        as M
import           Graph.Graph
import           Graph.GraphRule
import           Graph.GraphMorphism
import qualified Partitions.GraphPart     as GP

--from verigraph to GraphPart

setUniqueId :: GP.Graph -> GP.Graph
setUniqueId (GP.Graph nodes edges) = GP.Graph n e
  where
    n = map (\(id, GP.Node a b _ d) -> (GP.Node a b id d)) (zip [0..] nodes)
    e = map (\(id, GP.Edge a b _ d e f) -> (GP.Edge a b id d e f)) (zip [0..] edges)

-- | Creates the disjoint union of left sides of two rules in 'GraphPart' format
mixLeftRule :: GraphRule a b -> GraphRule a b -> GP.Graph
mixLeftRule l r = setUniqueId $ mixGM (M.codomain (left l)) (M.codomain (left r))

-- | Creates the disjoint union of two verigraph graphs in 'GraphPart' format
mixGM :: GraphMorphism a b -> GraphMorphism a b -> GP.Graph
mixGM l r = setUniqueId $ disjUnionGraphs (tgmToGP l "Left") (tgmToGP r "Right")
   where
      disjUnionGraphs a b = GP.Graph (GP.nodes a ++ GP.nodes b) (GP.edges a ++ GP.edges b)

tgmToGP :: GraphMorphism a b -> String -> GP.Graph
tgmToGP morfL side = GP.Graph nods edgs
   where
      nods   = nodesToGP morfL side $ nodes graphL
      edgs   = edgesToGP morfL side graphL $ edges graphL
      graphL = M.domain morfL

nodesToGP :: TypedGraph a b -> String -> [NodeId] -> [GP.Node]
nodesToGP _  _    []            = []
nodesToGP tg side (NodeId b:xs) = GP.Node n b (-1) side : nodesToGP tg side xs
   where
     Just (NodeId n) = applyNode tg (NodeId b)

edgesToGP :: TypedGraph a b -> String -> Graph a b -> [EdgeId] -> [GP.Edge]
edgesToGP _  _    _ []            = []
edgesToGP tg side g (EdgeId b:xs) = GP.Edge typ b (-1) src tgt side : edgesToGP tg side g xs
   where
      Just (EdgeId typ) = applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ (-1) side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ (-1) side
      Just (NodeId n1) = applyNode tg (NodeId src_)
      Just (NodeId n2) = applyNode tg (NodeId tgt_)
