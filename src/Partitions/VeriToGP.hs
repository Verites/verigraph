module Partitions.VeriToGP (
   mixLeftRule,
   mixGM,
   mixNac
   ) where

import qualified Abstract.Morphism        as M
import           Data.List                (partition)
import           Graph.Graph
import           Graph.GraphRule
import           Graph.GraphMorphism
import           Graph.TypedGraphMorphism as TGM
import qualified Partitions.GraphPart     as GP

--from verigraph to GraphPart

-- | Creates the disjoint union of left sides of two rules in 'GraphPart' format
mixLeftRule :: GraphRule a b -> GraphRule a b -> GP.Graph
mixLeftRule l r = mixGM (M.codomain (left l)) (M.codomain (left r))

-- | Creates the disjoint union of two verigraph graphs in 'GraphPart' format
mixGM :: GraphMorphism a b -> GraphMorphism a b -> GP.Graph
mixGM l r = disjUnionGraphs left right
   where
     (left,id) = tgmToGP l "Left" 0
     (right,_) = tgmToGP r "Right" id
     disjUnionGraphs a b = GP.Graph (GP.nodes a ++ GP.nodes b) (GP.edges a ++ GP.edges b)

-- | Creates the disjoint union of two verigraph graphs in 'GraphPart' format, with restriction to @nac@
mixNac :: GraphMorphism a b -> TGM.TypedGraphMorphism a b -> GP.Graph
mixNac r nac = disjUnionGraphs left rightNac
   where
     (left,id) = tgmToGP r "Left" 0
     (right,_) = tgmToGP (M.codomain nac) "Right" id
     rightNac = setNotChange nac right
     disjUnionGraphs a b = GP.Graph (GP.nodes a ++ GP.nodes b) (GP.edges a ++ GP.edges b)

setNotChange :: TGM.TypedGraphMorphism a b -> GP.Graph -> GP.Graph
setNotChange nac (GP.Graph nodes edges) = GP.Graph nods edgs
  where
    nods = map (\(GP.Node a n c _ e) -> (GP.Node a n c (TGM.applyNodeTGM nac (NodeId n) == Nothing) e)) nodes
    edgs = map (\(GP.Edge a n c d e _ g) -> (GP.Edge a n c d e (TGM.applyEdgeTGM nac (EdgeId n) == Nothing) g)) edges

tgmToGP :: GraphMorphism a b -> String -> Int -> (GP.Graph,Int)
tgmToGP morfL side id = (GP.Graph nods edgs, nextId)
   where
      nods   = nodesToGP morfL side id $ nodes graphL
      edgs   = edgesToGP morfL side graphL id $ edges graphL
      graphL = M.domain morfL
      nextId = max (length nods) (length edgs)

nodesToGP :: TypedGraph a b -> String -> Int -> [NodeId] -> [GP.Node]
nodesToGP _  _    _  []            = []
nodesToGP tg side id (NodeId b:xs) = GP.Node n b id True side : nodesToGP tg side (id+1) xs
   where
     Just (NodeId n) = applyNode tg (NodeId b)

edgesToGP :: TypedGraph a b -> String -> Graph a b -> Int -> [EdgeId] -> [GP.Edge]
edgesToGP _  _    _ _  []            = []
edgesToGP tg side g id (EdgeId b:xs) = GP.Edge typ b id src tgt True side : edgesToGP tg side g (id+1) xs
   where
      Just (EdgeId typ) = applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ (-1) True side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ (-1) True side
      Just (NodeId n1) = applyNode tg (NodeId src_)
      Just (NodeId n2) = applyNode tg (NodeId tgt_)
