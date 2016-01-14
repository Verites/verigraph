module CriticalPairs.VeriToGP (
   mixLeftRule,
   mixTGM
   ) where

import Graph.Graph
import qualified CriticalPairs.GraphPart as GP
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import Graph.GraphRule

import Data.Maybe
import qualified Abstract.Morphism as M

--from verigraph to GraphPart

mixLeftRule :: GraphRule a b -> GraphRule a b -> GP.Graph
mixLeftRule l r = mixTGM (left l) (left r)

mixTGM :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> GP.Graph
mixTGM l r = disjUnionGraphs (tgmToGP l "Left") (tgmToGP r "Right")
   where
      disjUnionGraphs a b = GP.Graph ((GP.nodes a) ++ (GP.nodes b)) ((GP.edges a) ++ (GP.edges b))

tgmToGP :: TGM.TypedGraphMorphism a b -> String -> GP.Graph
tgmToGP tgm side = GP.Graph nods edgs
   where
      nods   = nodesToGP morfL side [] $ nodes graphL
      edgs   = edgesToGP morfL side graphL [] $ edges graphL
      morfL  = M.codomain tgm --morfismo L -> grafotipo
      graphL = M.domain morfL --grafo L

nodesToGP :: GM.TypedGraph a b -> String -> [GP.Node] -> [NodeId] -> [GP.Node]
nodesToGP _  _    a []              = a
nodesToGP tg side a ((NodeId b):xs) = nodesToGP tg side ((GP.Node n b side GP.Undefined):a) xs
   where
     (NodeId n) = fromJust $ GM.applyNode tg (NodeId b)

edgesToGP :: GM.TypedGraph a b -> String -> Graph a b -> [GP.Edge] -> [EdgeId] -> [GP.Edge]
edgesToGP _  _    _ a []              = a
edgesToGP tg side g a ((EdgeId b):xs) = edgesToGP tg side g ((GP.Edge typ b src tgt side GP.Undefined):a) xs
   where
      (EdgeId typ) = fromJust $ GM.applyEdge tg (EdgeId b)
      src = GP.Node n1 (read (show $ fromJust $ sourceOf g (EdgeId b)) :: Int) side GP.Undefined
      tgt = GP.Node n2 (read (show $ fromJust $ targetOf g (EdgeId b)) :: Int) side GP.Undefined
      (NodeId n1) = fromJust $ GM.applyNode tg (fromJust $ sourceOf g (EdgeId b))
      (NodeId n2) = fromJust $ GM.applyNode tg (fromJust $ targetOf g (EdgeId b))
