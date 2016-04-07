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

mixLeftRule :: GraphRule a b -> GraphRule a b -> GP.Graph
mixLeftRule l r = mixTGM (left l) (left r)

mixTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> GP.Graph
mixTGM l r = disjUnionGraphs (tgmToGP l "Left") (tgmToGP r "Right")
   where
      disjUnionGraphs a b = GP.Graph (GP.nodes a ++ GP.nodes b) (GP.edges a ++ GP.edges b)

tgmToGP :: TypedGraphMorphism a b -> String -> GP.Graph
tgmToGP tgm side = GP.Graph nods edgs
   where
      nods   = nodesToGP morfL side $ nodes graphL
      edgs   = edgesToGP morfL side graphL $ edges graphL
      morfL  = M.codomain tgm
      graphL = M.domain morfL

nodesToGP :: GM.TypedGraph a b -> String -> [NodeId] -> [GP.Node]
nodesToGP _  _    []            = []
nodesToGP tg side (NodeId b:xs) = GP.Node n b side : (nodesToGP tg side xs)
   where
     Just (NodeId n) = GM.applyNode tg (NodeId b)

edgesToGP :: GM.TypedGraph a b -> String -> Graph a b -> [EdgeId] -> [GP.Edge]
edgesToGP _  _    _ []            = []
edgesToGP tg side g (EdgeId b:xs) = (GP.Edge typ b src tgt side) : edgesToGP tg side g xs
   where
      Just (EdgeId typ) = GM.applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ side
      Just (NodeId n1) = GM.applyNode tg (NodeId src_)
      Just (NodeId n2) = GM.applyNode tg (NodeId tgt_)
