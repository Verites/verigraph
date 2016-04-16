module Partitions.VeriToGP (
   mixLeftRule,
   mixGM,
   mixNac
   ) where

import qualified Abstract.Morphism        as M
import           Graph.Graph              as G
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
     (left,id) = tgmToGP Nothing l "Left" 0
     (right,_) = tgmToGP Nothing r "Right" id
     nodes = fst
     edges = snd
     disjUnionGraphs a b = ((nodes a ++ nodes b),(edges a ++ edges b))

-- | Creates the disjoint union of two verigraph graphs in 'GraphPart' format, with restriction to @nac@
mixNac :: GraphMorphism a b -> TGM.TypedGraphMorphism a b -> GP.Graph
mixNac r nac = disjUnionGraphs left right
   where
     (left,id) = tgmToGP Nothing r "Left" 0
     inac = TGM.invertTGM nac
     (right,_) = tgmToGP (Just inac) (M.codomain nac) "Right" id
     nodes = fst
     edges = snd
     disjUnionGraphs a b = ((nodes a ++ nodes b),(edges a ++ edges b))

tgmToGP :: Maybe (TGM.TypedGraphMorphism a b) -> GraphMorphism a b -> String -> Int -> (GP.Graph,Int)
tgmToGP inac morfL side id = ((nods,edgs), nextId)
   where
      nods   = nodesToGP inac morfL side id $ nodes graphL
      edgs   = edgesToGP inac morfL side graphL id $ edges graphL
      graphL = M.domain morfL
      nextId = max (length nods) (length edgs)

nodesToGP :: Maybe (TGM.TypedGraphMorphism a b) -> TypedGraph a b -> String -> Int -> [NodeId] -> [GP.Node]
nodesToGP _    _  _    _  []            = []
nodesToGP inac tg side id (NodeId b:xs) = GP.Node n b id flag side : nodesToGP inac tg side (id+1) xs
   where
     Just (NodeId n) = applyNode tg (NodeId b)
     flag = case inac of
              Just tgm -> checkNodeMix tgm (NodeId n)
              Nothing -> True

edgesToGP :: Maybe (TGM.TypedGraphMorphism a b) -> TypedGraph a b -> String -> Graph a b -> Int -> [EdgeId] -> [GP.Edge]
edgesToGP _    _  _    _ _  []            = []
edgesToGP inac tg side g id (EdgeId b:xs) = GP.Edge typ b id src tgt flag side : edgesToGP inac tg side g (id+1) xs
   where
      Just (EdgeId typ) = applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ (-1) flagSrc side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ (-1) flagTgt side
      Just (NodeId n1) = applyNode tg (NodeId src_)
      Just (NodeId n2) = applyNode tg (NodeId tgt_)
      flag = case inac of
               Just tgm -> checkEdgeMix tgm (EdgeId b)
               Nothing -> True
      flagSrc = case inac of
                  Just tgm -> checkNodeMix tgm (NodeId n1)
                  Nothing -> True
      flagTgt = case inac of
                  Just tgm -> checkNodeMix tgm (NodeId n2)
                  Nothing -> True

checkNodeMix :: TGM.TypedGraphMorphism a b -> NodeId -> Bool
checkNodeMix tgm n = n `elem` (G.nodes (M.domain (M.domain tgm))) && (TGM.applyNodeTGM tgm n == Nothing)

checkEdgeMix :: TGM.TypedGraphMorphism a b -> EdgeId -> Bool
checkEdgeMix tgm e = e `elem` (G.edges (M.domain (M.domain tgm))) && (TGM.applyEdgeTGM tgm e == Nothing)
