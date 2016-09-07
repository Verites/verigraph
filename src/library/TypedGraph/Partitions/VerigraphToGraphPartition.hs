module TypedGraph.Partitions.VerigraphToGraphPartition (
   --  mixLeftRule,
   mixGM,
   mixNac
   ) where

import qualified Abstract.Morphism                    as M
import           Graph.Graph                          as G
import           Graph.GraphMorphism
import           TypedGraph.Graph
import           TypedGraph.MorphismCore
import qualified TypedGraph.Partitions.GraphPartition as GP

-- | Creates the disjoint union of two verigraph graphs in 'GraphPartition' format
mixGM :: (GraphMorphism a b,Bool) -> (GraphMorphism a b,Bool) -> GP.Graph
mixGM (l,injL) (r,injR) = disjUnionGraphs left right
   where
     (left,id) = tgmToGP injectiveL l True 0
     (right,_) = tgmToGP injectiveR r False id
     injectiveL = if injL then (G.nodes (M.domain l), G.edges (M.domain l)) else ([],[])
     injectiveR = if injR then (G.nodes (M.domain r), G.edges (M.domain r)) else ([],[])
     nodes = fst
     edges = snd
     disjUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

-- | Creates the disjoint union of two verigraph graphs in 'GraphPartition' format, with restriction to @nac@
mixNac :: (GraphMorphism a b,Bool) -> (TypedGraphMorphism a b,Bool) -> GP.Graph
mixNac (r,injR) (nac,injN) = disjUnionGraphs left right
   where
     (left,id) = tgmToGP injectiveR r True 0
     (right,_) = tgmToGP injectiveN (M.codomain nac) False id
     injectiveR = if injR then (G.nodes (M.domain r), G.edges (M.domain r)) else ([],[])
     injectiveN = if injN then (nodesCodomain nac, edgesCodomain nac) else (injNodes, injEdges)
     injNodes = filter (\n -> countIncidentMap (applyNodeTGM nac) (nodesDomain nac) n < 2) (nodesCodomain nac)
     injEdges = filter (\n -> countIncidentMap (applyEdgeTGM nac) (edgesCodomain nac) n < 2) (edgesCodomain nac)
     nodes = fst
     edges = snd
     disjUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

countIncidentMap :: Eq a => (a -> Maybe a) -> [a] -> a -> Int
countIncidentMap f l y = length $ filter (\x -> f x == Just y) l

tgmToGP :: ([NodeId],[EdgeId]) -> GraphMorphism a b -> Bool -> Int -> (GP.Graph,Int)
tgmToGP inj@(injNodes,_) morfL side id = ((nods,edgs), nextId)
   where
      nods   = nodesToGP injNodes morfL side id $ nodes graphL
      edgs   = edgesToGP inj morfL side graphL id $ edges graphL
      graphL = M.domain morfL
      nextId = max (length nods) (length edgs)

nodesToGP :: [NodeId] -> TypedGraph a b -> Bool -> Int -> [NodeId] -> [GP.Node]
nodesToGP _        _  _    _  []            = []
nodesToGP injNodes tg side id (NodeId b:xs) = GP.Node n b id flag side : nodesToGP injNodes tg side (id+1) xs
   where
     Just (NodeId n) = applyNode tg (NodeId b)
     flag = NodeId b `elem` injNodes

edgesToGP :: ([NodeId],[EdgeId]) -> TypedGraph a b -> Bool -> Graph a b -> Int -> [EdgeId] -> [GP.Edge]
edgesToGP _        _  _    _ _  []            = []
edgesToGP inj@(injNodes,injEdges) tg side g id (EdgeId b:xs) = GP.Edge typ b id src tgt flag side : edgesToGP inj tg side g (id+1) xs
   where
      Just (EdgeId typ) = applyEdge tg (EdgeId b)
      Just (NodeId src_) = sourceOf g (EdgeId b)
      src = GP.Node n1 src_ (-1) flagSrc side
      Just (NodeId tgt_) = targetOf g (EdgeId b)
      tgt = GP.Node n2 tgt_ (-1) flagTgt side
      Just (NodeId n1) = applyNode tg (NodeId src_)
      Just (NodeId n2) = applyNode tg (NodeId tgt_)
      flag = EdgeId b `elem` injEdges
      flagSrc = NodeId src_ `elem` injNodes
      flagTgt = NodeId tgt_ `elem` injNodes
