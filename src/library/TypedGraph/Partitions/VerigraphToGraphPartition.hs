module TypedGraph.Partitions.VerigraphToGraphPartition (
   createDisjointUnion,
   createSatisfyingNacsDisjointUnion
   ) where

import qualified Abstract.Morphism                    as M
import           Graph.Graph                          as G
import           Graph.GraphMorphism                  as GM
import           TypedGraph.Graph
import           TypedGraph.Morphism.Core             as TGM
import qualified TypedGraph.Partitions.GraphPartition as GP

-- | Creates the disjoint union of two verigraph graphs in 'GraphPartition' format
createDisjointUnion :: (TypedGraph a b,Bool) -> (TypedGraph a b,Bool) -> GP.Graph
createDisjointUnion (g1,inj1) (g2,inj2) = disjointUnionGraphs left right
   where
     nodes = fst
     edges = snd
     injective1 = if inj1 then (G.nodeIds (M.domain g1), G.edgeIds (M.domain g1)) else ([],[])
     injective2 = if inj2 then (G.nodeIds (M.domain g2), G.edgeIds (M.domain g2)) else ([],[])
     (left,id) = graphMorphismToPartitionGraph injective1 g1 True 0
     (right,_) = graphMorphismToPartitionGraph injective2 g2 False id
     disjointUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

-- | Creates the disjoint union of two verigraph graphs for a right graph and a nac in 'GraphPartition' format
createSatisfyingNacsDisjointUnion :: (TypedGraph a b,Bool) -> (TypedGraphMorphism a b,Bool) -> GP.Graph
createSatisfyingNacsDisjointUnion (g,injG) (n,injN) = disjointUnionGraphs left right
   where
     nodes = fst
     edges = snd
     injNodes = filter (\x -> countIncidentMap (TGM.applyNode n) (nodeIdsFromDomain n) x < 2) (nodeIdsFromCodomain n)
     injEdges = filter (\x -> countIncidentMap (TGM.applyEdge n) (edgeIdsFromCodomain n) x < 2) (edgeIdsFromCodomain n)
     injectiveR = if injG then (G.nodeIds (M.domain g), G.edgeIds (M.domain g)) else ([],[])
     injectiveN = if injN then (nodeIdsFromCodomain n, edgeIdsFromCodomain n) else (injNodes, injEdges)
     (left,id) = graphMorphismToPartitionGraph injectiveR g True 0
     (right,_) = graphMorphismToPartitionGraph injectiveN (M.codomain n) False id
     disjointUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

countIncidentMap :: Eq a => (a -> Maybe a) -> [a] -> a -> Int
countIncidentMap f l y = length $ filter (\x -> f x == Just y) l

graphMorphismToPartitionGraph :: ([NodeId],[EdgeId]) -> GraphMorphism (Maybe a) (Maybe b) -> Bool -> Int -> (GP.Graph,Int)
graphMorphismToPartitionGraph inj@(injNodes,_) morfL side id = ((nodes',edges'), nextId)
   where
      graphL = M.domain morfL
      nodes'   = nodesToPartitionNodes injNodes morfL side id $ nodeIds graphL
      edges'   = edgesToPartitionEdges inj morfL side graphL id $ edges graphL
      nextId = max (length nodes') (length edges')

nodesToPartitionNodes :: [NodeId] -> TypedGraph a b -> Bool -> Int -> [NodeId] -> [GP.Node]
nodesToPartitionNodes _        _  _    _  []            = []
nodesToPartitionNodes injNodes tg side id (NodeId b:xs) = GP.Node n b id flag side : nodesToPartitionNodes injNodes tg side (id+1) xs
   where
     Just (NodeId n) = GM.applyNode tg (NodeId b)
     flag = NodeId b `elem` injNodes

edgesToPartitionEdges :: ([NodeId],[EdgeId]) -> TypedGraph a b -> Bool -> Graph (Maybe a) (Maybe b) -> Int -> [Edge (Maybe b)] -> [GP.Edge]
edgesToPartitionEdges _        _  _    _ _  []            = []
edgesToPartitionEdges inj@(injNodes,injEdges) tg side g id (e:xs) =
  GP.Edge typ edgeNumber id src tgt flag side : edgesToPartitionEdges inj tg side g (id+1) xs
    where
      EdgeId edgeNumber = edgeId e
      
      Just (EdgeId typ) = GM.applyEdge tg (edgeId e)
      
      src = GP.Node n1 src_ (-1) flagSrc side
      NodeId src_ = sourceId e
      
      tgt = GP.Node n2 tgt_ (-1) flagTgt side
      NodeId tgt_ = targetId e
      
      Just (NodeId n1) = GM.applyNode tg (NodeId src_)
      Just (NodeId n2) = GM.applyNode tg (NodeId tgt_)
      
      flag = (edgeId e) `elem` injEdges
      flagSrc = NodeId src_ `elem` injNodes
      flagTgt = NodeId tgt_ `elem` injNodes
