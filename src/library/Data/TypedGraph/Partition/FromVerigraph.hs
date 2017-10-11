{-|
Description : This module converts Verigraph into Partitions structures.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
-}
module Data.TypedGraph.Partition.FromVerigraph
  ( createDisjointUnion,
    createSatisfyingNacsDisjointUnion
  ) where

import           Data.Graphs                     as G
import           Data.Graphs.Morphism            as GM
import           Data.TypedGraph                 as TG
import           Data.TypedGraph.Morphism        as TGM
import qualified Data.TypedGraph.Partition.Types as GP

-- | The starting integer id for the generated elements.
startId :: Int
startId = 0

-- | Creates the disjoint union of two verigraph graphs in 'GraphPartition' format
createDisjointUnion :: (TypedGraph a b,Bool) -> (TypedGraph a b,Bool) -> GP.Graph
createDisjointUnion (g1,inj1) (g2,inj2) = disjointUnionGraphs left right
  where
    nodes = fst
    edges = snd
    injectiveLeft = if inj1 then (G.nodeIds (GM.domainGraph g1), G.edgeIds (GM.domainGraph g1)) else ([],[])
    injectiveRight = if inj2 then (G.nodeIds (GM.domainGraph g2), G.edgeIds (GM.domainGraph g2)) else ([],[])
    (left,nextId) = graphMorphismToPartitionGraph injectiveLeft g1 True startId
    (right,_) = graphMorphismToPartitionGraph injectiveRight g2 False nextId
    disjointUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

-- | Creates the disjoint union of two verigraph graphs for a right graph and a nac in 'GraphPartition' format
createSatisfyingNacsDisjointUnion :: (TypedGraph a b,Bool) -> (TypedGraphMorphism a b,Bool) -> GP.Graph
createSatisfyingNacsDisjointUnion (g,injG) (n,injN) = disjointUnionGraphs left right
  where
    nodes = fst
    edges = snd
    injNodes = filter (\x -> (all (\n' -> TGM.applyNodeIdUnsafe n n' /= x) (TG.nodeIds $ TGM.domainGraph n))) (TG.nodeIds $ TGM.codomainGraph n)
    injEdges = filter (\x -> (all (\e' -> TGM.applyEdgeIdUnsafe n e' /= x) (TG.edgeIds $ TGM.domainGraph n))) (TG.edgeIds $ TGM.codomainGraph n)
    injectiveR = if injG then (G.nodeIds (GM.domainGraph g), G.edgeIds (GM.domainGraph g)) else ([],[])
    injectiveN = if injN then (TG.nodeIds $ TGM.codomainGraph n, TG.edgeIds $ TGM.codomainGraph n) else (injNodes, injEdges)
    (left,nextId) = graphMorphismToPartitionGraph injectiveR g True startId
    (right,_) = graphMorphismToPartitionGraph injectiveN (TGM.codomainGraph n) False nextId
    disjointUnionGraphs a b = (nodes a ++ nodes b, edges a ++ edges b)

graphMorphismToPartitionGraph :: ([NodeId],[EdgeId]) -> GraphMorphism (Maybe a) (Maybe b) -> Bool -> Int -> (GP.Graph,Int)
graphMorphismToPartitionGraph inj@(injNodes,_) morfL side id = ((nodes',edges'), nextId)
  where
    graphL = GM.domainGraph morfL
    nodes_ = zip (G.nodeIds graphL) [id..]
    nodes' = map (nodeToPartitionNode injNodes morfL side) nodes_
    edges_ = zip (G.edges graphL) [id..]
    edges' = map (edgeToPartitionEdge inj morfL side) edges_
    nextId = max (length nodes') (length edges')

nodeToPartitionNode :: [NodeId] -> TypedGraph a b -> Bool -> (NodeId,Int) -> GP.Node
nodeToPartitionNode injNodes tg side (NodeId b,id) = GP.Node n b id flag side
  where
    NodeId n = GM.applyNodeIdUnsafe tg (NodeId b)
    flag = NodeId b `elem` injNodes

edgeToPartitionEdge :: ([NodeId],[EdgeId]) -> TypedGraph a b -> Bool -> (Edge (Maybe b),Int) -> GP.Edge
edgeToPartitionEdge (injNodes,injEdges) tg side (e,id) =
  GP.Edge typ edgeNumber id src tgt flag side
    where
      EdgeId edgeNumber = edgeId e

      EdgeId typ = GM.applyEdgeIdUnsafe tg (edgeId e)

      src = GP.Node n1 src_ (-1) flagSrc side
      NodeId src_ = sourceId e

      tgt = GP.Node n2 tgt_ (-1) flagTgt side
      NodeId tgt_ = targetId e

      NodeId n1 = GM.applyNodeIdUnsafe tg (NodeId src_)
      NodeId n2 = GM.applyNodeIdUnsafe tg (NodeId tgt_)

      flag = edgeId e `elem` injEdges
      flagSrc = NodeId src_ `elem` injNodes
      flagTgt = NodeId tgt_ `elem` injNodes
