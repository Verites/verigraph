{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Abstract.AdhesiveHLR
import           Graph.Graph
import           Graph.GraphMorphism
import           Math.Combinat.Numbers (bellNumber)
import           Test.HUnit
import           TypedGraph.Morphism   hiding (createEdgeOnDomain, createNodeOnDomain)
import           Utils

main :: IO ()
main = runTests tests

tests =
  test (
  -- Tests if a graph with the same repeated node n times has partitions length equals to the n-th bell number
  map
    (\n ->
       ("BellNumber " ++ show n ++ " nodes") ~:
       fromInteger (bellNumber n) ~=?
       length (getPart (graph1 [1..n])))
    ids
    :
    -- Tests if a graph with the same repeated edge n times has partitions length equals to the n-th bell number
    [map
      (\e ->
         ("BellNumber " ++ show e ++ " edges") ~:
         fromInteger (bellNumber e) ~=?
         length (getPart (graph2 [1..e])))
      ids])

getPart :: GraphMorphism (Maybe a) (Maybe b) -> [TypedGraphMorphism a b]
getPart = createAllSubobjects False

limitBellNumber = 8
ids = [1..limitBellNumber]

--typegraph: graph with one node and one edge on itself
typegraph = insertEdge (EdgeId 0) (NodeId 0) (NodeId 0) (insertNode (NodeId 0) Graph.Graph.empty)

--graph1: typed graph with 'limitBellNumber' nodes of same type
initGraph1 = Graph.GraphMorphism.empty Graph.Graph.empty typegraph
graph1 = foldr (\n -> createNodeOnDomain (NodeId n) (NodeId 0)) initGraph1

--graph2: typed graph with 'limitBellNumber' edges of same type with the same source and target
initGraph2 = Graph.GraphMorphism.empty (insertNode (NodeId 0) Graph.Graph.empty) typegraph
graph2 = foldr
           (\e -> createEdgeOnDomain (EdgeId e) (NodeId 0) (NodeId 0) (EdgeId 0))
           (updateNodes (NodeId 0) (NodeId 0) initGraph2)
