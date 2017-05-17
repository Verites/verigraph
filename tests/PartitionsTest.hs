{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Math.Combinat.Numbers (bellNumber)
import           Test.HUnit

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Graph.Graph
import           Graph.GraphMorphism
import           SndOrder.Morphism
import           TypedGraph.Morphism   hiding (createEdgeOnDomain, createNodeOnDomain)
import           Utils
import qualified XML.GGXReader as XML

main :: IO ()
main = do
  let fileName = "tests/grammars/sndOrderEpi.ggx"
      dpoConf1 = MorphismsConfig MonoMatches MonomorphicNAC
      dpoConf2 = MorphismsConfig AnyMatches MonomorphicNAC
      dpoConf3 = MorphismsConfig MonoMatches PartiallyMonomorphicNAC
      dpoConf4 = MorphismsConfig AnyMatches PartiallyMonomorphicNAC
  
  (gg1a,gg2a,_) <- XML.readGrammar fileName False dpoConf1
  (gg1b,gg2b,_) <- XML.readGrammar fileName False dpoConf2
  (gg1c,gg2c,_) <- XML.readGrammar fileName False dpoConf3
  (gg1d,gg2d,_) <- XML.readGrammar fileName False dpoConf4
  
  runTests $
    testPartition :
    testCreateJointlyEpimorphicPairsFromNAC dpoConf1 gg1a gg2a 21 :
    testCreateJointlyEpimorphicPairsFromNAC dpoConf2 gg1b gg2b 26 :
    testCreateJointlyEpimorphicPairsFromNAC dpoConf3 gg1c gg2c 34 :
    testCreateJointlyEpimorphicPairsFromNAC dpoConf4 gg1d gg2d 43 :
    []

testCreateJointlyEpimorphicPairsFromNAC conf gg1 gg2 n = (length epi) ~?= n
  where
    r1 = snd (head (productions gg1))
    r2 = snd (head (productions gg2))
    
    rr1 = buildProduction (getLHS r1) (getRHS r1) []
    rr2 = head (getNACs r2)
    
    epi = createJointlyEpimorphicPairsFromNAC conf (codomain (getLHS rr1)) (mappingLeft rr2)

testPartition =
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
