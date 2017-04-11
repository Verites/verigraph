module Main where


import           Abstract.Valid
import           Graph.Graph
import           Graph.QuickCheck

import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Function


main :: IO ()
main =
  defaultMain [
    testGroup "operations preserve validity" preserveValidityTests
  , testGroup "graph API" graphApiTests
  ]


preserveValidityTests :: [Test]
preserveValidityTests =
  [ testProperty "empty" $
      isValid empty

  , testProperty "build" $ \nodes edges ->
      isValid (build nodes edges)

  , testProperty "fromNodesAndEdges" $ \nodes edges ->
      isValid (fromNodesAndEdges nodes edges :: Graph Int Int)

  , testProperty "insertNode" $ \graph nodeId ->
      isValid (insertNode nodeId graph :: Graph (Maybe Int) Int)

  , testProperty "insertNodeWithPayload" $ \graph nodeId payload ->
      isValid (insertNodeWithPayload nodeId payload graph :: Graph Int Int)

  , testProperty "insertEdge" $ \graph edgeId srcId tgtId ->
      isValid (insertEdge edgeId srcId tgtId graph :: Graph Int (Maybe Int))

  , testProperty "insertEdgeWithPayload" $ \graph edgeId srcId tgtId payload ->
      isValid (insertEdgeWithPayload edgeId srcId tgtId payload graph :: Graph Int Int)

  , testProperty "removeNode" $ \graph nodeId ->
      isValid (removeNode nodeId graph :: Graph Int Int)

  , testProperty "removeNodeAndIncidentEdges" $ \graph nodeId ->
      isValid (removeNodeAndIncidentEdges nodeId graph :: Graph Int Int)

  , testProperty "removeEdge" $ \graph edgeId ->
      isValid (removeEdge edgeId graph :: Graph Int Int)
  ]


graphApiTests :: [Test]
graphApiTests =
  [ testGroup "insertNode"
      [ testProperty "node present after insert" $ \graph nodeId ->
          lookupNode nodeId (insertNode nodeId graph :: Graph (Maybe Int) Int)
          == Just (Node nodeId Nothing)
      ]

  , testGroup "insertNodeWithPayload"
      [ testProperty "node present after insert" $ \graph nodeId payload ->
          lookupNode nodeId (insertNodeWithPayload nodeId payload graph :: Graph Int Int)
          == Just (Node nodeId payload)
      ]

  , testGroup "insertEdge"
      [ testProperty "edge present after insert" $ \graph edgeId ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeIdOf graph) $ \src ->
          forAll (randomNodeIdOf graph) $ \tgt ->
            lookupEdge edgeId (insertEdge edgeId src tgt graph :: Graph Int (Maybe Int))
            == Just (Edge edgeId src tgt Nothing)
      ]

  , testGroup "insertEdgeWithPayload"
      [ testProperty "edge present after insert" $ \graph edgeId payload ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeIdOf graph) $ \src ->
          forAll (randomNodeIdOf graph) $ \tgt ->
            lookupEdge edgeId (insertEdgeWithPayload edgeId src tgt payload graph :: Graph Int Int)
            == Just (Edge edgeId src tgt payload)
      ]

  , testGroup "removeNode"
      [ testProperty "node removed if isolated" $ \graph ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeInContextOf graph) $ \(node, ctx) ->
          Prelude.null (incidentEdges ctx) ==>
            lookupNode (nodeId node) (removeNode (nodeId node) graph :: Graph Int Int)
            == Nothing
      , testProperty "no edges removed" $ \graph ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeIdOf graph) $ \nodeId ->
            edges (removeNode nodeId graph :: Graph Int Int)
            == edges graph
      ]

  , testGroup "removeNodeAndIncidentEdges"
      [ testProperty "node removed" $ \graph ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeIdOf graph) $ \nodeId ->
            lookupNode nodeId (removeNodeAndIncidentEdges nodeId graph :: Graph Int Int)
            == Nothing
      ]

  , testGroup "removeEdge"
      [ testProperty "edge removed" $ \graph ->
          not (Prelude.null $ edges graph) ==>
          forAll (randomEdgeIdOf graph) $ \edgeId ->
            lookupEdge edgeId (removeEdge edgeId graph :: Graph Int Int)
            == Nothing
      ]

  , testGroup "updateNodePayload"
      [ testProperty "payload changed" $ \graph f ->
          not (Prelude.null $ nodes graph) ==>
          forAll (randomNodeIdOf graph) $ \nodeId ->
            let
              graph' = updateNodePayload nodeId graph (apply f) :: Graph Int Int
            in
              (nodeInfo <$> lookupNode nodeId graph')
              == (apply f . nodeInfo <$> lookupNode nodeId graph)
      ]

  , testGroup "updateEdgePayload"
      [ testProperty "payload changed" $ \graph f ->
          not (Prelude.null $ edges graph) ==>
          forAll (randomEdgeIdOf graph) $ \edgeId ->
            let
              graph' = updateEdgePayload edgeId graph (apply f) :: Graph Int Int
            in
              (edgeInfo <$> lookupEdge edgeId graph')
              == (apply f . edgeInfo <$> lookupEdge edgeId graph)
      ]
  ]
