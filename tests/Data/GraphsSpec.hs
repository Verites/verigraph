module Data.GraphsSpec where

import           Data.Maybe
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Base.Valid
import           Data.Graphs
import           Data.Graphs.QuickCheck


spec :: Spec
spec = do

  describe "empty" $
    it "is valid" $ empty `shouldSatisfy` isValid

  describe "build" $
    prop "is always valid" $ \nodes edges -> isValid (build nodes edges)

  describe "fromNodesAndEdges" $
    prop "is always valid" $ \nodes edges ->
      isValid (fromNodesAndEdges nodes edges :: Graph Int Int)

  describe "insertNode" $ do
    prop "preserves validity" $ \graph nodeId ->
      isValid (insertNode nodeId graph :: Graph (Maybe Int) Int)

    prop "node present after insert" $ \graph nodeId ->
      lookupNode nodeId (insertNode nodeId graph :: Graph (Maybe Int) Int)
      == Just (Node nodeId Nothing)

  describe "insertNodeWithPayload" $ do
    prop "preserves validity" $ \graph nodeId payload ->
      isValid (insertNodeWithPayload nodeId payload graph :: Graph Int Int)

    prop "node present after insert" $ \graph nodeId payload ->
      lookupNode nodeId (insertNodeWithPayload nodeId payload graph :: Graph Int Int)
      == Just (Node nodeId payload)

  describe "insertEdge" $ do
    prop "preserves validity" $ \graph edgeId srcId tgtId ->
      isValid (insertEdge edgeId srcId tgtId graph :: Graph Int (Maybe Int))

    prop "edge present after insert" $ \graph edgeId ->
      not (Prelude.null $ nodes graph) ==>
      forAll (randomNodeIdOf graph) $ \src ->
      forAll (randomNodeIdOf graph) $ \tgt ->
        lookupEdge edgeId (insertEdge edgeId src tgt graph :: Graph Int (Maybe Int))
        == Just (Edge edgeId src tgt Nothing)


  describe "insertEdgeWithPayload" $ do
    prop "preserves validity" $ \graph edgeId srcId tgtId payload ->
      isValid (insertEdgeWithPayload edgeId srcId tgtId payload graph :: Graph Int Int)

    prop "edge present after insert" $ \graph edgeId payload ->
      not (Prelude.null $ nodes graph) ==>
      forAll (randomNodeIdOf graph) $ \src ->
      forAll (randomNodeIdOf graph) $ \tgt ->
        lookupEdge edgeId (insertEdgeWithPayload edgeId src tgt payload graph :: Graph Int Int)
        == Just (Edge edgeId src tgt payload)


  describe "removeNode" $ do
    prop "preserves validity" $ \graph nodeId ->
      isValid (removeNode nodeId graph :: Graph Int Int)

    prop "node removed if isolated" $ \graph ->
      not (Prelude.null $ nodes graph) ==>
      (`all` nodesInContext graph) $ \(node, ctx) ->
      Prelude.null (incidentEdges ctx) -->
      isNothing $ lookupNode (nodeId node) (removeNode (nodeId node) graph :: Graph Int Int)

    prop "removes no edges" $ \graph ->
      not (Prelude.null $ nodes graph) ==>
      (`all` nodeIds graph) $ \nodeId ->
        edges (removeNode nodeId graph :: Graph Int Int)
        == edges graph

  describe "removeNodeAndIncidentEdges" $ do
    prop "preserves validity" $ \graph nodeId ->
      isValid (removeNodeAndIncidentEdges nodeId graph :: Graph Int Int)

    prop "node removed" $ \graph ->
      not (Prelude.null $ nodes graph) ==>
      (`all` nodeIds graph) $ \nodeId ->
        isNothing $ lookupNode nodeId (removeNodeAndIncidentEdges nodeId graph :: Graph Int Int)

  describe "removeEdge" $ do
    prop "preserves validity" $ \graph edgeId ->
      isValid (removeEdge edgeId graph :: Graph Int Int)

    prop "edge removed" $ \graph ->
      not (Prelude.null $ edges graph) ==>
      (`all` edgeIds graph) $ \edgeId ->
        isNothing $ lookupEdge edgeId (removeEdge edgeId graph :: Graph Int Int)

  describe "updateNodePayload" $
    prop "payload changed" $ \graph f ->
      not (Prelude.null $ nodes graph) ==>
      (`all` nodeIds graph) $ \nodeId ->
        let
          graph' = updateNodePayload nodeId graph (apply f) :: Graph Int Int
        in
          (nodeInfo <$> lookupNode nodeId graph')
          == (apply f . nodeInfo <$> lookupNode nodeId graph)

  describe "updateEdgePayload" $
    prop "payload changed" $ \graph f ->
      not (Prelude.null $ edges graph) ==>
      (`all` edgeIds graph) $ \edgeId ->
        let
          graph' = updateEdgePayload edgeId graph (apply f) :: Graph Int Int
        in
          (edgeInfo <$> lookupEdge edgeId graph')
          == (apply f . edgeInfo <$> lookupEdge edgeId graph)


(-->) :: Bool -> Bool -> Bool
False --> _ = True
True --> p = p

infixr 0 -->
