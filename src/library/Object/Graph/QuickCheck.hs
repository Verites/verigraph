module Object.Graph.QuickCheck where

import           Object.Graph

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen


instance Arbitrary NodeId where
  arbitrary = NodeId <$> arbitrary


instance Arbitrary EdgeId where
  arbitrary = EdgeId <$> arbitrary


instance (Arbitrary n) => Arbitrary (Node n) where
  arbitrary = Node <$> arbitrary <*> arbitrary
  shrink (Node id payload) = map (Node id) (shrink payload)


instance (Arbitrary e) => Arbitrary (Edge e) where
  arbitrary = Edge <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Edge id src tgt payload) = map (Edge id src tgt) (shrink payload)


instance (Arbitrary n, Arbitrary e) => Arbitrary (Graph n e) where
  arbitrary =
    sized $ \size ->
      let
        nodeSizePeriod = 10
        numNodes = size `div` nodeSizePeriod
        edgeIncrement = (numNodes * numNodes) `div` nodeSizePeriod
        numEdges = (size `mod` nodeSizePeriod) * edgeIncrement
      in randomGraph arbitrary arbitrary numNodes numEdges

  shrink = shrinkGraph


instance Show (NodeContext n e) where
  show _ = "<NodeCtx>"


instance Eq n => Eq (Node n) where
  Node id1 payload1 == Node id2 payload2 = (id1, payload1) == (id2, payload2)


instance Eq e => Eq (Edge e) where
  Edge id1 srcId1 tgtId1 payload1 == Edge id2 srcId2 tgtId2 payload2 =
    (id1, srcId1, tgtId1, payload1) == (id2, srcId2, tgtId2, payload2)


-- | Generate a random node that is a member of the given graph.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ nodes graph) ==>
--   forAll (randomNodeOf graph) $ \node ->
--     someProperty graph node
randomNodeOf :: Graph n e -> Gen (Node n)
randomNodeOf = elements . nodes

-- | Generate a random node identifier that is a member of the given graph.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ nodes graph) ==>
--   forAll (randomNodeIdOf graph) $ \nodeId ->
--     someProperty graph nodeId
randomNodeIdOf :: Graph n e -> Gen NodeId
randomNodeIdOf = elements . nodeIds

-- | Generate a random node that is a member of the given graph, along with its context.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ nodes graph) ==>
--   forAll (randomNodeInContextOf graph) $ \(node, ctx) ->
--     someProperty graph node ctx
randomNodeInContextOf :: Graph n e -> Gen (NodeInContext n e)
randomNodeInContextOf = elements . nodesInContext

-- | Generate a random edge that is a member of the given graph.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ edges graph) ==>
--   forAll (randomEdgeOf graph) $ \edge ->
--     someProperty graph edge
randomEdgeOf :: Graph n e -> Gen (Edge e)
randomEdgeOf = elements . edges

-- | Generate a random edge identifier that is a member of the given graph.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ edges graph) ==>
--   forAll (randomEdgeIdOf graph) $ \edgeId ->
--     someProperty graph edgeId
randomEdgeIdOf :: Graph n e -> Gen EdgeId
randomEdgeIdOf = elements . edgeIds

-- | Generate a random edge that is a member of the given graph, along with its context.
-- Must not be used with empty graphs, so calls to this are often similar to the following:
--
--   not (Prelude.null $ edges graph) ==>
--   forAll (randomEdgeInContextOf graph) $ \(edge, ctx) ->
--     someProperty graph edge ctx
randomEdgeInContextOf :: Graph n e -> Gen (EdgeInContext n e)
randomEdgeInContextOf = elements . edgesInContext

-- | Given generators for payloads, the number of nodes and the number of edges,
-- generates a random graph.
randomGraph :: Gen n -> Gen e -> Int -> Int -> Gen (Graph n e)
randomGraph randomNodePayload randomEdgePayload numNodes numEdges =
  let
    nodeIds = map toEnum [0 .. numNodes - 1]
    edgeIds = map toEnum [0 .. numEdges - 1]
    randomNode n = Node n <$> randomNodePayload
    randomEdge e = Edge e <$> elements nodeIds <*> elements nodeIds <*> randomEdgePayload
  in fromNodesAndEdges <$> mapM randomNode nodeIds <*> mapM randomEdge edgeIds

-- | Produces all immediate shrinks of the given graph. Each shrink step may remove a single node
-- (along with all its incident edges) or a single edge.
shrinkGraph :: Graph n e -> [Graph n e]
shrinkGraph graph =
  [ removeNodeAndIncidentEdges n graph | n <- nodeIds graph ] ++
  [ removeEdge e graph | e <- edgeIds graph ]
