{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.LabeledGraph.QuickCheck where


import           Data.Foldable
import qualified Data.Text              as Text
import           Test.QuickCheck


import           Data.Graphs
import qualified Data.Graphs.QuickCheck as Graph
import           Data.LabeledGraph
import           Data.Variable


instance {-# OVERLAPPING #-} Arbitrary LabeledGraph where
  arbitrary = sized randomSizedGraph
  shrink = Graph.shrinkGraph

-- | Given a number of nodes, the number of edges a the maximum number of variables, generates a
-- random labeled graph.
randomGraph :: Int -> Int -> Int -> Gen LabeledGraph
randomGraph numNodes numEdges maxVariables =
  let
    variables = map (Text.pack . ('x':) . show) [0 .. maxVariables - 1]
    randomLabel
       | maxVariables > 0 = oneof [pure Nothing, Just <$> elements variables]
       | otherwise = pure Nothing
  in
    Graph.randomGraph randomLabel arbitrary numNodes numEdges

-- | Generates a random labeled graph with a given size parameter.
--
-- The number of nodes and edges is defined as in 'Graph.randomSizedGraph'.
-- The number of variables is defined by @numVars = numNodes * 1.2@, which makes it
-- a little less likely for different nodes to have the same label.
randomSizedGraph :: Int -> Gen LabeledGraph
randomSizedGraph size = do
  (numNodes, numEdges) <- Graph.numNodesAndEdgesFor size
  varFactor <- choose (0.8, 1.2 :: Double)
  randomGraph numNodes numEdges (round $ fromIntegral numNodes * varFactor)


-- | Generates a random labeled graph that is smaller or equal in size to the given one.
-- That is, the number of nodes, edges and variables of the generated graph will be at most
-- the same as the given graph.
randomGraphSmallerThan :: LabeledGraph -> Gen LabeledGraph
randomGraphSmallerThan graph = do
  numNodes <- choose (0, length (nodes graph))
  numEdges <- choose (0, length (edges graph))
  maxVariables <- choose (0, length (freeVariablesOf graph))
  randomGraph numNodes numEdges maxVariables


-- | Generates a random subgraph of the given graph.
randomSubgraphOf :: LabeledGraph -> Gen LabeledGraph
randomSubgraphOf graph = do
  deletedNodeIds <- sublistOf =<< shuffle (nodeIds graph)
  let graph' = foldl' (flip removeNodeAndIncidentEdges) graph deletedNodeIds
  deletedEdgeIds <- sublistOf =<< shuffle (edgeIds graph)
  let graph'' = foldl' (flip removeEdge) graph' deletedEdgeIds
  return graph''
