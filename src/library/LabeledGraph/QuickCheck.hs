{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LabeledGraph.QuickCheck where


import           Data.Foldable
import qualified Data.Text             as Text
import           Test.QuickCheck


import           Abstract.Variable
import           Graph.Graph
import qualified Graph.QuickCheck      as Graph
import           LabeledGraph.Internal



instance {-# OVERLAPPING #-} Arbitrary LabeledGraph where

  arbitrary =
    sized randomSizedGraph


  shrink =
    Graph.shrinkGraph


-- | Given a number of nodes, the number of edges a the maximum number of variables, generates a
-- random labeled graph.
randomGraph :: Int -> Int -> Int -> Gen LabeledGraph
randomGraph numNodes numEdges maxVariables =
  let
    variables =
      map (Text.pack . ('x':) . show) [0 .. maxVariables - 1]

    randomLabel =
      if maxVariables > 0 then
        oneof [pure Nothing, Just <$> elements variables]
      else
        pure Nothing
  in
    Graph.randomGraph randomLabel arbitrary numNodes numEdges


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


-- | Generates a random labeled graph with a given size parameter.
--
-- The number of nodes and edges is defined as in 'Graph.randomSizedGraph'.
-- The number of variables is defined by @numVars = numNodes * 1.2@, which makes it
-- a little less likely for different nodes to have the same label.
randomSizedGraph :: Int -> Gen LabeledGraph
randomSizedGraph size =
  let
    (numNodes, numEdges) =
      Graph.numNodesAndEdgesFor size

    numVariables =
      round $ 1.2 * (fromIntegral numNodes :: Double)
  in
    randomGraph numNodes numEdges numVariables
