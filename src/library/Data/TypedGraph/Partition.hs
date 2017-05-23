{-|
Description : Typed graph partitions generator.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>

A partition of a set is a grouping of the set's elements into non-empty subsets,
in such a way that every element is included in one and only one of the subsets.
A partition of a typed graph is an extension of the sets, where nodes can
be combined by their types and edges by their types and source/target nodes.
-}
module Data.TypedGraph.Partition
  ( generateGraphPartitions
  ) where

import           Data.TypedGraph.Partition.Generator
import           Data.TypedGraph.Partition.Types
import           Data.TypedGraph.Partition.Util

-- | Checks if a node belongs to an equivalence class
nodeBelongsToEquivalenceClass :: Node -> [Node] -> Bool
nodeBelongsToEquivalenceClass _ [] = error "error 'nodeBelongsToEquivalenceClass' in GraphPartition"
nodeBelongsToEquivalenceClass (Node type1 _ _ injectiveNode side1) l@(Node type2 _ _ _ _ : _) =
  type1 == type2 && if injectiveNode then not thereIsAnotherSameSideNode else not thereIsAnotherInjectiveNode
  where
    thereIsAnotherInjectiveNode = any (\(Node _ _ _ injectiveNode side2) -> injectiveNode && side1 == side2) l
    thereIsAnotherSameSideNode = any (\(Node _ _ _ _ side2) -> side1 == side2) l

-- | Checks if two edges are in the same equivalence class
-- Needs @nodes@ to know if a source or target was collapsed
edgeBelongsToEquivalenceClass :: [[Node]] -> Edge -> [Edge] -> Bool
edgeBelongsToEquivalenceClass _ _ [] = error "error 'edgeBelongsToEquivalenceClass' in GraphPartition"
edgeBelongsToEquivalenceClass nodes (Edge type1 _ _ s1 t1 injectiveEdge side) l@(Edge type2 _ _ s2 t2 _ _ : _) =
  equalTypes && canBeAddedToClass && equivalentSources && equivalentTargets
  where
    equalTypes = type1 == type2
    canBeAddedToClass = if injectiveEdge then not thereIsAnotherSameSideEdge else not thereIsAnotherInjectiveEdge
    thereIsAnotherInjectiveEdge = any (\(Edge _ _ _ _ _ inj side2) -> inj && side == side2) l
    thereIsAnotherSameSideEdge = any (\(Edge _ _ _ _ _ _ side2) -> side == side2) l
    source1   = getNode (nodeNameAndSource s1) nodes
    source2   = getNode (nodeNameAndSource s2) nodes
    equivalentSources = source1 == source2
    source3   = getNode (nodeNameAndSource t1) nodes
    source4   = getNode (nodeNameAndSource t2) nodes
    equivalentTargets = source3 == source4

-- | Runs generator of partitions for nodes, and after for edges according to the nodes generated
generateGraphPartitions :: Graph -> [GraphPartition]
generateGraphPartitions (nodes,edges) = concatMap runEdges nodeEquivalences
  where
    nodeEquivalences = generatePartitions nodeBelongsToEquivalenceClass nodes
    edgeEquivalences x = generatePartitions (edgeBelongsToEquivalenceClass x) edges

    runEdges nodeEqs =
      zip (replicate (length (edgeEquivalences nodeEqs)) nodeEqs) (edgeEquivalences nodeEqs)
