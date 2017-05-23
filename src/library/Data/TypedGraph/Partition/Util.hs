{-|
Description : Partitions generator util functions.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
-}
module Data.TypedGraph.Partition.Util
  ( nodeNameAndSource
  , getNode
  , getListContainingNode
  , getListContainingEdge
  ) where

import           Data.TypedGraph.Partition.Types

nodeNameAndSource :: Node -> (Bool,Int)
nodeNameAndSource node = (nodeFromLeft node, nodeName node)

-- | Returns the node that @p@ was collapsed in the partitions algorithm.
-- It is used to compare if an edge can be mixed with another.
-- GraphPartitionToVerigraph uses this to discover edges source and target nodes.
getNode :: (Bool,Int) -> [[Node]] -> Node
getNode a b = head (getListContainingNode a b)

-- | Returns the list which Node is in [[Node]]
getListContainingNode :: (Bool,Int) -> [[Node]] -> [Node]
getListContainingNode _          []     = error "error when mounting overlapping pairs (getListContainingNode)"
getListContainingNode p@(side,a) (x:xs) =
  if any (\n -> nodeName n == a && nodeFromLeft n == side) x then x else getListContainingNode p xs

-- | Returns the list which Edge is in [[Edge]]
getListContainingEdge :: (Bool,Int) -> [[Edge]] -> [Edge]
getListContainingEdge _          []     = error "error when mounting overlapping pairs (getListContainingEdge)"
getListContainingEdge p@(side,a) (x:xs) =
  if any (\e -> (label e == a) && (edgeFromLeft e == side)) x then x else getListContainingEdge p xs
