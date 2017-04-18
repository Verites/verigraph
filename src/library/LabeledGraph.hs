{-|
An implementation of graphs labeled with uninterepreted variables.

The implementation is based on the @Graph.Graph@ module. Currently, it only supports untyped graphs
where nodes have a single label.

Operation comments contain the operation time complexity in the Big-O notation
<http://en.wikipedia.org/wiki/Big_O_notation>, denoting by /v/ the number of nodes in a graph, and
by /e/ the number of edges in a graph.
-}
module LabeledGraph
  (
  -- * Labeled Graph type
    LabeledGraph
  , LNode
  , nodeLabel
  , LEdge

  , module Graph.Graph
  ) where

import           Graph.Graph
import           LabeledGraph.Internal
