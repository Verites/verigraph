{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
An implementation of graphs labeled with uninterepreted variables.

The implementation is based on the @Graph.Graph@ module. Currently, it only supports untyped graphs
where nodes have a single label.

Operation comments contain the operation time complexity in the Big-O notation
<http://en.wikipedia.org/wiki/Big_O_notation>, denoting by /v/ the number of nodes in a graph, and
by /e/ the number of edges in a graph.
-}
module Data.LabeledGraph
  (
  -- * Symbolic Graph type
    LabeledGraph
  , LNode
  , nodeLabel
  , LEdge

  -- * Construction
  , empty
  , fromNodesAndEdges

  -- * Graph API
  -- $graph-api
  , module Data.Graphs
  ) where


import           Data.Graphs   hiding (empty, fromNodesAndEdges, nodeInfo)
import qualified Data.Graphs   as Graph
import           Data.Variable

import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text     as Text


-- | A graph with uninterpreted variables as labels.
--
-- Every node and edge is identified by a unique integer. The "namespaces" of nodes and edges are
-- independent, i.e. the same number may be used to identify both a node and an edge within the same
-- graph.
--
-- Equality tests cost /O(v² + e²)/.
type LabeledGraph = Graph (Maybe Variable) ()

-- | Nodes from within a labeled graph, also containing their label.
type LNode = Graph.Node (Maybe Variable)

nodeLabel :: LNode -> Maybe Variable
nodeLabel = Graph.nodeInfo

-- | Edges from within a labeled graph, also containing their source and target.
type LEdge = Graph.Edge ()


-- | Empty graph, with no nodes or edges. /O(1)/.
empty :: LabeledGraph
empty = Graph.empty

-- | Build a graph from lists of nodes and edges. Edges with undefined source or target are ignored
-- and omitted from the resulting graph. /O(v + e*v)/
fromNodesAndEdges :: [LNode] -> [LEdge] -> LabeledGraph
fromNodesAndEdges = Graph.fromNodesAndEdges


-- $graph-api
-- Since labeled graphs are just 'Graph's with particular labels, the regular graph API may be  used
-- with 'LabeledGraph's. The relevant functions are reexported by this module, so there is no need
-- to import `Data.Graphs` along with `Data.LabeledGraph`.


instance {-# OVERLAPPING #-} Show LabeledGraph where

  show (Graph.Graph nodes edges) = concat
    [ "Nodes:\n"
    , concatMap showNode nodes
    , "Edges:\n"
    , concatMap showEdge edges ]
    where
      showNode (n, Graph.Node _ label) = "\t" ++ show n ++ " [" ++ showLabel label ++ "]" ++ "\n"
      showEdge (e, Graph.Edge _ src tgt _) =
        "\t" ++ show e ++ " (" ++ show src ++ "->" ++ show tgt ++ ")\n"
      showLabel Nothing  = ""
      showLabel (Just v) = Text.unpack v


instance FreeVariables LNode where

  freeVariableSet (Node _ Nothing)  = Set.empty
  freeVariableSet (Node _ (Just v)) = Set.singleton v
  {-# INLINE freeVariableSet #-}

  renameVariable x y (Node n v) = Node n (if v == Just x then Just y else v)
  {-# INLINE renameVariable #-}

  renameVariables subst (Node n v) = Node n (Map.findWithDefault <$> v <*> v <*> pure subst)
  {-# INLINE renameVariables #-}


instance FreeVariables LabeledGraph where

  freeVariableSet = freeVariableSet . Graph.nodes
  {-# INLINE freeVariableSet #-}

  renameVariable x y = Graph.mapNodes (fmap rename . Graph.nodeInfo)
    where rename v = if v == x then y else v
  {-# INLINE renameVariable #-}

  renameVariables subst = Graph.mapNodes (fmap rename . Graph.nodeInfo)
    where rename v = Map.findWithDefault v v subst
  {-# INLINE renameVariables #-}
