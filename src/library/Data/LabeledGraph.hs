{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
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

import qualified Data.List     as List
import qualified Data.Text     as Text

import qualified Data.EnumMap  as EnumMap
import           Data.Graphs   hiding (empty, fromNodesAndEdges, nodeInfo)
import qualified Data.Graphs   as Graph
import           Data.Variable



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
      showLabel Nothing                    = ""
      showLabel (Just (Variable id [])) = show id
      showLabel (Just (Variable id names)) =
        show id ++ ":" ++ List.intercalate "," (map Text.unpack names)


instance FreeVariables LNode where

  freeVariableMap (Node _ Nothing)  = EnumMap.empty
  freeVariableMap (Node _ (Just v)) = EnumMap.singleton (varId v) v
  {-# INLINE freeVariableMap #-}

  renameVariable idX varY (Node n v) = Node n (if (varId <$> v) == Just idX then Just varY else v)
  {-# INLINE renameVariable #-}

  renameVariables subst (Node n v) =
    Node n (EnumMap.findWithDefault <$> v <*> (varId <$> v) <*> pure subst)
  {-# INLINE renameVariables #-}


instance FreeVariables LabeledGraph where

  freeVariableMap = freeVariableMap . Graph.nodes
  {-# INLINE freeVariableMap #-}

  renameVariable idX varY = Graph.mapNodes (fmap rename . Graph.nodeInfo)
    where rename v = if varId v == idX then varY else v
  {-# INLINE renameVariable #-}

  renameVariables subst = Graph.mapNodes (fmap rename . Graph.nodeInfo)
    where rename v = EnumMap.findWithDefault v (varId v) subst
  {-# INLINE renameVariables #-}
