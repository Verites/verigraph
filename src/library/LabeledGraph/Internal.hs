{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- |
= WARNING

This module is considered __internal__.

It should _not_ be imported anywhere except in other `LabeledGraph` modules.
-}
module LabeledGraph.Internal
  (
  -- * Symbolic Graph type
    LabeledGraph
  , LNode
  , nodeLabel
  , LEdge
  ) where


import           Abstract.Variable
import           Graph.Graph       (Graph, Node (..))
import qualified Graph.Graph       as Graph

import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Text         as Text



-- | A graph with uninterpreted variables as labels.
--
-- Every node and edge is identified by a unique integer. The "namespaces" of nodes and edges are
-- independent, i.e. the same number may be used to identify both a node and an edge within the
-- same graph.
--
-- Equality tests cost /O(v² + e²)/.
type LabeledGraph =
  Graph (Maybe Variable) ()


-- | Nodes from within a labeled graph, also containing their label.
type LNode =
  Graph.Node (Maybe Variable)


nodeLabel :: LNode -> Maybe Variable
nodeLabel =
  Graph.nodeInfo


-- | Edges from within a labeled graph, also containing their source and target.
type LEdge =
  Graph.Edge ()



instance FreeVariables LNode where

  {-# INLINE freeVariableSet #-}
  freeVariableSet (Node _ Nothing)  = Set.empty
  freeVariableSet (Node _ (Just v)) = Set.singleton v


  {-# INLINE renameVariables #-}
  renameVariables subst (Node n v) =
    Node n (Map.findWithDefault <$> v <*> v <*> pure subst)



instance FreeVariables LabeledGraph where

  {-# INLINE freeVariableSet #-}
  freeVariableSet =
    freeVariableSet . Graph.nodes


  {-# INLINE renameVariables #-}
  renameVariables subst =
      Graph.mapNodes (fmap renameVariable . Graph.nodeInfo)
    where
      renameVariable v =
        Map.findWithDefault v v subst



instance {-# OVERLAPPING #-} Show LabeledGraph where

  show (Graph.Graph nodes edges) =
      "Nodes:\n"
      ++ concatMap showNode nodes
      ++ "Edges:\n"
      ++ concatMap showEdge edges
    where
      showNode (n, Graph.Node _ label) =
        "\t" ++ show n ++ " [" ++ showLabel label ++ "]" ++ "\n"

      showLabel Nothing  = ""
      showLabel (Just v) = Text.unpack v

      showEdge (e, Graph.Edge _ src tgt _) =
        "\t" ++ show e ++ " (" ++ show src ++ "->" ++ show tgt ++ ")\n"
