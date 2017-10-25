{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Basic building blocks for writing graphs with the Dot syntax.

This contains only the basic building blocks of to Dot syntax,
and it does __not__ provide conventions to print particular
kinds of graphs.
-}
module Image.Dot.StateSpace (stateSpace) where

import qualified Data.IntMap                       as IntMap
import qualified Data.Set                          as Set
import           Data.Text.Prettyprint.Doc         (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc         as PP

import           Abstract.Rewriting.DPO.StateSpace
import qualified Image.Dot.Prettyprint             as Dot

-- | Create a dotfile representation of the given state space, labeling states with their IDs
stateSpace :: StateSpace morph -> Doc ann
stateSpace space = Dot.digraph "stateSpace" $
  (map prettyState . IntMap.toList . states) space
  ++ (map prettyTransition . Set.toList . transitions) space
  where
    prettyState (key, (_, predicates)) = Dot.node (pretty key) [("label", PP.dquotes label)]
      where label = PP.fillSep (pretty key : map pretty predicates)
    prettyTransition (from, to) = Dot.dirEdge (pretty from) (pretty to) []
