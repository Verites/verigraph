{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Category.LabeledGraph (LabeledGraph, LabeledMorphism) where

import qualified Data.List                          as List

import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary         ()
import           Base.Isomorphic
import           Category.LabeledGraph.Category     ()
import           Category.LabeledGraph.Cocomplete   ()
import           Category.LabeledGraph.FindMorphism ()
import           Data.LabeledGraph
import           Data.LabeledGraph.Morphism

instance {-# OVERLAPS #-} Iso LabeledGraph where
  g1 ~= g2 = not $ List.null (findIsomorphisms g1 g2 :: [LabeledMorphism])
