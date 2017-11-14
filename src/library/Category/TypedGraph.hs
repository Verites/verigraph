{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Category.TypedGraph (TypedGraph, TypedGraphMorphism) where

import           Abstract.Category.FindMorphism
import           Base.Isomorphic
import           Category.TypedGraph.Adhesive     ()
import           Category.TypedGraph.Category
import           Category.TypedGraph.FindMorphism ()
import           Category.TypedGraph.Finitary     ()
import           Category.TypedGraph.Limit        ()

instance {-# OVERLAPS #-} Iso (TypedGraph n e) where
  g ~= h = not . null $ findIsomorphisms @(TypedGraphMorphism n e) g h
