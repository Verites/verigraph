{-|
This module contains utilities for annotating values with metadata.

A particular application is tracking the locations of entities in
parsed files, to aid in error reporting.

This module is intended to be imported qualified, as follows.

    import Base.Annotation (Annotated(..), Located)
    import qualified Base.Annotation as Ann
-}
module Base.Annotation
  ( -- * General Annotations
    Annotated(..)
  , drop
  
    -- * Source File Locations
  , Located
  , Location
  , Position(..)
  , reportLocation
  ) where

import Prelude hiding (drop)

data Annotated annotation a = 
  A annotation a
  deriving Show

instance Functor (Annotated info) where
  fmap f (A ann x) = A ann (f x)

instance Eq a => Eq (Annotated info a) where
  (A _ x) == (A _ y) = x == y

drop :: Annotated info a -> a
drop (A _ x) = x

data Position =
  Position { line :: Int, column :: Int }
  deriving (Eq, Show)

instance Ord Position where
  Position l1 c1 <= Position l2 c2 = l1 < l2 || (l1 == l2 && c1 <= c2)

type Location = (Position, FilePath)

type Located a =
  Annotated (Maybe (Position, FilePath)) a

reportLocation :: Maybe Location -> String
reportLocation Nothing = ""
reportLocation (Just (Position l c, path)) =
  " at " ++ path ++ ':' : show l ++ ':' : show c