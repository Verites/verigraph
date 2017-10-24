{-|
This module contains utilities for annotating values with metadata.

A particular application is tracking the locations of entities in
parsed files, to aid in error reporting.

This module is intended to be imported qualified, as follows.

    import Base.Annotation (Annotated(..), Located, at)
    import qualified Base.Annotation as Ann
-}
module Base.Annotation
  ( -- * Annotated datatypes
    Annotated(..)
  , drop

    -- * Source dile locations
  , Located
  , at
  , locationOf
  ) where

import           Prelude       hiding (drop)

import           Base.Location
import           Prelude       hiding (drop)

-- | An version of type @a@ annotated with values of type @info@.
--
-- Annotations are ignored by the 'Eq' and 'Ord' instances.
data Annotated info a =
  A info a
  deriving Show

instance Functor (Annotated info) where
  fmap f (A ann x) = A ann (f x)

instance Eq a => Eq (Annotated info a) where
  (A _ x) == (A _ y) = x == y

instance Ord a => Ord (Annotated info a) where
  (A _ x) <= (A _ y) = x <= y

-- | Remove the annotation from the value.
drop :: Annotated info a -> a
drop (A _ x) = x

-- | A version of type @a@ annotated with its location in a source file.
type Located a = Annotated (Maybe Location) a

-- | Helper creating located values.
at :: a -> Location -> Located a
at x loc = A (Just loc) x

-- | Get the location of a located value.
locationOf :: Located a -> Maybe Location
locationOf (A loc _) = loc
