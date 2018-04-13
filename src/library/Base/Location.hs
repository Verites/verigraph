{- | Utilities for dealing with the location of parsed entities in their source files.
-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Location
  ( Location(..)
  , Position(..)
  ) where

import           Data.Monoid
import           Data.Text.Prettyprint.Doc (Pretty (..))


-- | Position within a text file.
data Position =
  Position { line :: Int, column :: Int }
  deriving (Eq, Show)

instance Ord Position where
  Position l1 c1 <= Position l2 c2 = l1 < l2 || (l1 == l2 && c1 <= c2)

instance Pretty Position where
  pretty (Position l1 c1) = pretty l1 <> ":" <> pretty c1

data Location = Location
  { sourceFile :: FilePath
  , position   :: Position }
  deriving (Show, Eq, Ord)

instance Pretty Location where
  pretty (Location path pos) = pretty path <> ":" <> pretty pos

