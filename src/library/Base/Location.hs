{- | Utilities for dealing with the location of parsed entities in their source files.
-}
module Base.Location
  ( Location(..)
  , Position(..)
  , showLocation
  , reportLocation
  ) where

-- | Position within a text file.
data Position =
  Position { line :: Int, column :: Int }
  deriving (Eq, Show)

instance Ord Position where
  Position l1 c1 <= Position l2 c2 = l1 < l2 || (l1 == l2 && c1 <= c2)

data Location = Location
  { sourceFile :: FilePath
  , position   :: Position }
  deriving (Show, Eq, Ord)

-- | Create a human- and machine-readable string expressing the given location.
showLocation :: Location -> String
showLocation (Location path (Position l c)) =
  path ++ ':' : show l ++ ':' : show c

-- | Create a human- and machine-readable string expressing the given location,
-- which can be used to qualify a statement. If there is no location, returns an empty string.
--
-- >>> reportLocation $ Just (Location "foo/bar.baz" (Position 42 12))
-- " at foo/bar.baz:42:12"
--
-- >>> reportLocation Nothing
-- ""
reportLocation :: Maybe Location -> String
reportLocation Nothing    = ""
reportLocation (Just loc) = " at " ++ showLocation loc
