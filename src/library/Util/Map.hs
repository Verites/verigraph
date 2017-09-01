module Util.Map (inverse, lookupMaybe) where

import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | Given a list of key-value pairs, create a map from each value to its keys. /O(n*log(n))/
inverse :: Ord v => [(k, v)] -> Map v [k]
inverse = foldl' addInverse Map.empty
  where addInverse m (k, v) = Map.insertWith (++) v [k] m

-- | @lookupMaybe x m@ is shorthand for @x >>= (`lookup` m)@
lookupMaybe :: Ord k => Maybe k -> Map k v -> Maybe v
lookupMaybe x m = x >>= (`Map.lookup` m)
