module Util.EnumMap (inverse, isInjective, lookupMaybe) where

import           Data.Foldable

import           Data.EnumMap  (EnumMap)
import qualified Data.EnumMap  as EnumMap


-- | Given a list of key-value pairs, create a map from each value to its keys. /O(n*max(n,k))/
-- /Assumes/ keys occur only once.
inverse :: Enum v => [(k, v)] -> EnumMap v [k]
inverse = foldl' addInverse EnumMap.empty
  where addInverse m (k, v) = EnumMap.insertWith (++) v [k] m

-- | Given a list of key-value pairs, check if it represents an injective map /O(n*max(n,k))/
-- /Assumes/ keys occur only once.
isInjective :: Enum v => [(k, v)] -> Bool
isInjective = all isEmptyOrSingleton . inverse
  where
    isEmptyOrSingleton []  = True
    isEmptyOrSingleton [_] = True
    isEmptyOrSingleton _   = False

-- | @lookupMaybe x m@ is shorthand for @x >>= (`lookup` m)@
lookupMaybe :: Enum k => Maybe k -> EnumMap k v -> Maybe v
lookupMaybe x m = x >>= (`EnumMap.lookup` m)
