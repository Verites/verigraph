{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
--
-- A wrapper around Data.IntMap.Strict, allowing general 'Enum' keys. That is,
-- an efficient implementation of maps from enumerable keys to values
-- (dictionaries).
--
-- API of this module is strict in both the keys and the values.
--
-- This module is intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.EnumMap (EnumMapMap)
-- >  import qualified Data.EnumMap as EnumMap
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
--
-- Conversion between 'Int' and the enumerable type is assumed to take constant time.
-----------------------------------------------------------------------------

module Data.EnumMap (
    -- * Strictness properties
    -- $strictness

    -- * Map type
    EnumMap

    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Universal combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet
    , fromSet

    -- ** IntMap
    , toIntMap
    , fromIntMap

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey
    ) where

import           Prelude            hiding (filter, foldl, foldr, lookup, map, null)

import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List          as List
import           Data.Maybe         (fromMaybe)
import           Data.Semigroup

import           Data.EnumSet       (EnumSet)
import qualified Data.EnumSet       as EnumSet

-- $strictness
--
-- This module satisfies the following strictness properties:
--
-- 1. k arguments are evaluated to WHNF;
--
-- 2. Keys and values are evaluated to WHNF before they are stored in
--    the map.
--
-- Here's an example illustrating the first property:
--
-- > delete undefined m  ==  undefined
--
-- Here are some examples that illustrate the second property:
--
-- > map (\ v -> undefined) m  ==  undefined      -- m is not empty
-- > mapKeys (\ k -> undefined) m  ==  undefined  -- m is not empty

-- | A map of enumerable keys @k@ to values @a@.
newtype EnumMap k a = EnumMap { toIntMap :: IntMap a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Semigroup, Monoid)

fromIntMap :: IntMap a -> EnumMap k a
fromIntMap = EnumMap
{-# INLINE fromIntMap #-}

withIntMap :: (IntMap a -> IntMap b) -> EnumMap k a -> EnumMap k b
withIntMap f = fromIntMap . f . toIntMap
{-# INLINE withIntMap #-}

with2IntMaps :: (IntMap a -> IntMap b -> IntMap c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
with2IntMaps f (EnumMap m1) (EnumMap m2) = EnumMap (f m1 m2)
{-# INLINE with2IntMaps #-}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Enum k => EnumMap k a -> k -> a
EnumMap m ! k = m IntMap.! fromEnum k
{-# INLINE (!) #-}

-- | Same as 'difference'.
(\\) :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
(\\) = difference
{-# INLINE (\\) #-}

infixl 9 \\


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.IntMap.null (empty)           == True
-- > Data.IntMap.null (singleton 1 'a') == False
null :: Enum k => EnumMap k a -> Bool
null = IntMap.null . toIntMap
{-# INLINE null #-}

-- | /O(n)/. Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: Enum k => EnumMap k a -> Int
size = IntMap.size . toIntMap

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Enum k => k -> EnumMap k a -> Bool
member k = IntMap.member (fromEnum k) . toIntMap

-- | /O(min(n,W))/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
notMember :: Enum k => k -> EnumMap k a -> Bool
notMember k m = not $ member k m

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: Enum k => k -> EnumMap k a -> Maybe a
lookup k = IntMap.lookup (fromEnum k) . toIntMap

-- | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: Enum k => a -> k -> EnumMap k a -> a
findWithDefault def k = IntMap.findWithDefault def (fromEnum k) . toIntMap

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupLT k =  convertPair . IntMap.lookupLT (fromEnum k) . toIntMap

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupGT k = convertPair . IntMap.lookupGT (fromEnum k) . toIntMap

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupLE k = convertPair . IntMap.lookupLE (fromEnum k) . toIntMap

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupGE k = convertPair . IntMap.lookupGE (fromEnum k) . toIntMap

convertPair :: Enum k => Maybe (Int, a) -> Maybe (k, a)
convertPair Nothing       = Nothing
convertPair (Just (k, a)) = Just (toEnum k, a)
{-# INLINE convertPair #-}

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: Enum k => k -> a -> EnumMap k a
singleton k x = EnumMap $ IntMap.singleton (fromEnum k) x
{-# INLINE singleton #-}

-- | /O(1)/. A map with no elements.
empty :: Enum k => EnumMap k a
empty = EnumMap IntMap.empty
{-# INLINE empty #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert a new k\/value pair in the map.
-- If the k is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
insert :: Enum k => k -> a -> EnumMap k a -> EnumMap k a
insert k x = withIntMap $ IntMap.insert (fromEnum k) x
{-# INLINE insert #-}

-- right-biased insertion, used by 'union'
-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f k value mp@
-- will insert the pair (k, value) into @mp@ if k does
-- not exist in the map. If the k does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: Enum k => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith f k x = withIntMap $ IntMap.insertWith f (fromEnum k) x
{-# INLINE insertWith #-}

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWithKey' f k value mp@
-- will insert the pair (k, value) into @mp@ if k does
-- not exist in the map. If the k does exist, the function will
-- insert @f k new_value old_value@.
--
-- > let f k new_value old_value = (show k) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
--
-- If the k exists in the map, this function is lazy in @x@ but strict
-- in the result of @f@.
insertWithKey :: Enum k => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k x = withIntMap $ IntMap.insertWithKey (f . toEnum) (fromEnum k) x
{-# INLINE insertWithKey #-}

-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f k new_value old_value = (show k) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
insertLookupWithKey :: Enum k => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k x (EnumMap m) = (val, EnumMap m')
  where (val, m') = IntMap.insertLookupWithKey (f . toEnum) (fromEnum k) x m
{-# INLINE insertLookupWithKey #-}


{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty
delete :: Enum k => k -> EnumMap k a -> EnumMap k a
delete k = withIntMap $ IntMap.delete (fromEnum k)

-- | /O(min(n,W))/. Adjust a value at a specific k. When the k is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
adjust :: Enum k => (a -> a) -> k -> EnumMap k a -> EnumMap k a
adjust f k = withIntMap $ IntMap.adjust f (fromEnum k)
{-# INLINE adjust #-}

-- | /O(min(n,W))/. Adjust a value at a specific k. When the k is not
-- a member of the map, the original map is returned.
--
-- > let f k x = (show k) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
adjustWithKey :: Enum k => (k -> a -> a) -> k -> EnumMap k a -> EnumMap k a
adjustWithKey f k = withIntMap $ IntMap.adjustWithKey (f . toEnum) (fromEnum k)
{-# INLINE adjustWithKey #-}

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the k @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
update :: Enum k => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k = withIntMap $ IntMap.update f (fromEnum k)
{-# INLINE update #-}

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the k @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: Enum k => (k -> a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
updateWithKey f k = withIntMap $ IntMap.updateWithKey (f . toEnum) (fromEnum k)
{-# INLINE updateWithKey #-}

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original k value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: Enum k => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a,EnumMap k a)
updateLookupWithKey f k (EnumMap m) = (val, EnumMap m')
  where (val, m') = IntMap.updateLookupWithKey (f . toEnum) (fromEnum k) m
{-# INLINE updateLookupWithKey #-}

-- | /O(min(n,W))/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: Enum k => (Maybe v -> Maybe v) -> k -> EnumMap k v -> EnumMap k v
alter f k = withIntMap $ IntMap.alter f (fromEnum k)
{-# INLINE alter #-}


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: Enum k => [EnumMap k a] -> EnumMap k a
unions = List.foldl' union empty

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: Enum k => EnumMap k a -> EnumMap k a -> EnumMap k a
union = with2IntMaps IntMap.union

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: Enum k => (a->a->a) -> [EnumMap k a] -> EnumMap k a
unionsWith f = List.foldr (unionWith f) empty
{-# INLINE unionsWith #-}

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: Enum k => (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f = with2IntMaps (IntMap.unionWith f)
{-# INLINE unionWith #-}

-- | /O(n+m)/. The union with a combining function.
--
-- > let f k left_value right_value = (show k) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: Enum k => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f = with2IntMaps $ IntMap.unionWithKey (f . toEnum)
{-# INLINE unionWithKey #-}

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
difference :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
difference = with2IntMaps IntMap.difference

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: Enum k => (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f = with2IntMaps $ IntMap.differenceWith f
{-# INLINE differenceWith #-}

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the k and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"
differenceWithKey :: Enum k => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f = with2IntMaps $ IntMap.differenceWithKey (f . toEnum)
{-# INLINE differenceWithKey #-}

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
intersection = with2IntMaps IntMap.intersection

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
intersectionWith :: Enum k => (a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWith f = with2IntMaps $ IntMap.intersectionWith f
{-# INLINE intersectionWith #-}

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: Enum k => (k -> a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWithKey f = with2IntMaps $ IntMap.intersectionWithKey (f . toEnum)
{-# INLINE intersectionWithKey #-}


{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | /O(n+m)/. A high-performance universal combining function. Using
-- 'mergeWithKey', all combining functions can be defined without any loss of
-- efficiency (with exception of 'union', 'difference' and 'intersection',
-- where sharing of some nodes is lost with 'mergeWithKey').
--
-- Please make sure you know what is going on when using 'mergeWithKey',
-- otherwise you can be surprised by unexpected code growth or even
-- corruption of the data structure.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define your custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'IntMap's is created, such that
--
-- * if a k is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the k is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily.  Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
-- @'filterWithKey' f@ could be used for any @f@.
mergeWithKey :: Enum k =>
  (k -> a -> b -> Maybe c) -> (EnumMap k a -> EnumMap k c) -> (EnumMap k b -> EnumMap k c)
  -> EnumMap k a -> EnumMap k b -> EnumMap k c
mergeWithKey combine only1 only2 = with2IntMaps $
  IntMap.mergeWithKey (combine . toEnum) (toIntMap . only1 . fromIntMap) (toIntMap . only2 . fromIntMap)
{-# INLINE mergeWithKey #-}

{--------------------------------------------------------------------
  Min\/Max
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing
maxViewWithKey :: Enum k => EnumMap k a -> Maybe ((k, a), EnumMap k a)
maxViewWithKey (EnumMap m) = convert <$> IntMap.maxViewWithKey m
  where convert ((k, a), m') = ((toEnum k, a), EnumMap m')

-- | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing
minViewWithKey :: Enum k => EnumMap k a -> Maybe ((k, a), EnumMap k a)
minViewWithKey (EnumMap m) = convert <$> IntMap.minViewWithKey m
  where convert ((k, a), m') = ((toEnum k, a), EnumMap m')

-- | /O(min(n,W))/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: Enum k => EnumMap k a -> Maybe (a, EnumMap k a)
maxView (EnumMap m) = convert <$> IntMap.maxView m
  where convert (a, m') = (a, EnumMap m')

-- | /O(min(n,W))/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: Enum k => EnumMap k a -> Maybe (a, EnumMap k a)
minView (EnumMap m) = convert <$> IntMap.minView m
  where convert (a, m') = (a, EnumMap m')

-- | /O(min(n,W))/. Delete and find the maximal element.
deleteFindMax :: Enum k => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map has no maximal element") . maxViewWithKey

-- | /O(min(n,W))/. Delete and find the minimal element.
deleteFindMin :: Enum k => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map has no minimal element") . minViewWithKey

-- | /O(min(n,W))/. The minimal key of the map.
findMin :: Enum k => EnumMap k a -> (k, a)
findMin (EnumMap m) = (toEnum k, a)
  where (k, a) = IntMap.findMin m

-- | /O(min(n,W))/. The maximal key of the map.
findMax :: Enum k => EnumMap k a -> (k, a)
findMax (EnumMap m) = (toEnum k, a)
  where (k, a) = IntMap.findMax m

-- | /O(min(n,W))/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'EnumMap k' was already empty.
deleteMin :: Enum k => EnumMap k a -> EnumMap k a
deleteMin = withIntMap IntMap.deleteMin

-- | /O(min(n,W))/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'EnumMap k' was already empty.
deleteMax :: Enum k => EnumMap k a -> EnumMap k a
deleteMax = withIntMap IntMap.deleteMax

-- | /O(log n)/. Update the value at the minimal k.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMinWithKey :: Enum k => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMinWithKey f = withIntMap $ IntMap.updateMinWithKey (f . toEnum)
{-# INLINE updateMinWithKey #-}

-- | /O(log n)/. Update the value at the maximal k.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMaxWithKey :: Enum k => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey f = withIntMap $ IntMap.updateMaxWithKey (f . toEnum)
{-# INLINE updateMaxWithKey #-}

-- | /O(log n)/. Update the value at the maximal k.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMax :: Enum k => (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMax f = updateMaxWithKey (const f)
{-# INLINE updateMax #-}

-- | /O(log n)/. Update the value at the minimal k.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMin :: Enum k => (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMin f = updateMinWithKey (const f)
{-# INLINE updateMin #-}


{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: Enum k => (a -> b) -> EnumMap k a -> EnumMap k b
map f = withIntMap $ IntMap.map f
{-# INLINE map #-}

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f k x = (show k) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: Enum k => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f = withIntMap $ IntMap.mapWithKey (f . toEnum)
{-# INLINE mapWithKey #-}

-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the k associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: (Enum k, Applicative t) => (k -> a -> t b) -> EnumMap k a -> t (EnumMap k b)
traverseWithKey f (EnumMap m) = EnumMap <$> IntMap.traverseWithKey (f . toEnum) m
{-# INLINE traverseWithKey #-}

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: Enum k => (a -> b -> (a,c)) -> a -> EnumMap k b -> (a,EnumMap k c)
mapAccum f a (EnumMap m) = (a', EnumMap m')
  where (a', m') = IntMap.mapAccum f a m
{-# INLINE mapAccum #-}

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: Enum k => (a -> k -> b -> (a,c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey f a (EnumMap m) = (a', EnumMap m')
  where
    (a', m') = IntMap.mapAccumWithKey f' a m
    f' a = f a . toEnum
{-# INLINE mapAccumWithKey #-}


-- | /O(n)/. The function @'mapAccumR'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: Enum k => (a -> k -> b -> (a,c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumRWithKey f a (EnumMap m) = (a', EnumMap m')
  where
    (a', m') = IntMap.mapAccumRWithKey f' a m
    f' a = f a . toEnum
{-# INLINE mapAccumRWithKey #-}

-- | /O(n*min(n,W))/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
mapKeys :: Enum k => (k->k) -> EnumMap k a -> EnumMap k a
mapKeys f = withIntMap $ IntMap.mapKeys (fromEnum . f . toEnum)

-- | /O(n*min(n,W))/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each k of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new k.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
mapKeysWith :: Enum k => (a -> a -> a) -> (k->k) -> EnumMap k a -> EnumMap k a
mapKeysWith c f = withIntMap $ IntMap.mapKeysWith c (fromEnum . f . toEnum)
{-# INLINE mapKeysWith #-}

-- | /O(n*min(n,W))/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has slightly better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]

mapKeysMonotonic :: Enum k => (k->k) -> EnumMap k a -> EnumMap k a
mapKeysMonotonic f = withIntMap $ IntMap.mapKeysMonotonic (fromEnum . f . toEnum)


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: Enum k => (a -> Bool) -> EnumMap k a -> EnumMap k a
filter p = withIntMap $ IntMap.filter p

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: Enum k => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey predicate = withIntMap $ IntMap.filterWithKey (predicate . toEnum)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: Enum k => (a -> Bool) -> EnumMap k a -> (EnumMap k a,EnumMap k a)
partition p (EnumMap m) = (EnumMap m1, EnumMap m2)
  where (m1, m2) = IntMap.partition p m

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: Enum k => (k -> a -> Bool) -> EnumMap k a -> (EnumMap k a,EnumMap k a)
partitionWithKey p (EnumMap m) = (EnumMap m1, EnumMap m2)
  where (m1, m2) = IntMap.partitionWithKey (p . toEnum) m

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: Enum k => (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)
{-# INLINE mapMaybe #-}

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("k : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "k : 3"
mapMaybeWithKey :: Enum k => (k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey f = withIntMap $ IntMap.mapMaybeWithKey (f . toEnum)
{-# INLINE mapMaybeWithKey #-}

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
mapEither :: Enum k => (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither f = mapEitherWithKey (\_ x -> f x)
{-# INLINE mapEither #-}

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
mapEitherWithKey :: Enum k => (k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey f (EnumMap m) = (EnumMap m1, EnumMap m2)
  where (m1, m2) = IntMap.mapEitherWithKey (f . toEnum) m
{-# INLINE mapEitherWithKey #-}

-- | /O(min(n,W))/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
split :: Enum k => k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split k (EnumMap m) = (EnumMap m1, EnumMap m2)
  where (m1, m2) = IntMap.split (fromEnum k) m

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Enum k => k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup k (EnumMap m) = (EnumMap m1, v, EnumMap m2)
  where (m1, v, m2) = IntMap.splitLookup (fromEnum k) m

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Enum k, Eq a) => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':
  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
 But the following are all 'False':
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOfBy predicate (EnumMap m1) (EnumMap m2) = IntMap.isProperSubmapOfBy predicate m1 m2

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':
  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
 But the following are all 'False':
  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}
isSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isSubmapOfBy predicate (EnumMap m1) (EnumMap m2) = IntMap.isSubmapOfBy predicate m1 m2


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: Enum k => (a -> b -> b) -> b -> EnumMap k a -> b
foldr f z = IntMap.foldr f z . toIntMap
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: Enum k => (a -> b -> b) -> b -> EnumMap k a -> b
foldr' f z = IntMap.foldr' f z . toIntMap
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: Enum k => (a -> b -> a) -> a -> EnumMap k b -> a
foldl f z = IntMap.foldl f z . toIntMap
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: Enum k => (a -> b -> a) -> a -> EnumMap k b -> a
foldl' f z = IntMap.foldl' f z . toIntMap
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: Enum k => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey f z = IntMap.foldrWithKey (f . toEnum) z . toIntMap
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: Enum k => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey' f z = IntMap.foldrWithKey' (f . toEnum) z . toIntMap
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: Enum k => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey f z = IntMap.foldlWithKey f' z . toIntMap
  where f' a = f a . toEnum
        {-# INLINE f' #-}
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: Enum k => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey' f z = IntMap.foldlWithKey' f' z . toIntMap
  where f' a = f a . toEnum
        {-# INLINE f' #-}
{-# INLINE foldlWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: (Monoid m, Enum k) => (k -> a -> m) -> EnumMap k a -> m
foldMapWithKey f = IntMap.foldMapWithKey (f . toEnum) . toIntMap
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
elems :: Enum k => EnumMap k a -> [a]
elems = IntMap.elems . toIntMap

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
keys :: Enum k => EnumMap k a -> [k]
keys = List.map toEnum . IntMap.keys . toIntMap

-- | /O(n)/. An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
assocs :: Enum k => EnumMap k a -> [(k,a)]
assocs = List.map (\(k, v) -> (toEnum k, v)) . IntMap.assocs . toIntMap

-- | /O(n*min(n,W))/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty
keysSet :: Enum k => EnumMap k a -> EnumSet k
keysSet = EnumSet.fromIntSet . IntMap.keysSet . toIntMap

-- | /O(n)/. Build a map from a set of keys and a function which for each k
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.IntSet.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.IntSet.empty == empty
fromSet :: Enum k => (k -> a) -> EnumSet k -> EnumMap k a
fromSet f = EnumMap . IntMap.fromSet (f . toEnum) . EnumSet.toIntSet

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list
-- fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []
toList :: Enum k => EnumMap k a -> [(k,a)]
toList = List.map (\(k, v) -> (toEnum k, v)) . IntMap.toList . toIntMap

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: Enum k => EnumMap k a -> [(k,a)]
toAscList = List.map (\(k, v) -> (toEnum k, v)) . IntMap.toAscList . toIntMap

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]
toDescList :: Enum k => EnumMap k a -> [(k,a)]
toDescList = List.map (\(k, v) -> (toEnum k, v)) . IntMap.toDescList . toIntMap

-- | /O(n*min(n,W))/. Create a map from a list of k\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
fromList :: Enum k => [(k,a)] -> EnumMap k a
fromList = EnumMap . IntMap.fromList . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n*min(n,W))/. Create a map from a list of k\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty
fromListWith :: Enum k => (a -> a -> a) -> [(k,a)] -> EnumMap k a
fromListWith f = EnumMap . IntMap.fromListWith f . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n*min(n,W))/. Build a map from a list of k\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty
fromListWithKey :: Enum k => (k -> a -> a -> a) -> [(k,a)] -> EnumMap k a
fromListWithKey f = EnumMap . IntMap.fromListWithKey (f . toEnum) . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n)/. Build a map from a list of k\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscList :: Enum k => [(k,a)] -> EnumMap k a
fromAscList = EnumMap . IntMap.fromAscList . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n)/. Build a map from a list of k\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
fromAscListWith :: Enum k => (a -> a -> a) -> [(k,a)] -> EnumMap k a
fromAscListWith f = EnumMap . IntMap.fromAscListWith f . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n)/. Build a map from a list of k\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
fromAscListWithKey :: Enum k => (k -> a -> a -> a) -> [(k,a)] -> EnumMap k a
fromAscListWithKey f = EnumMap . IntMap.fromAscListWithKey (f . toEnum) . List.map (\(k, x) -> (fromEnum k, x))

-- | /O(n)/. Build a map from a list of k\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: Enum k => [(k,a)] -> EnumMap k a
fromDistinctAscList = EnumMap . IntMap.fromDistinctAscList . List.map (\(k, x) -> (fromEnum k, x))

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a map into pieces based on the structure
-- of the underlying tree. This function is useful for consuming a
-- map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6::Int] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d'),(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
splitRoot :: Enum k => EnumMap k a -> [EnumMap k a]
splitRoot = List.map EnumMap . IntMap.splitRoot . toIntMap
{-# INLINE splitRoot #-}
