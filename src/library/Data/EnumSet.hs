{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
--
-- A wrapper arount Data.EnumSet a, allowing general 'Enum' keys. That is,
-- an efficient implementation of integer sets.
--
-- This module is intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.EnumSet (EnumSet)
-- >  import qualified Data.EnumSet as EnumSet
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
--
-- Conversion between 'Int' and the enumerable type is assumed to take constant time.
-----------------------------------------------------------------------------

module Data.EnumSet (
            -- * Strictness properties
            -- $strictness

            -- * Set type
              EnumSet          -- instance Eq,Show

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , isSubsetOf
            , isProperSubsetOf

            -- * Construction
            , empty
            , singleton
            , insert
            , delete

            -- * Combine
            , union
            , unions
            , difference
            , intersection

            -- * Filter
            , filter
            , partition
            , split
            , splitMember
            , splitRoot

            -- * Map
            , map

            -- * Folds
            , foldr
            , foldl
            -- ** Strict folds
            , foldr'
            , foldl'

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** IntSet
            , toIntSet
            , fromIntSet

            -- ** List
            , elems
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , toDescList
            , fromAscList
            , fromDistinctAscList
            ) where

import           Data.IntSet    (IntSet)
import qualified Data.IntSet    as IntSet
import qualified Data.List      as List
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup
import           Prelude        hiding (filter, foldl, foldr, map, null)

-- $strictness
--
-- This module satisfies the following strictness property:
--
-- * a arguments are evaluated to WHNF
--
-- Here are some examples that illustrate the property:
--
-- > delete undefined s  ==  undefined

-- | A set of values of the enumerable type @a@.
newtype EnumSet a = EnumSet { toIntSet :: IntSet }
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

fromIntSet :: IntSet -> EnumSet a
fromIntSet = EnumSet
{-# INLINE fromIntSet #-}

withIntSet :: (IntSet -> IntSet) -> EnumSet a -> EnumSet b
withIntSet f (EnumSet a) = EnumSet (f a)
{-# INLINE withIntSet #-}

with2IntSets :: (IntSet -> IntSet -> IntSet) -> EnumSet a -> EnumSet b -> EnumSet c
with2IntSets f (EnumSet a) (EnumSet b) = EnumSet (f a b)
{-# INLINE with2IntSets #-}


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | /O(n+m)/. See 'difference'.
(\\) :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
(\\) = difference
{-# INLINE (\\) #-}

infixl 9 \\

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the set empty?
null :: EnumSet a -> Bool
null = IntSet.null . toIntSet
{-# INLINE null #-}

-- | /O(n)/. Cardinality of the set.
size :: EnumSet a -> Int
size = IntSet.size . toIntSet
{-# INLINE size #-}

-- | /O(min(n,W))/. Is the value a member of the set?
member :: Enum a => a -> EnumSet a -> Bool
member x = IntSet.member (fromEnum x) . toIntSet
{-# INLINE member #-}

-- | /O(min(n,W))/. Is the element not in the set?
notMember :: Enum a => a -> EnumSet a -> Bool
notMember x = not . member x
{-# INLINE notMember #-}

-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3
lookupLT :: Enum a => a -> EnumSet a -> Maybe a
lookupLT x = fmap toEnum . IntSet.lookupLT (fromEnum x) . toIntSet
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing
lookupGT :: Enum a => a -> EnumSet a -> Maybe a
lookupGT x = fmap toEnum . IntSet.lookupGT (fromEnum x) . toIntSet
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5

-- See Note: Local 'go' functions and capturing.
lookupLE :: Enum a => a -> EnumSet a -> Maybe a
lookupLE x = fmap toEnum . IntSet.lookupLE (fromEnum x) . toIntSet
{-# INLINE lookupLE #-}

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing
lookupGE :: Enum a => a -> EnumSet a -> Maybe a
lookupGE x = fmap toEnum . IntSet.lookupGE (fromEnum x) . toIntSet
{-# INLINE lookupGE #-}


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: Enum a => EnumSet a
empty = EnumSet IntSet.empty
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: Enum a => a -> EnumSet a
singleton x = EnumSet $ IntSet.singleton (fromEnum x)
{-# INLINE singleton #-}


{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
-- IntSets.
insert :: Enum a => a -> EnumSet a -> EnumSet a
insert x = withIntSet $ IntSet.insert (fromEnum x)
{-# INLINE insert #-}

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Enum a => a -> EnumSet a -> EnumSet a
delete x = withIntSet $ IntSet.delete (fromEnum x)
{-# INLINE delete #-}

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.
unions :: Enum a => [EnumSet a] -> EnumSet a
unions = List.foldl' union empty
{-# INLINE unions #-}

-- | /O(n+m)/. The union of two sets.
union :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
union = with2IntSets IntSet.union
{-# INLINE union #-}


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two sets.
difference :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
difference = with2IntSets IntSet.difference
{-# INLINE difference #-}


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The intersection of two sets.
intersection :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
intersection = with2IntSets IntSet.intersection
{-# INLINE intersection #-}


{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Enum a => EnumSet a -> EnumSet a -> Bool
isProperSubsetOf (EnumSet s1) (EnumSet s2) = IntSet.isProperSubsetOf s1 s2
{-# INLINE isProperSubsetOf #-}

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Enum a => EnumSet a -> EnumSet a -> Bool
isSubsetOf (EnumSet s1) (EnumSet s2) = IntSet.isSubsetOf s1 s2
{-# INLINE isSubsetOf #-}


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: Enum a => (a -> Bool) -> EnumSet a -> EnumSet a
filter predicate = withIntSet $ IntSet.filter (predicate . toEnum)
{-# INLINE filter #-}

-- | /O(n)/. partition the set according to some predicate.
partition :: Enum a => (a -> Bool) -> EnumSet a -> (EnumSet a,EnumSet a)
partition predicate (EnumSet s) = (EnumSet p1, EnumSet p2)
  where (p1, p2) = IntSet.partition (predicate . toEnum) s
{-# INLINE partition #-}

-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Enum a => a -> EnumSet a -> (EnumSet a,EnumSet a)
split x (EnumSet s) = (EnumSet p1, EnumSet p2)
  where (p1, p2) = IntSet.split (fromEnum x) s
{-# INLINE split #-}

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Enum a => a -> EnumSet a -> (EnumSet a,Bool,EnumSet a)
splitMember x (EnumSet s) = (EnumSet p1, member, EnumSet p2)
  where (p1, member, p2) = IntSet.splitMember (fromEnum x) s
{-# INLINE splitMember #-}


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList [1..120]) == [fromList [1..63],fromList [64..120]]
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two subsets,
--  but you should not depend on this behaviour because it can change in the
--  future without notice. Also, the current version does not continue
--  splitting all the way to individual singleton sets -- it stops at some
--  point.
splitRoot :: Enum a => EnumSet a -> [EnumSet a]
splitRoot = List.map EnumSet . IntSet.splitRoot . toIntSet
{-# INLINE splitRoot #-}

{----------------------------------------------------------------------
  Min/Max
----------------------------------------------------------------------}

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Enum a => EnumSet a -> Maybe (a, EnumSet a)
maxView (EnumSet s) = case IntSet.maxView s of
  Just (val, s') -> Just (toEnum val, EnumSet s')
  Nothing        -> Nothing
{-# INLINE maxView #-}

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Enum a => EnumSet a -> Maybe (a, EnumSet a)
minView (EnumSet s) = case IntSet.minView s of
  Just (val, s') -> Just (toEnum val, EnumSet s')
  Nothing        -> Nothing
{-# INLINE minView #-}

-- | /O(min(n,W))/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: Enum a => EnumSet a -> (a, EnumSet a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty set has no minimal element") . minView
{-# INLINE deleteFindMin #-}

-- | /O(min(n,W))/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Enum a => EnumSet a -> (a, EnumSet a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty set has no maximal element") . maxView
{-# INLINE deleteFindMax #-}

-- | /O(min(n,W))/. The minimal element of the set.
findMin :: Enum a => EnumSet a -> a
findMin = toEnum . IntSet.findMin . toIntSet
{-# INLINE findMin #-}

-- | /O(min(n,W))/. The maximal element of a set.
findMax :: Enum a => EnumSet a -> a
findMax = toEnum . IntSet.findMax . toIntSet
{-# INLINE findMax #-}

-- | /O(min(n,W))/. Delete the minimal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMin :: Enum a => EnumSet a -> EnumSet a
deleteMin = withIntSet IntSet.deleteMin
{-# INLINE deleteMin #-}

-- | /O(min(n,W))/. Delete the maximal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMax :: Enum a => EnumSet a -> EnumSet a
deleteMax = withIntSet IntSet.deleteMax
{-# INLINE deleteMax #-}

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*min(n,W))/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: Enum a => (a -> a) -> EnumSet a -> EnumSet a
map f = withIntSet $ IntSet.map (fromEnum . f . toEnum)
{-# INLINE map #-}

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: Enum a => (a -> b -> b) -> b -> EnumSet a -> b
foldr f b = IntSet.foldr (f . toEnum) b . toIntSet
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: Enum a => (a -> b -> b) -> b -> EnumSet a -> b
foldr' f b = IntSet.foldr' (f . toEnum) b . toIntSet
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: Enum b => (a -> b -> a) -> a -> EnumSet b -> a
foldl f b = IntSet.foldl f' b . toIntSet
  where f' a = f a . toEnum
        {-# INLINE f' #-}
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: Enum b => (a -> b -> a) -> a -> EnumSet b -> a
foldl' f b = IntSet.foldl' f' b . toIntSet
  where f' a = f a . toEnum
        {-# INLINE f' #-}
{-# INLINE foldl' #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: Enum a => EnumSet a -> [a]
elems = List.map toEnum . IntSet.elems . toIntSet
{-# INLINE elems #-}

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: Enum a => EnumSet a -> [a]
toList = toAscList
{-# INLINE toList #-}

-- | /O(n)/. Convert the set to an ascending list of elements. Subject to list
-- fusion.
toAscList :: Enum a => EnumSet a -> [a]
toAscList = List.map toEnum . IntSet.toAscList . toIntSet
{-# INLINE toAscList #-}

-- | /O(n)/. Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: Enum a => EnumSet a -> [a]
toDescList = List.map toEnum . IntSet.toAscList . toIntSet
{-# INLINE toDescList #-}

-- | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: Enum a => [a] -> EnumSet a
fromList = EnumSet . IntSet.fromList . List.map fromEnum
{-# INLINE fromList #-}

-- | /O(n)/. Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Enum a => [a] -> EnumSet a
fromAscList = EnumSet . IntSet.fromAscList . List.map fromEnum
{-# INLINE fromAscList #-}

-- | /O(n)/. Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: Enum a => [a] -> EnumSet a
fromDistinctAscList = EnumSet . IntSet.fromDistinctAscList . List.map fromEnum
{-# INLINE fromDistinctAscList #-}
