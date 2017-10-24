{- | Difference lists: a data structure for /O(1)/ append on lists, also adapted
for /O(1)/ test if it is empty.
-}
module Data.DList
  ( DList

    -- * Construction
  , empty
  , singleton
  , cons
  , snoc
  , append
    -- * Query
  , null
    -- * Conversion
    -- ** Lists
  , fromList
  , toList
  , apply
  ) where

import           Prelude hiding (null)


-- | A difference list is a function that takes a list and prepends some
-- elements. It allows /O(1)/ append and snoc operations.
--
-- This is a modified version where the empty list is distinguished from other
-- difference lists, allowing /O(1)/ checks if the list is empty.
newtype DList a = DList { unDList :: Maybe ([a] -> [a]) }

instance Monoid (DList a) where
  mempty = empty

  mappend (DList Nothing) dl                    = dl
  mappend dl (DList Nothing)                    = dl
  mappend (DList (Just dl1)) (DList (Just dl2)) = DList (Just $ dl1 . dl2)

-- | A dlist containing no elements. /O(1)/.
empty :: DList a
empty = DList Nothing

-- | A dlist with the single given element. /O(1)/.
singleton :: a -> DList a
singleton a = DList $ Just (a:)

-- | Add an element to the start of the dlist. /O(1)/.
cons :: a -> DList a -> DList a
cons a = mappend (singleton a)

-- | Add an element to the end of the dlist. /O(1)/.
snoc :: DList a -> a -> DList a
snoc dl = mappend dl . singleton

-- | Append the given dlists. /O(1)/.
append :: DList a -> DList a -> DList a
append (DList Nothing) dl                    = dl
append dl (DList Nothing)                    = dl
append (DList (Just dl1)) (DList (Just dl2)) = DList (Just $ dl1 . dl2)

-- | Check if the given dlist is empty. /O(1)/.
null :: DList a -> Bool
null (DList Nothing)  = True
null (DList (Just _)) = False

-- | Convert the given list into a dlist. /O(1)/.
fromList :: [a] -> DList a
fromList [] = DList Nothing
fromList xs = DList $ Just (xs++)

-- | Convert the given dlist into a list. /O(n)/.
toList :: DList a -> [a]
toList dl = apply dl []

-- | Prepend the contents of the dlist onto a list. /O(n)/.
apply :: DList a -> [a] -> [a]
apply (DList Nothing)   = id
apply (DList (Just dl)) = dl

