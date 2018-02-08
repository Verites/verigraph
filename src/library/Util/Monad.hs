module Util.Monad
  ( -- * Boolean helpers
    andM
  , orM
  , allM
  , anyM
    -- * List helpers
  , concatMapM
  , mapMaybeM
  , partitionM
  , breakM
  , nubByM
    -- * ListT helpers
  , pickOne
  , pickFromList
  , guardM
    -- * ExceptT helpers
  , tryError
  , mapMCollectErrors
  , mapMCollectErrors_
  , forMCollectErrors
  , forMCollectErrors_
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.List
import           Data.Either          (partitionEithers)
import           Data.Foldable
import           Data.Maybe           (catMaybes)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM mp mq = do
  p <- mp
  if p then mq else return False

orM :: Monad m => m Bool -> m Bool -> m Bool
orM mp mq = do
  p <- mp
  if p then return True else mq

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM test (x:xs) = do
  result <- test x
  if result then allM test xs else return False

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM test (x:xs) = do
  result <- test x
  if result then return True else anyM test xs

pickOne :: Monad m => m [a] -> ListT m a
pickOne = ListT

pickFromList :: Monad m => [a] -> ListT m a
pickFromList = ListT . return

guardM :: (Monad m, MonadTrans t, Monad (t m), Alternative (t m)) => m Bool -> t m ()
guardM test = lift test >>= guard

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f

partitionM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m ([a], [a])
partitionM p = foldrM (select p) ([],[])
{-# INLINE partitionM #-}

select :: Monad m => (a -> m Bool) -> a -> ([a], [a]) -> m ([a], [a])
select p x (ts, fs) = do
  px <- p x
  return $ if px then (x:ts, fs) else (ts, x:fs)

breakM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
breakM _ [] = return ([], [])
breakM p xs@(x:xs') = do
  px <- p x
  if px
    then return ([], xs)
    else do
      (ys, zs) <- breakM p xs'
      return (x:ys, zs)

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq l = nubBy' l []
  where
    nubBy' [] _ = return []
    nubBy' (y:ys) alreadyAdded = do
      hasDuplicate <- anyM (eq y) alreadyAdded
      if hasDuplicate
        then nubBy' ys alreadyAdded
        else (y:) <$> nubBy' ys (y : alreadyAdded)

-- | Given an action that may fail throwing and error, execute it for every item
-- of the list regardless if any action fails, accumulating the results or
-- errors. If any action fails, accumulates all errors and throw them together.
--
-- Be /very careful/ when using this: any side effects that happen for any
-- sucessful item will /not/ be rolled back.
--
-- Unlike 'mapM', this doesn't stop when the first error is thrown. Instead, the
-- action is still executed with the remaining items of the list, accumulating
-- errors instead of results. Then, at the very end, the accumulated errors are
-- thrown.
mapMCollectErrors :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m [b]
mapMCollectErrors fn xs = do
  results <- mapM (tryError . fn) xs
  let (errors, values) = partitionEithers results
  if null errors
    then return values
    else throwError (mconcat errors)

-- | Like 'mapMCollectErrors', but doesn't return the result list.
mapMCollectErrors_ :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m ()
mapMCollectErrors_ fn xs = mapMCollectErrors fn xs >> return ()

-- | Like 'mapMCollectErrors', but with the arguments fliped.
forMCollectErrors :: (Monoid e, MonadError e m) => [a] -> (a -> m b) -> m [b]
forMCollectErrors = flip mapMCollectErrors

-- | Like 'mapMCollectErrors', but has the arguments fliped and doesn't return
-- the result list.
forMCollectErrors_ :: (Monoid e, MonadError e m) => [a] -> (a -> m b) -> m ()
forMCollectErrors_ = flip mapMCollectErrors_

-- | Execute the action and catch any thrown errors.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action =
  (Right <$> action) `catchError` (return . Left)
