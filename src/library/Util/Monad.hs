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

mapMCollectErrors :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m [b]
mapMCollectErrors fn xs = do
  results <- mapM (tryError . fn) xs
  let (errors, values) = partitionEithers results
  if null errors
    then return values
    else throwError (mconcat errors)

mapMCollectErrors_ :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m ()
mapMCollectErrors_ fn xs = mapMCollectErrors fn xs >> return ()

forMCollectErrors :: (Monoid e, MonadError e m) => [a] -> (a -> m b) -> m [b]
forMCollectErrors = flip mapMCollectErrors

forMCollectErrors_ :: (Monoid e, MonadError e m) => [a] -> (a -> m b) -> m ()
forMCollectErrors_ = flip mapMCollectErrors_

tryError :: MonadError e m => m a -> m (Either e a)
tryError action =
  (Right <$> action) `catchError` (return . Left)
