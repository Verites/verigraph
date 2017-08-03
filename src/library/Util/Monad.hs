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
    -- * ListT helpers
  , pickOne
  , pickFromList
  , guardM
  ) where

import Control.Monad
import Control.Monad.List
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Foldable

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