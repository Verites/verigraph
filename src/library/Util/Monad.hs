module Util.Monad where

import Control.Monad
import Control.Monad.List
import Control.Applicative

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

guardM :: (Monad m, MonadTrans t, Monad (t m), Alternative (t m)) => m Bool -> t m ()
guardM test = lift test >>= guard