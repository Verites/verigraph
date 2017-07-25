module Util.Monad where


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
  