module Dag (Dag, empty, insertPoset, lowerCover, elemsWithAdj) where

import Control.Monad
import Data.Array.IO
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IntSet
import Data.IORef
import Data.Maybe (fromJust)

data DagNode a = DN { dagVal :: a, greaterThan :: IntSet } 

data Dag a = Dag
  { capacity :: Int
  , sizeRef :: IORef Int
  , maximalRef :: IORef (IntSet) 
  , dagArray :: IOArray Int (Maybe (DagNode a))
  }

empty :: Int -> IO (Dag a)
empty capacity = 
  Dag capacity
    <$> newIORef 0
    <*> newIORef IntSet.empty
    <*> newArray (0, capacity - 1) Nothing

elemsWithAdj :: Dag a -> IO [(a, [a])]
elemsWithAdj dag = do
  size <- readIORef (sizeRef dag)
  forM [0..size - 1] $ \i -> do
    Just (DN x downSet) <- readArray (dagArray dag) i
    downElems <- mapM (fmap (dagVal . fromJust) . readArray (dagArray dag)) (IntSet.toList downSet)
    return (x, downElems) 

insertPoset :: (a -> a -> Maybe Ordering) -> a -> Dag a -> IO ()
insertPoset cmp x dag = do
  (isRepeated, upSet, downSet) <- locateNewElement cmp x dag
  unless isRepeated $ do
    -- Insert into DAG
    newIndex <- readIORef (sizeRef dag)
    modifyIORef (sizeRef dag) (+1)
    writeArray (dagArray dag) newIndex (Just $ DN x downSet)

    -- Insert into maximal or into the down-sets of greater nodes
    if IntSet.null upSet then
      modifyIORef (maximalRef dag) (IntSet.insert newIndex)
    else
      let addToDownSet oldIndex = do
            Just (DN y downSet') <- readArray (dagArray dag) oldIndex
            writeArray (dagArray dag) oldIndex (Just . DN y $ IntSet.insert newIndex downSet') :: IO ()
      in mapM_ addToDownSet (IntSet.toList upSet)

    -- Remove maximal elements that are now covered
    modifyIORef (maximalRef dag) (IntSet.\\ downSet)

locateNewElement :: (a -> a -> Maybe Ordering) -> a -> Dag a -> IO (Bool, IntSet, IntSet)
locateNewElement cmp x dag = do
  maximal <- readIORef (maximalRef dag)
  loop (qFromList $ IntSet.toList maximal) IntSet.empty IntSet.empty maximal
  where
    loop :: Queue Int -> IntSet -> IntSet -> IntSet -> IO (Bool, IntSet, IntSet)
    loop unvisited upSet downSet seen = case takeQ unvisited of
      Nothing -> return (False, upSet, downSet)
      Just (i, unvisited') -> do
        Just (DN y downSetY) <- readArray (dagArray dag) i
        let unseenDownSetY = downSetY IntSet.\\ seen
        case cmp x y of
          Just EQ -> return (True, upSet, downSet)
          Just GT -> loop unvisited' upSet (IntSet.insert i $ IntSet.union downSet downSetY) (IntSet.union seen downSetY)
          Just LT -> loop (appendQ (IntSet.toList unseenDownSetY) unvisited') (IntSet.insert i upSet) downSet (IntSet.union seen downSetY)
          Nothing -> loop (appendQ (IntSet.toList unseenDownSetY) unvisited') upSet downSet (IntSet.union seen downSetY)

cloneDag :: Dag a -> IO (Dag a)
cloneDag dag =
  Dag (capacity dag)
    <$> (newIORef =<< readIORef (sizeRef dag))
    <*> (newIORef =<< readIORef (maximalRef dag))
    <*> mapArray id (dagArray dag)

lowerCover :: Dag a -> IO (Dag a)
lowerCover poset = do
  cover <- cloneDag poset
  size <- readIORef (sizeRef poset)
  forM_ [size-1,size-2..0] $ \k ->
    forM_ [0..size-1] $ \i -> 
      forM_ [0..size-1] $ \j -> do
        Just (DN vI downI) <- readArray (dagArray cover) i 
        Just (DN _ downJ) <- readArray (dagArray cover) j
        Just (DN _ downK) <- readArray (dagArray cover) k
        when (j `IntSet.member` downK && k `IntSet.member` downI) .
          writeArray (dagArray cover) i . Just $ DN vI (IntSet.delete j downI)
  return cover

data Queue a = Q { qfront :: [a], qrear :: [a] }

emptyQ = Q [] []
isEmptyQ (Q f r) = null f && null r

invQ (Q [] r) = Q (reverse r) []
invQ (Q f r) = Q f r

takeQ (Q [] _) = Nothing
takeQ (Q (x:f) r) = Just (x, invQ $ Q f r)

insertQ x (Q f r) = invQ $ Q f (x:r)
appendQ xs (Q f r) = invQ $ Q f (xs++r)
qFromList xs = Q xs []