{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
module Util.Closures
( transitiveClosure
, reflexiveClosure
, reflexiveAndTransitiveClosure
, relationDomain
, relationImage
, setToMonad
, monadToSet
)

where

import qualified Data.Set       as S
import           Data.Set.Monad
import           Prelude        hiding (foldr, length, null)

setToMonad :: (Ord a) => S.Set a -> Set a
setToMonad = fromList . S.toList

monadToSet :: (Ord a) => Set a -> S.Set a
monadToSet = S.fromList . toList

type Relation a = Set (a,a)

transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure set
  | null set = set
  | otherwise =
    let
      base = [(a,d) | (a,b) <- set, (c,d) <- set, b == c]
      closureCandidate = base `union` set
      closure = if size closureCandidate == size set then closureCandidate else transitiveClosure closureCandidate
    in closure

reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure set
  | null set = set
  | otherwise =
    let
      base = [ fromList [(a,a),(b,b)] | (a,b) <- set]
      reduction = foldr union empty base
    in set `union` reduction

reflexiveAndTransitiveClosure :: Ord a => Relation a -> Relation a
reflexiveAndTransitiveClosure set
  | null set = set
  | otherwise = reflexiveClosure (transitiveClosure set)

relationDomain :: Ord a => Relation a ->  Set a
relationDomain set
  | null set = empty
  | otherwise = [a | (a,_) <- set]

relationImage :: Ord a => Relation a ->  Set a
relationImage set
  | null set = empty
  | otherwise = [b | (_,b) <- set]
