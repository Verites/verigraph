module Equivalence.EquivalenceClasses
  ( EquivalenceClass
  , Partition
  , discretePartition
  , mergePairs
  , mergeSets
  , getElem
  , getTail
  , tsort
  ) where

import           Data.Foldable (find, foldl')
import           Data.Set      (Set)
import qualified Data.Set      as Set


-- | An equivalence class is just a set of elements.
type EquivalenceClass a = Set a

-- | A partition is a set of disjoint equivalence classes.
type Partition a = Set (EquivalenceClass a)

-- | Create the discrete partition of the given set, i.e. the most disjoint partition. In this
-- partition, each element has its own equivalence class.
discretePartition :: (Ord a) => [a] -> Partition a
discretePartition = Set.fromList . map Set.singleton

-- | Given a list of pairs of elements that should belong to the same equivalence class, "collapse"
-- the necessary equivalence classes of the partition.
mergePairs :: (Ord a, Show a) => [(a, a)] -> Partition a -> Partition a
mergePairs toBeGlued partition = foldl' (flip merge) partition toBeGlued
  where
    merge (e1, e2) s = mergeEquivalences (e1, e2) s `Set.union` (s `diff` (e1,e2))
    diff s (e1, e2) =
      if e1 == e2 then
        Set.delete (findEquivalenceClass e1 s) s
      else
        Set.delete (findEquivalenceClass e1 s) . Set.delete (findEquivalenceClass e2 s) $ s

-- | Given a list of sets of elements that should belong to the same equivalence class, "collapse"
-- the necessary equivalence classes of the partition.
mergeSets :: (Ord a, Show a) => [Set a] -> Partition a -> Partition a
mergeSets toBeGlued partition = foldl' (flip merge) partition toBeGlued
  where
    merge eq s = mergeNEquivalences eq s `Set.union` diffNEquivalences eq s

diffNEquivalences :: (Ord a, Show a) => Set a -> Partition a -> Partition a
diffNEquivalences eq set = actualDiff allSubSets
  where
    actualDiff = Set.foldl Set.difference set
    allSubSets = Set.map newFind eq
    newFind = Set.singleton . (`findEquivalenceClass` set)

mergeNEquivalences :: (Ord a, Show a) => Set a -> Partition a -> Partition a
mergeNEquivalences eq set = Set.singleton $ actualMerge allSubSets
  where
    actualMerge = Set.foldl Set.union Set.empty
    allSubSets = Set.map (`findEquivalenceClass` set) eq

getElem :: Set a -> a
getElem = Set.elemAt 0

getUnitSubset :: Set a -> Set a
getUnitSubset set = Set.singleton (getElem set)

getTail :: (Ord a) => Set a -> Set a
getTail set = set `Set.difference` getUnitSubset set

mergeEquivalences :: (Ord a, Show a) => (a, a) -> Partition a -> Partition a
mergeEquivalences (e1,e2) set = Set.singleton (findEquivalenceClass e1 set `Set.union` findEquivalenceClass e2 set)

-- works only with non-empty sets
findEquivalenceClass :: (Eq a, Show a) => a -> Partition a -> EquivalenceClass a
findEquivalenceClass element set
  | Set.null domain = error $ "could not find equivalence class for " ++ show element ++ " in " ++ show set
  | otherwise = getElem domain
  where
    domain = Set.filter (element `elem`) set

type Relation a = Set (a,a)

elementInImage :: Ord a => Relation a -> a -> Relation a
elementInImage rel item = Set.filter ((== item) . snd) rel

elementNotInDomain :: Ord a => Relation a -> a -> Relation a
elementNotInDomain rel item = Set.filter ((/= item) . fst) rel

noIncoming :: Ord a => Relation a -> Set a -> Maybe a
noIncoming rel = find (null . elementInImage rel)

isCyclic :: Ord a => Relation a -> Bool
isCyclic = not . null . until (\rel -> removeOneItem rel == rel) removeOneItem
  where
    removeOneItem rel = maybe rel (elementNotInDomain rel) . noIncoming rel $ relationDomain rel
    relationDomain = Set.map fst

tsort :: Ord a => Relation a -> Set a -> Maybe [a]
tsort rel disconnected =
  let
    items = relationElements rel `Set.union` disconnected
  in if isCyclic rel then Nothing
     else Just $ buildOrdering rel items

relationElements :: Ord a => Relation a -> Set a
relationElements = foldr (\(x,y) -> Set.insert x . Set.insert y) Set.empty

buildOrdering :: Ord a => Relation a -> Set a -> [a]
buildOrdering relation items = maybe [] addToOrderRemoveFromRelation $ noIncoming relation items
  where
    addToOrderRemoveFromRelation i = i : buildOrdering (elementNotInDomain relation i) (Set.delete i items)
