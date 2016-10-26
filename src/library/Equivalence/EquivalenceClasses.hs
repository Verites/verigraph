module  Equivalence.EquivalenceClasses (

  EquivalenceClass,
  binaryConstruct,
  enaryConstruct,
  getElem,
  getTail,
  maximumDisjointClass


) where

import Data.Set as DS

maximumDisjointClass :: (Ord a) => [a] -> Set (EquivalenceClass a)
maximumDisjointClass l = fromList $ Prelude.map (fromList . (:[])) l

type EquivalenceClass a = Set a

binaryConstruct :: (Ord a, Show a) => Set(a,a) -> Set (EquivalenceClass a) -> Set (EquivalenceClass a)
binaryConstruct toBeGlued toBeX
  | DS.null toBeGlued = toBeX
  | otherwise = binaryConstruct (getTail toBeGlued) (merge (getElem toBeGlued) toBeX)
  where
    merge (e1,e2) s =  mergeEquivalences (e1,e2) s `union` (s `diff` (e1,e2))
    diff s (e1,e2) = if e1 == e2 then
        s `difference` singleton (findEquivalenceClass e1 s)
      else
        s `difference` singleton (findEquivalenceClass e1 s) `difference` singleton (findEquivalenceClass e2 s)

enaryConstruct :: (Ord a, Show a) => Set(Set a) -> Set (EquivalenceClass a) -> Set (EquivalenceClass a)
enaryConstruct toBeGlued toBeX
  | DS.null toBeGlued = toBeX
  | otherwise = enaryConstruct (getTail toBeGlued) (merge (getElem toBeGlued) toBeX)
  where
    merge eq s =  mergeNEquivalences eq s `union`  diffNEquivalences eq s

diffNEquivalences :: (Ord a, Show a) => Set a -> Set(EquivalenceClass a) -> Set(EquivalenceClass a)
diffNEquivalences eq set = actualDiff allSubSets
  where
    actualDiff = DS.foldl difference set
    allSubSets = DS.map newFind eq
    newFind = singleton . (`findEquivalenceClass` set)

mergeNEquivalences :: (Ord a, Show a) => Set a -> Set(EquivalenceClass a) -> Set(EquivalenceClass a)
mergeNEquivalences eq set = singleton $ actualMerge allSubSets
  where
    actualMerge = DS.foldl union DS.empty
    allSubSets = DS.map (`findEquivalenceClass` set) eq

getElem :: Set a -> a
getElem = elemAt 0

getUnitSubset :: Set a -> Set a
getUnitSubset set = singleton (getElem set)

getTail :: (Ord a) => Set a -> Set a
getTail set = set `difference` getUnitSubset set

mergeEquivalences :: (Ord a, Show a) => (a, a) -> Set(EquivalenceClass a) -> Set(EquivalenceClass a)
mergeEquivalences (e1,e2) set = singleton (findEquivalenceClass e1 set `union` findEquivalenceClass e2 set)

-- works only with non-empty sets
findEquivalenceClass :: (Eq a, Show a) => a -> Set(EquivalenceClass a) -> EquivalenceClass a
findEquivalenceClass element set
  | DS.null teste = error $ show element ++ show set
  | otherwise = getElem teste
  where
    teste = DS.filter (element `elem`) set
