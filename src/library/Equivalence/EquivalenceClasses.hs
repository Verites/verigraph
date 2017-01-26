module  Equivalence.EquivalenceClasses (

  EquivalenceClass,
  binaryConstruct,
  enaryConstruct,
  getElem,
  getTail,
  maximumDisjointClass,
  tsort


) where

import           Data.Foldable (find)
import           Data.Set as DS
import Prelude hiding (filter, null, foldr, foldl)

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
  | DS.null domain = error $ "could not find equivalence class for " ++ show element ++ " in " ++ show set
  | otherwise = getElem domain
  where
    domain = DS.filter (element `elem`) set

type Relation a = Set (a,a)

elementInImage :: Ord a => Relation a -> a -> Relation a
elementInImage rel item = filter ((== item) . snd) rel

elementNotInDomain :: Ord a => Relation a -> a -> Relation a
elementNotInDomain rel item = filter ((/= item) . fst) rel

noIncoming :: Ord a => Relation a -> Set a -> Maybe a
noIncoming rel = find (null . elementInImage rel)

isCyclic :: Ord a => Relation a -> Bool
isCyclic = not . null . until (\rel -> removeOneItem rel == rel) removeOneItem
  where
    removeOneItem rel = maybe rel (elementNotInDomain rel) . noIncoming rel $ relationDomain rel
    relationDomain = DS.map fst

tsort :: Ord a => Relation a -> Maybe [a]
tsort rel =
  let
    items = relationElements rel
  in if isCyclic rel then Nothing
     else Just $ buildOrdering rel items

conditionalTSort :: Ord a => Relation a -> Maybe [a]
conditionalTSort = tsort

relationElements :: Ord a => Relation a -> Set a
relationElements = foldr (\(x,y) -> insert x . insert y) empty

buildOrdering :: Ord a => Relation a -> Set a -> [a]
buildOrdering relation items = maybe [] addToOrderRemoveFromRelation $ noIncoming relation items
  where
    addToOrderRemoveFromRelation i = i : buildOrdering (elementNotInDomain relation i) (delete i items)
