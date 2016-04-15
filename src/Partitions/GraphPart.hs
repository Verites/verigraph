module Partitions.GraphPart (
   Node (..),
   Edge (..),
   Graph (..),
   EqClassGraph,
   getListNode,
   genGraphEqClass
   ) where

-- | A Node with the needed informations for the generating equivalence classes algorithm
data Node = Node {
    ntype    :: Int,
    nname    :: Int,
    nid      :: Int,
    npart    :: Bool,
    ngsource :: String --"Left" xor "Right"
    }

instance Show Node where
  show (Node a b id _ c) = show b ++ ":" ++ show a ++ ":" ++ c ++ " (id:" ++ show id ++")"

instance Eq Node where
  (Node a1 b1 _ _ d1) == (Node a2 b2 _ _ d2) =
    a1 == a2 &&
    b1 == b2 &&
    d1 == d2

-- | An Edge with the needed informations for the generating equivalence classes algorithm
data Edge = Edge {
    etype    :: Int,
    label    :: Int,
    eid      :: Int,
    source   :: Node,
    target   :: Node,
    epart    :: Bool,
    egsource :: String --"Left" xor "Right"
    } deriving (Eq)

instance Show Edge where
  show (Edge t a id (Node _ b2 _ _ _) (Node _ c2 _ _ _) _ s) = show a ++ ":" ++ show t ++ "(" ++ show b2 ++ "->" ++ show c2 ++ ")" ++ s ++ " (id:" ++ show id ++")"

-- | Graph for the generating equivalence classes algorithm
data Graph = Graph {
    nodes :: [Node],
    edges :: [Edge]
    } deriving (Show, Eq)

-- | An equivalence class of a 'Graph'

-- | TODO: use Data.Set?
type EqClassGraph = ([[Node]],[[Edge]])

{-partitions-}

replace :: Eq t => [t] -> [t] -> [[t]] -> [[t]]
replace _ _ [] = []
replace old new (l:ls)
  | l == old  = new : ls
  | otherwise = l : replace old new ls

h :: Eq t => t -> [[t]] -> [[[t]]]
h x list = ([x]:list) : map (\l -> replace l (x:l) list) list
--h x list = ([[x]:list]) ++ map (\l -> (x:l):(list\\[l])) list

-- dont call g _ [[[]]]
g :: Eq t => t -> [[[t]]] -> [[[t]]]
g x = concatMap (h x)

-- | Creates equivalence classes of [t]
partitions :: Eq t => [t] -> [[[t]]]
partitions [] = [[]]
partitions [a] = [[[a]]]
partitions a = foldr g [[[head a]]] (tail a)

-- | Checks if two nodes are in the same equivalence class
checkNode :: Node -> Node -> Bool
checkNode (Node type1 _ _ npart1 _) (Node type2 _ _ npart2 _) =
  type1 == type2 &&
  npart1 && npart2

-- | Checks if two edges are in the same equivalence class
checkST :: [[[Node]]] -> Edge -> Edge -> Bool
checkST nodes (Edge type1 _ _ s1 t1 epart1 _) (Edge type2 _ _ s2 t2 epart2 _) = exp1 && exp2 && exp3
    where
        exp1 = epart1 && epart2 && type1 == type2
        exp2 = ntype s1 == ntype s2 && ntype t1 == ntype t2
        exp3 = (getIdx (nameAndSrc s1) l1 == getIdx (nameAndSrc s2) l1) &&
               (getIdx (nameAndSrc t1) l2 == getIdx (nameAndSrc t2) l2)
        l1   = findTypeList (ntype s1) nodes
        l2   = findTypeList (ntype t1) nodes
        nameAndSrc node = (nname node, ngsource node)

-- | Adds elements in their eq class, creates a new if does not exists
insr :: (a -> Bool) -> a -> [[a]] -> [[a]]
insr _ e []     = [[e]]
insr f e (x:xs) = if f (head x) then (e:x):xs else x:insr f e xs

-- | Separates elements by his eq class
partBy :: (a -> a -> Bool) -> [a] -> [[a]]
partBy _ [] = [[]]
partBy f l = foldr (\a -> insr (f a) a) [[head l]] (tail l)

-- | Generates all equivalence class graphs
genGraphEqClass :: Graph -> [EqClassGraph]
genGraphEqClass gra = [(concat a, concat b) |
                   a <- mapM partitions (partBy checkNode (nodes gra)),
                   b <- mapM partitions (partBy (checkST a) (edges gra))]

-- | Returns the index of @a@ in [[Node]]
getIdx :: (Int,String) -> [[Node]] -> Int
getIdx p@(name,source) (x:xs) = if any (\node -> nname node == name && ngsource node == source) x then 0 else 1 + (getIdx p xs)
getIdx _ [] = error "error when generating overlapping pairs (getInd)"

-- | Returns the list which Node is in [[Node]]
getListNode :: (Int,String) -> [[Node]] -> [Node]
getListNode p@(name,source) (x:xs) = if any (\node -> nname node == name && ngsource node == source) x then x else getListNode p xs
getListNode _ [] = error "error when generating overlapping pairs (getListNode)"

-- | Returns the list that contains elements with @t@ type
findTypeList :: Int -> [[[Node]]] -> [[Node]]
findTypeList t (x:xs) = if ntype (head $ head x) == t then x else findTypeList t xs
findTypeList _ [] = error "error when generating overlapping pairs (findTypeList)"
