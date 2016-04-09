module Analysis.GraphPart (
   Node (..),
   Edge (..),
   Graph (..),
   EqClassGraph (..),
   getListNode,
   genEqClass
   ) where

import           Data.List

data Node = Node {
    ntype    :: Int,
    nname    :: Int,
    nid      :: Int,
    ngsource :: String --"Left" xor "Right"
    }

instance Show Node where
  show (Node a b id c) = show b ++ ":" ++ show a ++ ":" ++ c ++ " (id:" ++ show id ++")"

instance Eq Node where
  (Node a1 b1 _ d1) == (Node a2 b2 _ d2) =
    a1 == a2 &&
    b1 == b2 &&
    d1 == d2

data Edge = Edge {
    etype    :: Int,
    label    :: Int,
    eid      :: Int,
    source   :: Node,
    target   :: Node,
    egsource :: String --"Left" xor "Right"
    } deriving (Eq)

instance Show Edge where
  show (Edge t a id (Node b1 b2 _ b3) (Node c1 c2 _ c3) s) = show a ++ ":" ++ show t ++ "(" ++ show b2 ++ "->" ++ show c2 ++ ")" ++ s ++ " (id:" ++ show id ++")"

data Graph = Graph {
    nodes :: [Node],
    edges :: [Edge]
    } deriving (Show, Eq)

type EqClassGraph = ([[Node]],[[Edge]])

{-partitions-}

replace :: Eq t => [t] -> [t] -> [[t]] -> [[t]]
replace old new [] = []
replace old new (l:ls) = if l == old then new:ls else l:(replace old new ls)

h :: Eq t => t -> [[t]] -> [[[t]]]
h x list = ([[x]:list]) ++ map (\l -> replace l (x:l) list) list
--h x list = ([[x]:list]) ++ map (\l -> (x:l):(list\\[l])) list

-- dont call g _ [[[]]]
g :: Eq t => t -> [[[t]]] -> [[[t]]]
g x list = concatMap (h x) list

-- | Creates equivalence classes of [t]
partitions :: Eq t => [t] -> [[[t]]]
partitions [] = [[]]
partitions [a] = [[[a]]]
partitions a = foldr g [[[head a]]] (tail a)

-- | Checks if two nodes are in the same equivalence class
checkNode :: Node -> Node -> Bool
checkNode (Node type1 _ _ _) (Node type2 _ _ _) = type1 == type2

-- | Checks if two edges are in the same equivalence class
checkST :: [[[Node]]] -> Edge -> Edge -> Bool
checkST nodes (Edge type1 _ _ s1 t1 _) (Edge type2 _ _ s2 t2 _) = exp1 && exp2 && exp3
    where
        exp1 = type1 == type2
        exp2 = ntype s1 == ntype s2 && ntype t1 == ntype t2
        exp3 = (getInd l1 s1 0 == getInd l1 s2 0) && (getInd l2 t1 0 == getInd l2 t2 0)
        l1   = findTypeList nodes (ntype s1)
        l2   = findTypeList nodes (ntype t1)

-- | Adds elements in their eq class, creates a new if does not exists
insr :: (a -> Bool) -> a -> [[a]] -> [[a]]
insr f e []     = [[e]]
insr f e (x:xs) = if f (head x) then (e:x):xs else x:insr f e xs

-- | Separates elements by his eq class
partBy :: (a -> a -> Bool) -> [a] -> [[a]]
partBy f [] = [[]]
partBy f l = foldr (\a -> insr (f a) a) [[head l]] (tail l)

genEqClass :: Graph -> [EqClassGraph]
genEqClass gra = [(concat a, concat b) |
                   a <- mapM partitions (partBy checkNode (nodes gra)),
                   b <- mapM partitions (partBy (checkST a) (edges gra))]

-- | Returns the index of @a@ in [[Node]]
getInd :: [[Node]] -> Node -> Int -> Int
getInd (x:xs) a n = if any (\n -> nname n == nname a && ngsource n == ngsource a) x then n else getInd xs a (n+1)
getInd [] _ _ = error "error when generating overlapping pairs (getInd)"

-- | Returns the list which Node is in [[Node]]
getListNode :: [[Node]] -> Node -> [Node]
getListNode (x:xs) a = if any (\n -> nname n == nname a && ngsource n == ngsource a) x then x else getListNode xs a
getListNode [] _ = error "error when generating overlapping pairs (getListNode)"

findTypeList :: [[[Node]]] -> Int -> [[Node]]
findTypeList (x:xs) t = if ntype (head $ head x) == t then x else findTypeList xs t
findTypeList [] _ = error "error when generating overlapping pairs (findTypeList)"
