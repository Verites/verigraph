module Partitions.GraphPart (
   Node (..),
   Edge (..),
   Graph,
   EqClassGraph,
   getListNode,
   genGraphEqClass
   ) where

import Data.Maybe (mapMaybe)

-- | A Node with the needed informations for the generating equivalence classes algorithm
data Node = Node {
    ntype    :: Int,
    nname    :: Int,
    nid      :: Int,
    npart    :: Bool,
    ngsource :: String --"Left" xor "Right"
    }

instance Show Node where
  show (Node a b id f c) = show b ++ ":" ++ show a ++ ":" ++ c ++ " (id:" ++ show id ++") {"++ show f ++"}"

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
  show (Edge t a id (Node _ b2 _ _ _) (Node _ c2 _ _ _) f s) =
    show a ++ ":" ++
    show t ++ "(" ++
    show b2 ++ "->" ++
    show c2 ++ ")" ++
    s ++
    " (id:" ++ show id ++") {"++ show f ++"}"

-- | Graph for the generating equivalence classes algorithm
type Graph = ([Node],[Edge])

-- | An equivalence class of a 'Graph'

-- | TODO: use Data.Set?
type EqClassGraph = ([[Node]],[[Edge]])

{-partitions-}

replace :: Eq t => [t] -> [t] -> [[t]] -> [[t]]
replace _ _ [] = []
replace old new (l:ls)
  | l == old  = new : ls
  | otherwise = l : replace old new ls

-- | Checks if two nodes are in the same equivalence class
checkNode :: Node -> [Node] -> Bool
checkNode _ [] = error "checkNode"
checkNode (Node type1 _ _ npart _) l@((Node type2 _ _ _ _):_) =
  (npart || (countNotPartNode l <= 1)) && 
  type1 == type2

countNotPartNode :: [Node] -> Int
countNotPartNode [] = 0
countNotPartNode ((Node _ _ _ part _):xs) = (if part then 0 else 1) + (countNotPartNode xs)

countNotPartEdge :: [Edge] -> Int
countNotPartEdge [] = 0
countNotPartEdge ((Edge _ _ _ _ _ part _):xs) = (if part then 0 else 1) + (countNotPartEdge xs)

-- | Checks if two edges are in the same equivalence class
checkEdge :: [[Node]] -> Edge -> [Edge] -> Bool
checkEdge _ _ [] = error "checkEdge"
checkEdge nodes (Edge type1 _ _ s1 t1 epart _) l@((Edge type2 _ _ s2 t2 _ _):_) = exp1 && exp2 && exp3
  where
    exp1 = (epart || (countNotPartEdge l <= 1)) && type1 == type2
    nameAndSrc node = (nname node, ngsource node)
    l1   = getListNode (nameAndSrc s1) nodes
    l2   = getListNode (nameAndSrc s2) nodes
    exp2 = l1 == l2
    l3   = getListNode (nameAndSrc t1) nodes
    l4   = getListNode (nameAndSrc t2) nodes
    exp3 = l3 == l4

genGraphEqClass :: Graph -> [EqClassGraph]
genGraphEqClass gra = concatMap f a
  where
    nodes = fst
    edges = snd
    a = part checkNode (nodes gra)
    b x = part (checkEdge x) (edges gra)
    f :: [[Node]] -> [([[Node]],[[Edge]])]
    f a = zip (replicate (length (b a)) a) (b a)

part :: Eq a => (a -> [a] -> Bool) -> [a] -> [[[a]]]
part f l = if l == [] then [[]] else bt f l []

bt :: Eq a => (a -> [a] -> Bool) -> [a] -> [[a]] -> [[[a]]]
bt f toAdd [] = bt f (init toAdd) [[last toAdd]]
bt _ [] actual = [actual]
bt f toAdd actual = (bt f initAdd ([ad]:actual)) ++ 
                    concat
                      (mapMaybe
                        (\n -> if f ad n
                                 then
                                   Just (bt f initAdd (replace n (ad:n) actual))
                                 else
                                   Nothing)
                        actual)
  where
    initAdd = init toAdd
    ad = last toAdd

-- | Returns the list which Node is in [[Node]]
getListNode :: (Int,String) -> [[Node]] -> [Node]
getListNode p@(name,source) (x:xs) = if any (\node -> nname node == name && ngsource node == source) x then x else getListNode p xs
getListNode _ [] = error "error when generating overlapping pairs (getListNode)"


{- TESTS
n1 = Node 1 1 1 True "Left"
n2 = Node 1 2 2 True "Left"
n3 = Node 1 3 3 False "Right"
n4 = Node 1 4 4 False "Right"
l = [n1,n2]
e1 = Edge 1 1 1 n1 n2 False "Left"
e2 = Edge 1 1 1 n3 n4 False "Right"
o = [e1,e1]-}

{- OLD CODE,
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

-- | Generates all equivalence class graphs
genGraphEqClass :: Graph -> [EqClassGraph]
genGraphEqClass gra = [(concat a, concat b) |
                   a <- mapM partitions (partBy checkNode (nodes gra)),
                   b <- mapM partitions (partBy (checkST a) (edges gra))]

-- | Adds elements in their eq class, creates a new if does not exists
insr :: (a -> Bool) -> a -> [[a]] -> [[a]]
insr _ e []     = [[e]]
insr f e (x:xs) = if f (head x) then (e:x):xs else x:insr f e xs

-- | Separates elements by his eq class
partBy :: (a -> a -> Bool) -> [a] -> [[a]]
partBy _ [] = [[]]
partBy f l = foldr (\a -> insr (f a) a) [[head l]] (tail l)-}
