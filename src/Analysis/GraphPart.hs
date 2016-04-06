module Analysis.GraphPart (
   Node (..),
   Edge (..),
   Graph (..),
   EqClassGraphMap (..),
   getListNode,
   genEqClass
   ) where

import           Data.List

data Node = Node {
    ntype    :: Int,
    nname    :: Int,
    ngsource :: String --"Left" xor "Right"
    } deriving (Eq)

instance Show Node where
  show (Node a b c) = show b ++ ":" ++ show a ++ ":" ++ c

data Edge = Edge {
    etype    :: Int,
    label    :: Int,
    source   :: Node,
    target   :: Node,
    egsource :: String --"Left" xor "Right"
    } deriving (Eq)

instance Show Edge where
  show (Edge t a (Node b1 b2 b3) (Node c1 c2 c3) s) = show a ++ ":" ++ show t ++ "(" ++ show b2 ++ "->" ++ show c2 ++ ")" ++ s

data Graph = Graph {
    nodes :: [Node],
    edges :: [Edge]
    } deriving (Show, Eq)

type EqClassGraph = ([[Node]],[[Edge]])

data EqClassGraphMap = EqClassGraphMap {
    eqGraph :: EqClassGraph,
    nodeMap :: [([Node],Int)],
    edgeMap :: [([Edge],Int)]
    } deriving (Eq, Show)

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
partitions [] = [[[]]]
partitions [a] = [[[a]]]
partitions a = foldr g [[[head a]]] (tail a)

-- | Checks if two edges are in the same equivalence class
checkST :: [[[Node]]] -> Edge -> Edge -> Bool
checkST a (Edge type1 _ s1 t1 _) (Edge type2 _ s2 t2 _) = exp1 && exp2 && exp3
    where
        exp1 = type1 == type2
        exp2 = ntype s1 == ntype s2 && ntype t1 == ntype t2
        exp3 = (getInd l1 s1 0 == getInd l1 s2 0) && (getInd l2 t1 0 == getInd l2 t2 0)
        l1   = findTypeList a (ntype s1)
        l2   = findTypeList a (ntype t1)

-- | Checks if two nodes are in the same equivalence class
checkNode :: Node -> Node -> Bool
checkNode (Node a1 _ _) (Node a2 _ _) = a1 == a2

-- | Adds elements in their eq class, creates a new if does not exists
insr :: (a -> Bool) -> a -> [[a]] -> [[a]]
insr f e []     = [[e]]
insr f e (x:xs) = if f (head x) then (e:x):xs else x:insr f e xs

-- | Separates elements by his eq class
partitionBy :: (a -> a -> Bool) -> [[a]] -> [a] -> [[a]]
partitionBy f l    []     = l
partitionBy f [[]] (x:xs) = partitionBy f    [[x]]     xs
partitionBy f l    (x:xs) = partitionBy f (insr (f x) x l) xs

genEqClass :: Graph -> [EqClassGraphMap]
genEqClass gra = generate gra 1000

generate :: Graph -> Int -> [EqClassGraphMap]
generate gra = generateMap eqGraphs
   where
      partBy f = partitionBy f [[]]
      eqGraphs = map
                   adjust
                   [(a,b) |
                     a <- mapM partitions (partBy checkNode (nodes gra)),
                     b <- mapM partitions (partBy (checkST a) (edges gra))]

-- | Returns the index of @a@ in [[Node]]
getInd :: [[Node]] -> Node -> Int -> Int
getInd (x:xs) a n = if a `elem` x then n else getInd xs a (n+1)
getInd [] _ _ = error "error when generating overlapping pairs"

-- | Returns the list which Node is in [[Node]]
getListNode :: [[Node]] -> Node -> [Node]
getListNode (x:xs) a = if a `elem` x then x else getListNode xs a
getListNode [] _ = error "error when generating overlapping pairs"

-- | Returns the index of @a@ in [[[Node]]]
getIndList :: [[[Node]]] -> Node -> Int -> Int
getIndList (x:xs) a n = if ntype (head $ head x) == ntype a then getInd x a n else getIndList xs a (n + sum (map length x))
getIndList [] _ _ = error "error when generating overlapping pairs"

findTypeList :: [[[Node]]] -> Int -> [[Node]]
findTypeList (x:xs) t = if ntype (head $ head x) == t then x else findTypeList xs t
findTypeList [] _ = error "error when generating overlapping pairs"

generateMap :: [EqClassGraph] -> Int -> [EqClassGraphMap]
generateMap []     n = []
generateMap (g:gs) n = gmapAdj:generateMap gs n'
   where
      (gmap,n') = addMap g n
      gmapAdj = adjList gmap

adjList :: EqClassGraphMap -> EqClassGraphMap
adjList (EqClassGraphMap (n,e) nM eM) = EqClassGraphMap (n2,e2) nM eM
    where
        n2 = if n == [[]] then [] else n
        e2 = if e == [[]] then [] else e

adjust :: ([[[Node]]],[[[Edge]]]) -> EqClassGraph
adjust a = (concat (fst a), concat (snd a))

addMap :: EqClassGraph -> Int -> (EqClassGraphMap,Int)
addMap g@(nodes,edges) n = (EqClassGraphMap g (addMapElem nodes n) (addMapElem edges (n+ln)), n+ln+le)
   where
      ln = length nodes
      le = length edges

addMapElem :: [a] -> Int -> [(a,Int)]
addMapElem []     _ = []
addMapElem (i:is) n = (i,n):addMapElem is (n+1)
