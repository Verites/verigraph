module CriticalPairs.GraphPart (
   DelType (..),
   Node (..),
   Edge (..),
   Graph (..),
   EqClassGraph (..),
   EqClassGraphMap (..),
   getListNode,
   getInd,
   nRight,
   nLeft,
   eRight,
   eLeft,
   genEqClass
   ) where

import Data.List

data DelType = Undefined | Not | Right | Left | Both deriving (Eq, Show)

data Node = Node {
    ntype :: Int,
    nname :: Int,
    ngsource :: String, --"Left" xor "Right"
    ndel :: DelType
    }

nRight = \x -> ngsource x == "Right"
nLeft  = \x -> ngsource x == "Left"

instance Eq Node where
  (Node t1 n1 s1 d1) == (Node t2 n2 s2 d2) = 
     t1 == t2 &&
     n1 == n2 &&
     s1 == s2

instance Show Node where
  show (Node a b c _) = (show b) ++ ":" ++ (show a) ++ ":" ++ c

data Edge = Edge {
    etype :: Int,
    label :: Int,
    source :: Node, --do not access delType from here
    target :: Node, --do not access delType from here
    egsource :: String, --"Left" xor "Right"
    edel :: DelType
    }

eRight = \x -> egsource x == "Right"
eLeft  = \x -> egsource x == "Left"

instance Eq Edge where
  (Edge t1 l1 sr1 tg1 s1 d1) == (Edge t2 l2 sr2 tg2 s2 d2) =
     t1 == t2 &&
     l1 == l2 &&
     sr1 == sr2 &&
     tg1 == tg2 &&
     s1 == s2

instance Show Edge where
  show (Edge t a (Node b1 b2 b3 _) (Node c1 c2 c3 _) s _) = (show a) ++ ":" ++ (show t) ++ "(" ++ (show b2) ++ "->" ++ (show c2) ++ ")" ++ s

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

--h ("c") ["a", "b"] 2
-- = h ("c") ["a", "b"] 1 ++ ["a","b","c"]
--
--h ("c") ["a"] 0
-- = [["a","c"]]
h :: t -> [[t]] -> Int -> [[[t]]]
h a b 0 = [[a]:b]
h a b n = (h a b (n-1)) ++ [(insertPos n (a:x) b)]
    where
       x = b!!(n-1)
       insertPos n newVal l = (take (n-1) l) ++ [newVal] ++ (drop n l) --list[n] = newVal

--g ("c") [[A,B],[AB]] 2 ->
-- (g ("c") [[A,B],[AB]] 1) ++ (h ("c") [AB] 1)
g :: t -> [[[t]]] -> Int -> [[[t]]]
g a b 0 = []
g a b n = (g a b (n-1)) ++ (h a x (length x))
    where x = b!!(n-1)

--partitions cria as classes de equivalencia, inserindo um elemento por vez
partitions :: [[[t]]] -> [t] -> [[[t]]]
--fim dos elementos de a
partitions b [] = b
--primeira chamada de partitions, retorna um único elemento
partitions [[[]]] a = partitions [[[head a]]] (tail a)
--chama g para inserir na atual classe de equivalencia mais um elemento, combinando ele de todas formas possíveis
partitions b a = partitions (g (head a) b (length b)) (tail a)

--Retorna o indice que o Node está em [[Node]]
getInd :: [[Node]] -> Node -> Int -> Int
getInd (x:xs) a n = if (elem a x) then n else (getInd xs a (n+1))
--getInd [] _ _ = -1

--Retorna a lista que o Node está em [[Node]]
getListNode :: [[Node]] -> Node -> [Node]
getListNode (x:xs) a = if (elem a x) then x else (getListNode xs a)

--Retorna o indice que o Node está em [[[Node]]]
getIndList :: [[[Node]]] -> Node -> Int -> Int
getIndList (x:xs) a n = if ((ntype $ head $ head x) == (ntype a)) then (getInd x a n) else (getIndList xs a (n + (sum $ map length x)))

findTypeList :: [[[Node]]] -> Int -> [[Node]]
findTypeList (x:xs) t = if ((ntype $ head $ head x) == t) then x else findTypeList xs t

--critério para o agrupamento dos edges
checkST :: [[[Node]]] -> Edge -> Edge -> Bool
checkST a (Edge type1 _ s1 t1 _ _) (Edge type2 _ s2 t2 _ _) = exp1 && exp2 && exp3
    where
        exp1 = type1 == type2
        exp2 = ntype s1 == ntype s2 && ntype t1 == ntype t2
        exp3 = ((getInd l1 s1 0) == (getInd l1 s2 0)) && ((getInd l2 t1 0) == (getInd l2 t2 0))
        l1   = findTypeList a (ntype s1)
        l2   = findTypeList a (ntype t1)

--critério para o agrupamento dos nodes
checkNode :: Node -> Node -> Bool
checkNode (Node a1 _ _ _) (Node a2 _ _ _) = a1 == a2

--função auxiliar que adiciona o elemento na sua classe de equivalência
insr :: (a -> a -> Bool) -> [[a]] -> a -> [[a]]
insr f []     e = [[e]]
insr f (x:xs) e = if (f (head x) e) then (e:x):xs else x:(insr f xs e)

partitionBy :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
partitionBy l    f []     = l
partitionBy [[]] f (x:xs) = partitionBy    [[x]]     f xs
partitionBy l    f (x:xs) = partitionBy (insr f l x) f xs

genEqClass :: Graph -> [EqClassGraphMap]
genEqClass gra = generate gra 1000

generate :: Graph -> Int -> [EqClassGraphMap]
generate gra n = generateMap eqGraphs n
   where
      eqGraphs = map adjust [(a,b) | a <- sequence (map partit (partBy checkNode (nodes gra))), b <- sequence (map partit (partBy (checkST a) (edges gra)))]
      partBy = partitionBy [[]]
      partit = partitions [[[]]]

generateMap :: [EqClassGraph] -> Int -> [EqClassGraphMap]
generateMap []     n = []
generateMap (g:gs) n = gmapAdj:(generateMap gs n')
   where
      (gmap,n') = addMap g n
      gmapAdj = adjList gmap

{-Ajuste para passar de [[]] -> []-}
adjList :: EqClassGraphMap -> EqClassGraphMap
adjList (EqClassGraphMap (n,e) nM eM) = (EqClassGraphMap (n2,e2) nM eM)
    where
        n2 = if n == [[]] then [] else n
        e2 = if e == [[]] then [] else e

{-Ajuste no tipo, retira um nível de [] (o partitionBy cria uma lista para cada tipo de nodo e aresta)-}
adj :: [[a]] -> [a]
adj []      = []
adj ([]:xs) = (adj xs)
adj (x:xs)  = (head x):(adj ((tail x):xs))

--Estilo mu de uma mônada
adjust :: ([[[Node]]],[[[Edge]]]) -> EqClassGraph
adjust a = (adj (fst a), adj (snd a))

addMap :: EqClassGraph -> Int -> (EqClassGraphMap,Int)
addMap g n = (EqClassGraphMap g (addMapElem (fst g) n) (addMapElem (snd g) (n+ln)), n+ln+le)
   where
      ln = length (fst g)
      le = length (snd g)

addMapElem :: [a] -> Int -> [(a,Int)]
addMapElem []     _ = []
addMapElem (i:is) n = (i,n):(addMapElem is (n+1))
