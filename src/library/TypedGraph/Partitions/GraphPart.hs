module TypedGraph.Partitions.GraphPart (
   Node (..),
   Edge (..),
   Graph,
   EqClassGraph,
   getNode,
   genGraphEqClass
   ) where

import           Data.Maybe (mapMaybe)

-- | A Node with the needed informations for the generating equivalence classes algorithm
data Node = Node {
    ntype   :: Int,
    nname   :: Int,
    nid     :: Int,
    injn    :: Bool, --injective flag
    inLeftn :: Bool
    }

instance Show Node where
  show (Node a b id f c) = show b ++ ":" ++ show a ++ ":" ++ (if c then "Left" else "Right") ++ " (id:" ++ show id ++") {"++ show f ++"}"

instance Eq Node where
  (Node a1 b1 _ _ d1) == (Node a2 b2 _ _ d2) =
    a1 == a2 &&
    b1 == b2 &&
    d1 == d2

-- | An Edge with the needed informations for the generating equivalence classes algorithm
data Edge = Edge {
    etype   :: Int,
    label   :: Int,
    eid     :: Int,
    source  :: Node,
    target  :: Node,
    inje    :: Bool, --injective flag
    inLefte :: Bool
    } deriving (Eq)

instance Show Edge where
  show (Edge t a id (Node _ b2 _ _ _) (Node _ c2 _ _ _) f s) =
    show a ++ ":" ++
    show t ++ "(" ++
    show b2 ++ "->" ++
    show c2 ++ ")" ++
    (if s then "Left" else "Right") ++
    " (id:" ++ show id ++") {"++ show f ++"}"

-- | Graph for the generating equivalence classes algorithm
type Graph = ([Node],[Edge])

-- | An equivalence class of a 'Graph'

-- | TODO: use Data.Set?
type EqClassGraph = ([[Node]],[[Edge]])

{-partitions-}

-- | Checks if two nodes are in the same equivalence class
checkNode :: Node -> [Node] -> Bool
checkNode _ [] = error "error checkNode in GraphPart"
checkNode (Node type1 _ _ inj side) l@(Node type2 _ _ _ _ : _) =
  type1 == type2 && (not inj || not checkInj)
  where --checks if another inj node is in this list
    checkInj = any (\(Node _ _ _ inj side2) -> inj && side == side2) l

-- | Checks if two edges are in the same equivalence class
-- Needs @nodes@ to know if a source or target was collapsed
checkEdge :: [[Node]] -> Edge -> [Edge] -> Bool
checkEdge _ _ [] = error "error checkEdge in GraphPart"
checkEdge nodes (Edge type1 _ _ s1 t1 inj side) l@(Edge type2 _ _ s2 t2 _ _ : _) = exp1 && exp2 && exp3
  where
    exp1 = type1 == type2 && (not inj || not checkInj)
    --checks if another inj edge is in this list
    checkInj = any (\(Edge _ _ _ _ _ inj side2) -> inj && side == side2) l
    nameAndSrc node = (nname node, inLeftn node)
    l1   = getNode (nameAndSrc s1) nodes
    l2   = getNode (nameAndSrc s2) nodes
    exp2 = l1 == l2
    l3   = getNode (nameAndSrc t1) nodes
    l4   = getNode (nameAndSrc t2) nodes
    exp3 = l3 == l4

-- | Runs generator of partitions for nodes, and after for edges according to the nodes generated
genGraphEqClass :: Graph -> [EqClassGraph]
genGraphEqClass gra = concatMap f a
  where
    nodes = fst
    edges = snd
    a = part checkNode (nodes gra)
    b x = part (checkEdge x) (edges gra)
    f :: [[Node]] -> [([[Node]],[[Edge]])]
    f a = zip (replicate (length (b a)) a) (b a)

-- | Interface function to run the algorithm that generates the partitions
part :: (a -> [a] -> Bool) -> [a] -> [[[a]]]
part f l = if null l then [[]] else bt f l []

-- | Runs a backtracking algorithm to create all partitions
-- receives a function of restriction, to know when a element can be combined with other
bt :: (a -> [a] -> Bool) -> [a] -> [[a]] -> [[[a]]]
bt f toAdd [] = bt f (init toAdd) [[last toAdd]]
bt _ [] actual = [actual]
bt f toAdd actual =
  bt f initAdd ([ad]:actual) ++
    concat
      (mapMaybe
        (\(n,id) -> if f ad n
                      then
                        Just (bt f initAdd (replace id (ad:n) actual))
                      else
                        Nothing)
      (zip actual [0..]))
  where
    initAdd = init toAdd
    ad = last toAdd

-- | Replaces the @idx@-th element by @new@ in the list @l@
replace :: Int -> a -> [a] -> [a]
replace idx new l = take idx l ++ [new] ++ drop (idx+1) l

-- | Returns the node that this @p@ was collapsed in partitions
-- Used to compare if an edge can be mixed with another
-- GraphPartitionToVerigraph use to discover source and target of edges
getNode :: (Int,Bool) -> [[Node]] -> Node
getNode p@(name,source) (x:xs) =
  if any (\n -> nname n == name && inLeftn n == source) x
    then
      head x
    else
      getNode p xs
getNode _ [] = error "error when generating overlapping pairs (getListNode)"
