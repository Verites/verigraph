module TypedGraph.Partitions.GraphPartition (
   Node (..),
   Edge (..),
   Graph,
   GraphPartition,
   getNode,
   generateGraphPartitions
   ) where

import           Data.Maybe (mapMaybe)
import           Util.List

-- | A Node with the needed information for generating equivalence classes
data Node = Node {
    nodeType      :: Int,
    nodeName      :: Int,
    nodeId        :: Int,
    injectiveNode :: Bool, --injective flag
    nodeFromLeft  :: Bool
    }

instance Show Node where
  show (Node nodeType nodeName nodeId injectiveNode nodeFromLeft) =
    show nodeName ++ ":" ++ show nodeType ++ ":" ++
    (if nodeFromLeft then "Left" else "Right") ++ " (id:" ++ show nodeId ++") {"++ show injectiveNode ++"}"

instance Eq Node where
  (Node nodeType1 nodeName1 _ _ fromLeft1) == (Node nodeType2 nodeName2 _ _ fromLeft2) =
    nodeType1 == nodeType2 && nodeName1 == nodeName2 && fromLeft1 == fromLeft2

-- | An Edge with the needed informations for the generating equivalence classes algorithm
data Edge = Edge {
    edgeType      :: Int,
    label         :: Int,
    edgeId        :: Int,
    source        :: Node,
    target        :: Node,
    injectiveEdge :: Bool, --injective flag
    edgeFromLeft  :: Bool
    } deriving (Eq)

instance Show Edge where
  show (Edge edgeType label edgeId (Node _ sourceName _ _ _) (Node _ targetName _ _ _) injectiveEdge edgeFromLeft) =
    show label ++ ":" ++
    show edgeType ++ "(" ++
    show sourceName ++ "->" ++
    show targetName ++ ")" ++
    (if edgeFromLeft then "Left" else "Right") ++
    " (id:" ++ show edgeId ++") {"++ show injectiveEdge ++"}"

-- | Graph data for generating equivalence classes
type Graph = ([Node],[Edge])

-- | An equivalence class of a 'Graph'

-- | TODO: use Data.Set?
type GraphPartition = ([[Node]],[[Edge]])

{-partitions-}

-- | Checks if a node belongs to an equivalence class
nodeBelongsToEquivalenceClass :: Node -> [Node] -> Bool
nodeBelongsToEquivalenceClass _ [] = error "error 'nodeBelongsToEquivalenceClass' in GraphPartition"
nodeBelongsToEquivalenceClass (Node type1 _ _ injectiveNode side1) l@(Node type2 _ _ _ _ : _) =
  type1 == type2 && (not injectiveNode || not thereIsAnotherInjectiveNode)
  where
    thereIsAnotherInjectiveNode = any (\(Node _ _ _ injectiveNode side2) -> injectiveNode && side1 == side2) l

-- | Checks if two edges are in the same equivalence class
-- Needs @nodes@ to know if a source or target was collapsed
edgeBelongsToEquivalenceClass :: [[Node]] -> Edge -> [Edge] -> Bool
edgeBelongsToEquivalenceClass _ _ [] = error "error 'edgeBelongsToEquivalenceClass' in GraphPartition"
edgeBelongsToEquivalenceClass nodes (Edge type1 _ _ s1 t1 injectiveEdge side) l@(Edge type2 _ _ s2 t2 _ _ : _) =
  equalTypes && canBeAddedToClass && equivalentSources && equivalentTargets
  where
    equalTypes = type1 == type2
    canBeAddedToClass = not injectiveEdge || not thereIsAnotherInjectiveEdge
    thereIsAnotherInjectiveEdge = any (\(Edge _ _ _ _ _ inj side2) -> inj && side == side2) l
    nodeNameAndSource node = (nodeName node, nodeFromLeft node)
    source1   = getNode (nodeNameAndSource s1) nodes
    source2   = getNode (nodeNameAndSource s2) nodes
    equivalentSources = source1 == source2
    source3   = getNode (nodeNameAndSource t1) nodes
    source4   = getNode (nodeNameAndSource t2) nodes
    equivalentTargets = source3 == source4

-- | Runs generator of partitions for nodes, and after for edges according to the nodes generated
generateGraphPartitions :: Graph -> [GraphPartition]
generateGraphPartitions graph = concatMap f nodeEquivalences
  where
    nodes = fst graph
    edges = snd graph
    nodeEquivalences = generatePartitions nodeBelongsToEquivalenceClass nodes
    edgeEquivalences x = generatePartitions (edgeBelongsToEquivalenceClass x) edges
    f :: [[Node]] -> [GraphPartition]
    f nodeEquivalences =
      zip (replicate (length (edgeEquivalences nodeEquivalences)) nodeEquivalences) (edgeEquivalences nodeEquivalences)

-- | Interface function to run the algorithm that generates the partitions
generatePartitions :: (a -> [a] -> Bool) -> [a] -> [[[a]]]
generatePartitions equivalenceChecker equivalenceList =
  if null equivalenceList
    then [[]]
    else backtracking equivalenceChecker equivalenceList []

-- | Runs a backtracking algorithm to create all partitions
-- receives a function of restriction to know when a element can be combined with other
backtracking :: (a -> [a] -> Bool) -> [a] -> [[a]] -> [[[a]]]
backtracking equivalenceChecker toAdd []      = backtracking equivalenceChecker (init toAdd) [[last toAdd]]
backtracking _ [] eqClass                     = [eqClass]
backtracking equivalenceChecker toAdd eqClass =
  backtracking equivalenceChecker initAdd ([ad]:eqClass) ++
    concat
      (mapMaybe
        (\(n,id) -> if equivalenceChecker ad n
                      then
                        Just (backtracking equivalenceChecker initAdd (replace id (ad:n) eqClass))
                      else
                        Nothing)
      (zip eqClass [0..]))
  where
    initAdd = init toAdd
    ad = last toAdd

-- | Returns the node that this @p@ was collapsed in partitions
-- Used to compare if an edge can be mixed with another
-- GraphPartitionToVerigraph use to discover source and target of edges
getNode :: (Int,Bool) -> [[Node]] -> Node
getNode _ [] = error "error when generating overlapping pairs (getNode)"
getNode p@(name,source) (x:xs) =
  if any (\n -> nodeName n == name && nodeFromLeft n == source) x
    then
      head x
    else
      getNode p xs
