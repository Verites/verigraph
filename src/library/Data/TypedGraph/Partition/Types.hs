{-|
Description : Partitions generator types.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
-}
module Data.TypedGraph.Partition.Types
  ( Node (..)
  , Edge (..)
  , Graph
  , GraphPartition
  ) where

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
