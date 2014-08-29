{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module GraphClass where

import Data.Maybe
import Data.List
import qualified Data.IntMap as IM
------------- GRAPH TYPECLASS ----------------------------------

class GraphClass g where

  -- Graph types
  type Nd g :: *  -- node type
  type Ed g :: *  -- edge type
	
  -- Build and modify graphs
  empty           ::                                    g  -- required
  insertNode      ::                     (Nd g) -> g -> g  -- required
  insertEdge      :: (Ed g) -> (Nd g) -> (Nd g) -> g -> g  -- required
  removeNode      ::                     (Nd g) -> g -> g  -- required
  removeEdge      ::                     (Ed g) -> g -> g  -- required

  -- Test the presence and access nodes and edges
  nodes            :: g -> [Nd g]                          -- required
  edges            :: g -> [Ed g]                          -- required
  nodesConnectedTo :: g -> (Ed g) -> [(Nd g, Nd g)]        -- required
  isNodeOf         :: Eq (Nd g) => g -> Nd g -> Bool
  isEdgeOf         :: Eq (Ed g) => g -> Ed g -> Bool
  sourceOf         :: g -> Ed g -> [Nd g]
  targetOf         :: g -> Ed g -> [Nd g]

  -- Navigate within graph
  edgesFromNode  :: (Eq (Nd g)) => g -> Nd g -> [Ed g]
  edgesIntoNode  :: (Eq (Nd g)) => g -> Nd g -> [Ed g]
  nodesFromNode  :: (Eq (Nd g)) => g -> Nd g -> [Nd g]
  nodesIntoNode  :: (Eq (Nd g)) => g -> Nd g -> [Nd g]
  isAdjacentTo   :: (Eq (Nd g)) => g -> Nd g -> Nd g -> Bool
  isIncidentTo   :: (Eq (Nd g)) => g -> Ed g -> Nd g -> Bool
  incidentEdges  :: (Eq (Nd g), (Eq (Ed g))) => g -> Nd g -> [Ed g]
  neighbourNodes :: (Eq (Nd g)) => g -> Nd g -> [Nd g]

  ---------- Default implementations -------------

  -- Test the presence and access nodes and edges: default implementation
  isNodeOf g n  = n `elem` (nodes g)
  isEdgeOf g e  = e `elem` (edges g)
  nodesConnectedTo g e = do { s <- sourceOf g e; t <- targetOf g e; return (s,t) }
  sourceOf g e  = fmap fst $ nodesConnectedTo g e
  targetOf g e  = fmap snd $ nodesConnectedTo g e

  -- Navigate within graph: default implementation
  edgesFromNode g n    = filter (\e -> sourceOf g e == [n]) (edges g)
  edgesIntoNode g n    = filter (\e -> targetOf g e == [n]) (edges g)
  nodesFromNode g n    = filter (\v -> isAdjacentTo g n v) (nodes g)
  nodesIntoNode g n    = filter (\v -> isAdjacentTo g v n) (nodes g)
  isAdjacentTo g n1 n2 = any (\e -> nodesConnectedTo g e == [(n1,n2)]) (edges g)
  isIncidentTo g e n   = let res = nodesConnectedTo g e
                         in case res of
                            ((s, t):_) -> n == s || n == t
                            otherwise   -> False
  incidentEdges g n    = nub $ edgesIntoNode g n ++ edgesFromNode g n
  neighbourNodes g n   = nub $ nodesIntoNode g n ++ nodesFromNode g n 


-------------------------------------------------------------------------------------------

{-
class GraphClass g => TypedGraphClass g where

  getTypeOfNode :: (Nd g) -> g -> Maybe (Nd g)
  setTypeOfNode :: (Nd g) -> (Nd g) -> g -> g

  getTypeOfEdge :: (Ed g) -> g -> Maybe (Ed g)
  setTypeOfEdge :: (Ed g) -> (Ed g) -> g -> g
-}


class GraphClass g => ValuedGraphClass g where

  type NVal g :: *  -- node value
  type EVal g :: *  -- edge value

  setNodeValue   :: (Nd g) -> (NVal g) -> g -> g
  getNodeValue   :: (Nd g) -> g -> Maybe (NVal g)
  clearNodeValue :: (Nd g) -> g -> g
  

class GraphClass g => VisualGraphClass g where

  getNodePos   :: (Nd g) -> g -> (Double,Double)
  setNodePos   :: (Nd g) -> (Double,Double) -> g -> g
  getEdgePos   :: (Ed g) -> g -> (Double,Double)
  setEdgePos   :: (Ed g) -> (Double,Double) -> g -> g


