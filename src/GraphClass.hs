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
  nodesConnectedTo :: (Ed g) -> g -> Maybe (Nd g, Nd g)    -- required
  isNodeOf         :: Eq (Nd g) => (Nd g) -> g -> Bool
  isEdgeOf         :: Eq (Ed g) => (Ed g) -> g -> Bool
  sourceOf         :: Ed g -> g -> Maybe (Nd g)
  targetOf         :: Ed g -> g -> Maybe (Nd g)  

  -- Navigate within graph
  edgesFromNode  :: (Eq (Nd g)) => Nd g -> g -> [Ed g]
  edgesIntoNode  :: (Eq (Nd g)) => Nd g -> g -> [Ed g]
  nodesFromNode  :: (Eq (Nd g)) => Nd g -> g -> [Nd g]
  nodesIntoNode  :: (Eq (Nd g)) => Nd g -> g -> [Nd g]
  isAdjacentTo   :: (Eq (Nd g)) => Nd g -> Nd g -> g -> Bool
  isIncidentTo   :: (Eq (Nd g)) => Ed g -> Nd g -> g -> Bool
  incidentEdges  :: (Eq (Nd g), (Eq (Ed g))) => Nd g -> g -> [Ed g]
  neighbourNodes :: (Eq (Nd g)) => Nd g -> g -> [Nd g]

  ---------- Default implementations -------------

  -- Test the presence and access nodes and edges: default implementation
  isNodeOf n g  = n `elem` (nodes g)
  isEdgeOf e g  = e `elem` (edges g)
  nodesConnectedTo e g = do { s <- sourceOf e g; t <- targetOf e g; return (s,t) }
  sourceOf e g  = fmap fst $ nodesConnectedTo e g
  targetOf e g  = fmap snd $ nodesConnectedTo e g

  -- Navigate within graph: default implementation
  edgesFromNode n g    = filter (\e -> sourceOf e g == Just n)    (edges g)
  edgesIntoNode n g    = filter (\e -> targetOf e g == Just n)    (edges g)
  nodesFromNode n g    = filter (\v -> isAdjacentTo n v g)   (nodes g)
  nodesIntoNode n g    = filter (\v -> isAdjacentTo v n g)   (nodes g)
  isAdjacentTo n1 n2 g = any (\e-> nodesConnectedTo e g == Just (n1,n2)) (edges g)
  isIncidentTo e n g   = fromMaybe False $ do { (s,t) <- nodesConnectedTo e g; return $ n==s || n==t }
  incidentEdges n g    = nub $ edgesIntoNode n g ++ edgesFromNode n g
  neighbourNodes n g   = nub $ nodesIntoNode n g ++ nodesFromNode n g 


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


