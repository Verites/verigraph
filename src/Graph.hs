{-# LANGUAGE TypeFamilies #-}
module Graph (
      Graph
    , edges
    , edgesFromNode
    , edgesIntoNode
    , empty
    , incidentEdges
    , insertNode
    , insertEdge
    , isAdjacentTo
    , isEdgeOf
    , isIncidentTo
    , isNodeOf
    , neighbourNodes
    , nodes
    , nodesConnectedTo
    , nodesFromNode
    , nodesIntoNode
    , removeEdge
    , removeNode
    , sourceOf
    , targetOf
) where

import Valid
import Data.List
import Data.List.Utils
import Data.Maybe

data Node a = Node { nodePayload :: Maybe a
                   , nodeType    :: Maybe Int
              } deriving (Eq, Show, Read)

data Edge a = Edge { getSource   :: Int
                   , getTarget   :: Int
                   , edgePayload :: Maybe a
                   , edgeType    :: Maybe Int
              } deriving (Eq, Show, Read)

type NodeID = Int
type EdgeID = Int

data Graph a b = Graph [(Int, Node a)] [(Int, Edge b)]
    deriving (Show, Read, Eq)

empty :: Graph a b
empty = Graph [] []

-- Build and modify graphs
insertNode :: NodeID -> Graph a b -> Graph a b
insertNode n g@(Graph ns es) =
    Graph (addToAL ns n (Node Nothing Nothing)) es

insertEdge :: EdgeID -> NodeID -> NodeID -> Graph a b -> Graph a b
insertEdge e src tgt g@(Graph ns es)
    | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
        Graph ns (addToAL es e (Edge src tgt Nothing Nothing))
    | otherwise = g

removeNode :: NodeID -> Graph a b -> Graph a b
removeNode n g@(Graph ns es)
    | null $ incidentEdges g n = Graph (delFromAL ns n) es
    | otherwise = g

removeEdge :: EdgeID -> Graph a b -> Graph a b
removeEdge e (Graph ns es) = Graph ns (delFromAL es e)
        
-- Test the presence and access nodes and edges
nodes :: Graph a b -> [NodeID]
nodes (Graph ns _) = keysAL ns        

edges :: Graph a b -> [EdgeID]
edges (Graph _ es) = keysAL es        

nodesConnectedTo :: Graph a b -> EdgeID -> [(NodeID, NodeID)]
nodesConnectedTo g@(Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge src tgt _ _) -> [(src, tgt)]
        otherwise -> []

sourceOf :: Graph a b -> EdgeID -> [NodeID]
sourceOf (Graph _ es) e =
    let res = lookup e es
    in case res of
        (Just ed) -> [getSource ed]
        otherwise -> []

targetOf :: Graph a b -> EdgeID -> [NodeID]
targetOf (Graph _ es) e =
    let res = lookup e es
    in case res of
        (Just ed) -> [getTarget ed]
        otherwise -> []

isNodeOf :: Graph a b -> NodeID -> Bool
isNodeOf g n  = n `elem` (nodes g)

isEdgeOf :: Graph a b -> EdgeID -> Bool
isEdgeOf g e  = e `elem` (edges g)

-- Navigate within graph
edgesFromNode :: Graph a b -> NodeID -> [EdgeID]
edgesFromNode g n = filter (\e -> sourceOf g e == [n]) (edges g)

edgesIntoNode :: Graph a b -> NodeID -> [EdgeID]
edgesIntoNode g n = filter (\e -> targetOf g e == [n]) (edges g)

nodesFromNode :: Graph a b -> NodeID -> [NodeID]
nodesFromNode g n = filter (\v -> isAdjacentTo g n v) (nodes g)

nodesIntoNode :: Graph a b -> NodeID -> [NodeID]
nodesIntoNode g n = filter (\v -> isAdjacentTo g v n) (nodes g)

isAdjacentTo :: Graph a b -> NodeID -> NodeID -> Bool
isAdjacentTo g n1 n2 = any (\e -> nodesConnectedTo g e == [(n1,n2)]) (edges g)

isIncidentTo :: Graph a b -> NodeID -> NodeID -> Bool
isIncidentTo g e n = let res = nodesConnectedTo g e
                   in case res of
                      ((s, t):_) -> n == s || n == t
                      otherwise   -> False

incidentEdges :: Graph a b -> NodeID -> [EdgeID]
incidentEdges g n = nub $ edgesIntoNode g n ++ edgesFromNode g n

neighbourNodes :: Graph a b -> NodeID -> [NodeID]
neighbourNodes g n = nub $ nodesIntoNode g n ++ nodesFromNode g n 

instance Valid (Graph a b) where
    valid g =
        all (\e ->
                let src = sourceOf g e
                    tgt = targetOf g e
                in case (src, tgt) of
                    ((s:_), (t:_)) -> isNodeOf g s && isNodeOf g t
                    otherwise      -> False)
            (edges g)

{-
instance TypedGraphClass (Graph a b) where
    getTypeOfNode n (Graph ns _) =
        let found = lookup n ns
        in case found of
            Just nd   -> nodeType nd
            otherwise -> Nothing
    setTypeOfNode tn n (Graph ns es) =
        let found = lookup n ns
        in case found of
            Just (Node p _) -> Graph (addToAL ns n (Node p (Just tn))) es
            otherwise -> Graph ns es

    getTypeOfEdge e (Graph _ es) =
        let found = lookup e es
        in case found of
            Just ed   -> edgeType ed
            otherwise -> Nothing
    setTypeOfEdge te e (Graph ns es) =
        let found = lookup e es
        in case found of
            Just (Edge s t p _) -> Graph ns (addToAL es e (Edge s t p (Just te)))
            otherwise -> Graph ns es
-}
