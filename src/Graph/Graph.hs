{-# LANGUAGE TypeFamilies #-}
module Graph.Graph (
      edgePayload
    , Edge
    , EdgeId
    , edges
    , edgesFromNode
    , edgesIntoNode
    , empty
    , Graph
    , incidentEdges
    , insertEdge
    , insertEdgeWithPayload
    , insertNode
    , insertNodeWithPayload
    , isAdjacentTo
    , isEdgeOf
    , isIncidentTo
    , isNodeOf
    , neighbourNodes
    , Node
    , NodeId
    , nodePayload
    , nodes
    , nodesConnectedTo
    , nodesFromNode
    , nodesIntoNode
    , nodesWithPayload
    , Graph.Graph.null
    , removeEdge
    , removeNode
    , sourceOf
    , targetOf
    , updateEdgePayload
    , updateNodePayload
) where

import Control.Applicative ((<$>))
import Valid
import Data.List
import Data.List.Utils

data Node a = Node { getNodePayload :: Maybe a
              } deriving (Show, Read)

instance Eq (Node a) where
    n == n' = True -- Simplifies all Eq instances that depend upon Node

data Edge a = Edge { getSource   :: Int
                   , getTarget   :: Int
                   , getEdgePayload :: Maybe a
              } deriving (Show, Read)

instance Eq (Edge a) where
    e == e' = s == s' && t == t'
            where 
              s  = getSource e
              t  = getTarget e
              s' = getSource e'
              t' = getTarget e'

type NodeId = Int
type EdgeId = Int

data Graph a b = Graph {
    nodeMap :: [(Int, Node a)],
    edgeMap :: [(Int, Edge b)]
    } deriving (Eq, Read)



instance Show (Graph a b) where
    show gr@(Graph nm em) =
              "Nodes:\n" ++
              concatMap (\(n, _) -> "\t" ++ show n ++ "\n") nm ++
              "Edges:\n" ++
              concatMap (\(eid, e) -> "\t" ++ show eid ++ "\n") em

empty :: Graph a b
empty = Graph [] []

null :: Graph a b -> Bool
null (Graph [] []) = True
null _ = False

-- Build and modify graphs
insertNode :: NodeId -> Graph a b -> Graph a b
insertNode n g@(Graph ns es) =
    Graph (addToAL ns n (Node Nothing)) es

insertNodeWithPayload :: NodeId -> a -> Graph a b -> Graph a b
insertNodeWithPayload n p g@(Graph ns es) =
    Graph (addToAL ns n (Node (Just p))) es

insertEdge :: EdgeId -> NodeId -> NodeId -> Graph a b -> Graph a b
insertEdge e src tgt g@(Graph ns es)
    | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
        Graph ns (addToAL es e (Edge src tgt Nothing))
    | otherwise = g

insertEdgeWithPayload :: EdgeId -> NodeId -> NodeId -> b -> Graph a b -> Graph a b
insertEdgeWithPayload e src tgt p g@(Graph ns es)
    | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
        Graph ns (addToAL es e (Edge src tgt (Just p)))
    | otherwise = g


removeNode :: NodeId -> Graph a b -> Graph a b
removeNode n g@(Graph ns es)
    | Prelude.null $ incidentEdges g n = Graph (delFromAL ns n) es
    | otherwise = g

removeEdge :: EdgeId -> Graph a b -> Graph a b
removeEdge e (Graph ns es) = Graph ns (delFromAL es e)

updateNodePayload :: NodeId -> Graph a b -> (a -> a) -> Graph a b
updateNodePayload n g@(Graph ns es) f =
    case nd of
        Nothing -> g
        Just n' ->
            Graph
            (addToAL ns n $ n' { getNodePayload = f <$> (p n') }) es
  where
    nd = lookup n ns
    p n = getNodePayload n

updateEdgePayload :: EdgeId -> Graph a b -> (b -> b) -> Graph a b
updateEdgePayload e g@(Graph ns es) f =
    case ed of
        Nothing -> g
        Just e' ->
            Graph
            ns (addToAL es e $ e' { getEdgePayload = f <$> (p e') })
  where
    ed = lookup e es
    p e = getEdgePayload e


-- Test the presence and access nodes and edges
nodes :: Graph a b -> [NodeId]
nodes (Graph ns _) = keysAL ns        

edges :: Graph a b -> [EdgeId]
edges (Graph _ es) = keysAL es        

nodePayload :: Graph a b -> NodeId -> Maybe a
nodePayload g n = (lookup n $ nodeMap g) >>= getNodePayload

nodesWithPayload :: Graph a b -> [(NodeId, Maybe a)]
nodesWithPayload (Graph nodeMap _) =
    map (\(k, n) -> (k, getNodePayload n)) nodeMap

edgePayload :: EdgeId -> Graph a b -> Maybe b
edgePayload e g = (lookup e $ edgeMap g) >>= getEdgePayload

nodesConnectedTo :: Graph a b -> EdgeId -> [(NodeId, NodeId)]
nodesConnectedTo g@(Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge src tgt _) -> [(src, tgt)]
        otherwise -> []

sourceOf :: Graph a b -> EdgeId -> [NodeId]
sourceOf (Graph _ es) e =
    let res = lookup e es
    in case res of
        (Just ed) -> [getSource ed]
        otherwise -> []

targetOf :: Graph a b -> EdgeId -> [NodeId]
targetOf (Graph _ es) e =
    let res = lookup e es
    in case res of
        (Just ed) -> [getTarget ed]
        otherwise -> []

isNodeOf :: Graph a b -> NodeId -> Bool
isNodeOf g n  = n `elem` (nodes g)

isEdgeOf :: Graph a b -> EdgeId -> Bool
isEdgeOf g e  = e `elem` (edges g)

-- Navigate within graph
edgesFromNode :: Graph a b -> NodeId -> [EdgeId]
edgesFromNode g n = filter (\e -> sourceOf g e == [n]) (edges g)

edgesIntoNode :: Graph a b -> NodeId -> [EdgeId]
edgesIntoNode g n = filter (\e -> targetOf g e == [n]) (edges g)

nodesFromNode :: Graph a b -> NodeId -> [NodeId]
nodesFromNode g n = filter (\v -> isAdjacentTo g n v) (nodes g)

nodesIntoNode :: Graph a b -> NodeId -> [NodeId]
nodesIntoNode g n = filter (\v -> isAdjacentTo g v n) (nodes g)

isAdjacentTo :: Graph a b -> NodeId -> NodeId -> Bool
isAdjacentTo g n1 n2 = any (\e -> nodesConnectedTo g e == [(n1,n2)]) (edges g)

isIncidentTo :: Graph a b -> NodeId -> NodeId -> Bool
isIncidentTo g e n = let res = nodesConnectedTo g e
                   in case res of
                      ((s, t):_) -> n == s || n == t
                      otherwise   -> False

incidentEdges :: Graph a b -> NodeId -> [EdgeId]
incidentEdges g n = nub $ edgesIntoNode g n ++ edgesFromNode g n

neighbourNodes :: Graph a b -> NodeId -> [NodeId]
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
