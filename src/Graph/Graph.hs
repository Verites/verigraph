{-# LANGUAGE TypeFamilies #-}
module Graph.Graph (
    -- * Types
      Edge
    , EdgeId (..)
    , Node
    , NodeId (..)
    , Graph

    -- * Construction
    , empty
    , build

    -- ** Insertion
    , insertEdge
    , insertNode
    , insertEdgeWithPayload
    , insertNodeWithPayload

    -- ** Delete/Update
    , removeEdge
    , removeNode
    , updateEdgePayload
    , updateNodePayload

    -- * Extraction
    , edges
    , nodes
    , edgesFromNode
    , edgesIntoNode
    , edgesWithPayload
    , incidentEdges
    , neighbourNodes
    , nodesConnectedTo
    , nodesFromNode
    , nodesIntoNode
    , nodesWithPayload

    -- * Query
    , Graph.Graph.null
    , nodePayload
    , edgePayload
    , sourceOf
    , targetOf
    -- ** Predicates
    , isEdgeOf
    , isNodeOf
    , isAdjacentTo
    , isIncidentTo
) where

import Control.Applicative ((<$>))
import Abstract.Valid
import Data.List
import Data.List.Utils

data Node a = Node { getNodePayload :: Maybe a
              } deriving (Show, Read)

instance Eq (Node a) where
    n == n' = True -- Simplifies all Eq instances that depend upon Node

data Edge a = Edge { getSource   :: NodeId
                   , getTarget   :: NodeId
                   , getEdgePayload :: Maybe a
              } deriving (Show, Read)

instance Eq (Edge a) where
    e == e' = s == s' && t == t'
            where
              s  = getSource e
              t  = getTarget e
              s' = getSource e'
              t' = getTarget e'

data Graph a b = Graph {
    nodeMap :: [(NodeId, Node a)],
    edgeMap :: [(EdgeId, Edge b)]
    } deriving (Read)

eq :: (Eq t1, Eq t2) => [(t1, t2)] -> [(t1, t2)] -> Bool
eq [] [] = True
eq a  b  = hasAny a b && hasAny b a

instance Eq (Graph a b) where
    (Graph nodeMap1 edgeMap1) == (Graph nodeMap2 edgeMap2) =
         eq nodeMap1 nodeMap2 &&
         eq edgeMap1 edgeMap2

instance Show (Graph a b) where
    show gr@(Graph nm em) =
              "Nodes:\n" ++
              concatMap (\(n, _) -> "\t" ++ show n ++ "\n") nm ++
              "Edges:\n" ++
              concatMap (\(eid, e) -> "\t" ++ show eid ++ "\n") em


newtype NodeId = NodeId Int deriving (Eq, Ord, Read)
newtype EdgeId = EdgeId Int deriving (Eq, Ord, Read)

instance Show NodeId where
    show (NodeId i) = show i

instance Show EdgeId where
    show (EdgeId i) = show i

-- | Create an empty Graph.
empty :: Graph a b
empty = Graph [] []

-- | Build a Graph
build :: [Int] -> [(Int,Int,Int)] -> Graph a b
build n e = foldr (\(a,b,c) -> insertEdge a b c) g (map (\(a,b,c) -> (EdgeId a,NodeId b,NodeId c)) e)
    where
        g = foldr insertNode empty (map NodeId n)

-- | Insert a node @n@ in a graph @g@, without payload.
insertNode :: NodeId -> Graph a b -> Graph a b
insertNode n g@(Graph ns es) =
    Graph (addToAL ns n (Node Nothing)) es

-- | Insert a node @n@ in a graph @g@ with payload @p@.
insertNodeWithPayload :: NodeId -> a -> Graph a b -> Graph a b
insertNodeWithPayload n p g@(Graph ns es) =
    Graph (addToAL ns n (Node (Just p))) es

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@, without payload.
insertEdge :: EdgeId -> NodeId -> NodeId -> Graph a b -> Graph a b
insertEdge e src tgt g@(Graph ns es)
    | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
        Graph ns (addToAL es e (Edge src tgt Nothing))
    | otherwise = g

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@ with payload @p@.
insertEdgeWithPayload :: EdgeId -> NodeId -> NodeId -> b -> Graph a b -> Graph a b
insertEdgeWithPayload e src tgt p g@(Graph ns es)
    | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
        Graph ns (addToAL es e (Edge src tgt (Just p)))
    | otherwise = g

-- | If @n@ exists in @g@, and there are no incident edges on it, remove it.
-- Return @g@ otherwise.
removeNode :: NodeId -> Graph a b -> Graph a b
removeNode n g@(Graph ns es)
    | Prelude.null $ incidentEdges g n = Graph (delFromAL ns n) es
    | otherwise = g

-- | Remove edge @e@ from @g@.
removeEdge :: EdgeId -> Graph a b -> Graph a b
removeEdge e (Graph ns es) = Graph ns (delFromAL es e)

-- | Update @n@'s payload, applying @f@ on it.
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

-- | Update @e@'s payload, applying @f@ on it.
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

-- | Return a list of all node id's from from @g@.
nodes :: Graph a b -> [NodeId]
nodes (Graph ns _) = keysAL ns

-- | Return a list of all edge id's from from @g@.
edges :: Graph a b -> [EdgeId]
edges (Graph _ es) = keysAL es

-- | Return a list of all edges with @n@ as a source node.
edgesFromNode :: Graph a b -> NodeId -> [EdgeId]
edgesFromNode g n = filter (\e -> sourceOf g e == Just n) (edges g)

-- | Return a list of all edges with @n@ as a target node.
edgesIntoNode :: Graph a b -> NodeId -> [EdgeId]
edgesIntoNode g n = filter (\e -> targetOf g e == Just n) (edges g)

-- | Return a list of all nodes that are target of any edge going out from @n@.
nodesFromNode :: Graph a b -> NodeId -> [NodeId]
nodesFromNode g n = filter (\v -> isAdjacentTo g n v) (nodes g)

-- | Return a list of all nodes that are source of any edge going into @n@.
nodesIntoNode :: Graph a b -> NodeId -> [NodeId]
nodesIntoNode g n = filter (\v -> isAdjacentTo g v n) (nodes g)

-- | Return a list of all neighbour nodes from @n@.
neighbourNodes :: Graph a b -> NodeId -> [NodeId]
neighbourNodes g n = nub $ nodesIntoNode g n ++ nodesFromNode g n

-- | Return @n@'s payload.
nodePayload :: Graph a b -> NodeId -> Maybe a
nodePayload g n = (lookup n $ nodeMap g) >>= getNodePayload

-- | Return a list of all node id's, together with their payloads.
nodesWithPayload :: Graph a b -> [(NodeId, Maybe a)]
nodesWithPayload (Graph nodeMap _) =
    map (\(k, n) -> (k, getNodePayload n)) nodeMap

-- | Return @e@'s payload.
edgePayload :: Graph a b -> EdgeId -> Maybe b
edgePayload g e = (lookup e $ edgeMap g) >>= getEdgePayload

-- | Return a list of all edge id's, together with their payloads.
edgesWithPayload :: Graph a b -> [(EdgeId, Maybe b)]
edgesWithPayload (Graph _ edgeMap) =
    map (\(k, e) -> (k, getEdgePayload e)) edgeMap

-- | Return a pair containing @e@'s source and target nodes.
nodesConnectedTo :: Graph a b -> EdgeId -> Maybe (NodeId, NodeId)
nodesConnectedTo g@(Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge src tgt _) -> Just (src, tgt)
        otherwise -> Nothing

-- | Return @e@'s source.
sourceOf :: Graph a b -> EdgeId -> Maybe NodeId
sourceOf (Graph _ es) e =
    case res of
        Just ed -> Just $ getSource ed
        _ -> Nothing
  where
    res = lookup e es

-- | Return @e@'s target.
targetOf :: Graph a b -> EdgeId -> Maybe NodeId
targetOf (Graph _ es) e =
    case res of
        (Just ed) -> Just $ getTarget ed
        otherwise -> Nothing
  where
    res = lookup e es

-- | Test whether a graph is empty.
null :: Graph a b -> Bool
null (Graph [] []) = True
null _ = False

-- | Test if @n@ is a node from graph @g@.
isNodeOf :: Graph a b -> NodeId -> Bool
isNodeOf g n  = n `elem` (nodes g)

-- | Test if @e@ is an edge from graph @g@.
isEdgeOf :: Graph a b -> EdgeId -> Bool
isEdgeOf g e  = e `elem` (edges g)

-- | Test if @n1@ and @n2@ are adjacent.
isAdjacentTo :: Graph a b -> NodeId -> NodeId -> Bool
isAdjacentTo g n1 n2 =
    any (\e -> nodesConnectedTo g e == Just (n1,n2)) (edges g)

-- | Test if @n@ is connected to edge @e@.
isIncidentTo :: Graph a b -> NodeId -> EdgeId -> Bool
isIncidentTo g n e =
    case res of
        Just (s, t) -> n == s || n == t
        otherwise   -> False
  where
    res = nodesConnectedTo g e

-- | Return a list of all incident edges on @n@.
incidentEdges :: Graph a b -> NodeId -> [EdgeId]
incidentEdges g n = nub $ edgesIntoNode g n ++ edgesFromNode g n

instance Valid (Graph a b) where
    valid g =
        all (\e ->
                let src = sourceOf g e
                    tgt = targetOf g e
                in case (src, tgt) of
                    (Just s, Just t) -> isNodeOf g s && isNodeOf g t
                    otherwise -> False)
            (edges g)
