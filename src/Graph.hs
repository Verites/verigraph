{-# LANGUAGE TypeFamilies #-}
module Graph (Graph) where

import GraphClass
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

data Graph a b = Graph [(Int, Node a)] [(Int, Edge b)]
    deriving (Show, Read, Eq)

instance GraphClass (Graph a b) where
    type Nd (Graph a b) = Int
    type Ed (Graph a b) = Int

  -- Build and modify graphs
    empty = Graph [] []

    insertNode n g@(Graph ns es) =
        Graph (addToAL ns n (Node Nothing Nothing)) es

    insertEdge e src tgt g@(Graph ns es)
        | src `elem` (keysAL ns) && tgt `elem` (keysAL ns) =
            Graph ns (addToAL es e (Edge src tgt Nothing Nothing))
        | otherwise = g

    removeNode n g@(Graph ns es)
        | null $ incidentEdges n g = Graph (delFromAL ns n) es
        | otherwise = g

    removeEdge e (Graph ns es) = Graph ns (delFromAL es e)
        
    nodes (Graph ns _) = keysAL ns        
    edges (Graph _ es) = keysAL es        
    nodesConnectedTo e g@(Graph _ es) =
        let ed = lookup e es
        in case ed of
            Just (Edge src tgt _ _) -> Just (src, tgt)
            otherwise -> Nothing

    sourceOf e (Graph _ es) =
        fmap getSource ed
        where ed = lookup e es

    targetOf e (Graph _ es) =
        fmap getTarget ed
        where ed = lookup e es

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
