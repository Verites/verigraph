module Graph.Graph (
    -- * Types
      Edge
    , EdgeId (..)
    , Node
    , NodeId (..)
    , Graph(..)

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
    , edgesWithPayload
    , incidentEdges
    , nodesOf
    , nodesWithPayload

    -- * Query
    , Graph.Graph.null
    , nodePayload
    , edgePayload
    , sourceOf
    , sourceOfUnsafe
    , targetOf
    , targetOfUnsafe
    -- ** Predicates
    , isEdgeOf
    , isNodeOf
    , isAdjacentTo
    , isIncidentTo

    --
    , newNodes
    , newEdges
) where

import           Abstract.Cardinality
import           Abstract.Valid
import           Data.List
import           Data.List.Utils
import           Data.Maybe      (fromMaybe)

data Node n = Node { getNodePayload :: Maybe n
              } deriving (Show, Read)


data Edge e = Edge { getSource      :: NodeId
                   , getTarget      :: NodeId
                   , getEdgePayload :: Maybe e
              } deriving (Show, Read)


data Graph n e = Graph {
    nodeMap :: [(NodeId, Node n)],
    edgeMap :: [(EdgeId, Edge e)]
    } deriving (Read)

-- | Verify equality of two lists ignoring order
eq :: (Eq t) => [t] -> [t] -> Bool
eq a b = contained a b && contained b a

contained :: Eq t => [t] -> [t] -> Bool
contained a b = False `notElem` map (`elem` b) a

instance Eq (Graph n e) where
    (Graph nodeMap1 edgeMap1) == (Graph nodeMap2 edgeMap2) =
      let
        simplifyNode (nodeId, _) = nodeId
        simplifyEdge (edgeId, edge) = (edgeId, getSource edge, getTarget edge)
      in
       eq (map simplifyNode nodeMap1) (map simplifyNode nodeMap2) &&
       eq (map simplifyEdge edgeMap1) (map simplifyEdge edgeMap2)


instance Show (Graph n e) where
    show (Graph nm em) =
              "Nodes:\n" ++
              concatMap (\(n, _) -> "\t" ++ show n ++ "\n") nm ++
              "Edges:\n" ++
              concatMap (\(eid, _) -> "\t" ++ show eid ++ "\n") em

instance Cardinality (Graph n e) where
  cardinality = cardinality'


newtype NodeId = NodeId Int deriving (Eq, Ord, Read)
newtype EdgeId = EdgeId Int deriving (Eq, Ord, Read)

instance Show NodeId where
    show (NodeId i) = show i

instance Show EdgeId where
    show (EdgeId i) = show i

---- Convenient instances of Number and Enum classes for NodeId and EdgeId
instance Num NodeId where
  (NodeId x)  +  (NodeId y) = NodeId (x+y)
  (NodeId x)  *  (NodeId y) = NodeId (x*y)
  (NodeId x)  -  (NodeId y) = NodeId (x-y)
  negate (NodeId x) = NodeId (negate x)
  signum (NodeId x) = NodeId (signum x)
  fromInteger x       = NodeId $ fromIntegral x
  abs (NodeId x)    = NodeId (abs x)

instance Enum NodeId where
  toEnum = NodeId
  fromEnum (NodeId x) = x

instance Num EdgeId where
  (EdgeId x)  +  (EdgeId y) = EdgeId (x+y)
  (EdgeId x)  *  (EdgeId y) = EdgeId (x*y)
  (EdgeId x)  -  (EdgeId y) = EdgeId (x-y)
  negate (EdgeId x) = EdgeId (negate x)
  signum (EdgeId x) = EdgeId (signum x)
  fromInteger x       = EdgeId $ fromIntegral x
  abs (EdgeId x)    = EdgeId (abs x)

instance Enum EdgeId where
  toEnum = EdgeId
  fromEnum (EdgeId x) = x

-- | Infinite list of new node instances of a graph
newNodes :: Graph n e -> [NodeId]
newNodes g = [succ maxNode..]
  where maxNode = foldr max 0 (nodes g)

-- | Infinite list of new edge instances of a graph
newEdges :: Graph n e -> [EdgeId]
newEdges g = [succ maxEdge..]
  where maxEdge = foldr max 0 (edges g)

-- | Create an empty Graph.
empty :: Graph n e
empty = Graph [] []

-- | Build a Graph
build :: [Int] -> [(Int,Int,Int)] -> Graph n e
build n   = foldr ((\(a,b,c) -> insertEdge a b c) . (\(a,b,c) -> (EdgeId a, NodeId b, NodeId c))) g
    where
        g = foldr (insertNode . NodeId) empty n

-- | Insert a node @n@ in a graph @g@, without payload.
insertNode :: NodeId -> Graph n e -> Graph n e
insertNode n (Graph ns es) =
    Graph (addToAL ns n (Node Nothing)) es

-- | Insert a node @n@ in a graph @g@ with payload @p@.
insertNodeWithPayload :: NodeId -> n -> Graph n e -> Graph n e
insertNodeWithPayload n p (Graph ns es) =
    Graph (addToAL ns n (Node (Just p))) es

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@, without payload.
insertEdge :: EdgeId -> NodeId -> NodeId -> Graph n e -> Graph n e
insertEdge e src tgt g@(Graph ns es)
    | src `elem` keysAL ns && tgt `elem` keysAL ns =
        Graph ns (addToAL es e (Edge src tgt Nothing))
    | otherwise = g

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@ with payload @p@.
insertEdgeWithPayload :: EdgeId -> NodeId -> NodeId -> e -> Graph n e -> Graph n e
insertEdgeWithPayload e src tgt p g@(Graph ns es)
    | src `elem` keysAL ns && tgt `elem` keysAL ns =
        Graph ns (addToAL es e (Edge src tgt (Just p)))
    | otherwise = g

-- | If @n@ exists in @g@, and there are no incident edges on it, remove it.
-- Return @g@ otherwise.
removeNode :: NodeId -> Graph n e -> Graph n e
removeNode n g@(Graph ns es)
    | Prelude.null $ incidentEdges g n = Graph (delFromAL ns n) es
    | otherwise = g

-- | Remove edge @e@ from @g@.
removeEdge :: EdgeId -> Graph n e -> Graph n e
removeEdge e (Graph ns es) = Graph ns (delFromAL es e)

-- | Update @n@'s payload, applying @f@ on it.
updateNodePayload :: NodeId -> Graph n e -> (n -> n) -> Graph n e
updateNodePayload n g@(Graph ns es) f =
    case nd of
        Nothing -> g
        Just n' ->
            Graph
            (addToAL ns n $ n' { getNodePayload = f <$> p n' }) es
  where
    nd = lookup n ns
    p = getNodePayload

-- | Update @e@'s payload, applying @f@ on it.
updateEdgePayload :: EdgeId -> Graph n e -> (e -> e) -> Graph n e
updateEdgePayload e g@(Graph ns es) f =
    case ed of
        Nothing -> g
        Just e' ->
            Graph
            ns (addToAL es e $ e' { getEdgePayload = f <$> p e' })
  where
    ed = lookup e es
    p = getEdgePayload

-- | Return a list of all node id's from from @g@.
nodes :: Graph n e -> [NodeId]
nodes (Graph ns _) = keysAL ns

-- | Return a list of all edge id's from from @g@.
edges :: Graph n e -> [EdgeId]
edges (Graph _ es) = keysAL es

-- | Return a list of all edges with @n@ as a source node.
outgoingEdges :: Graph n e -> NodeId -> [EdgeId]
outgoingEdges g n = filter (\e -> sourceOf g e == Just n) (edges g)

-- | Return a list of all edges with @n@ as a target node.
incomingEdges :: Graph n e -> NodeId -> [EdgeId]
incomingEdges g n = filter (\e -> targetOf g e == Just n) (edges g)

-- | Return @n@'s payload.
nodePayload :: Graph n e -> NodeId -> Maybe n
nodePayload g n = lookup n (nodeMap g) >>= getNodePayload

-- | Return a list of all node id's, together with their payloads.
nodesWithPayload :: Graph n e -> [(NodeId, Maybe n)]
nodesWithPayload (Graph nodeMap _) =
    map (\(k, n) -> (k, getNodePayload n)) nodeMap

-- | Return @e@'s payload.
edgePayload :: Graph n e -> EdgeId -> Maybe e
edgePayload g e = lookup e (edgeMap g) >>= getEdgePayload

-- | Return a list of all edge id's, together with their payloads.
edgesWithPayload :: Graph n e -> [(EdgeId, Maybe e)]
edgesWithPayload (Graph _ edgeMap) =
    map (\(k, e) -> (k, getEdgePayload e)) edgeMap

-- | Return a pair containing @e@'s source and target nodes.
nodesOf :: Graph n e -> EdgeId -> Maybe (NodeId, NodeId)
nodesOf (Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge src tgt _) -> Just (src, tgt)
        _ -> Nothing

-- | Return @e@'s source.
sourceOf :: Graph n e -> EdgeId -> Maybe NodeId
sourceOf (Graph _ es) e =
    case res of
        Just ed -> Just $ getSource ed
        _ -> Nothing
  where
    res = lookup e es

-- | Return @e@'s target.
targetOf :: Graph n e -> EdgeId -> Maybe NodeId
targetOf (Graph _ es) e =
    case res of
        (Just ed) -> Just $ getTarget ed
        _ -> Nothing
  where
    res = lookup e es

-- | Return @e@'s source or error in the case of undefined
sourceOfUnsafe :: Graph n e -> EdgeId -> NodeId
sourceOfUnsafe g e = fromMaybe (error "Error, graph with source edges function non total") $ sourceOf g e

-- | Return @e@'s target or error in the case of undefined
targetOfUnsafe :: Graph n e -> EdgeId -> NodeId
targetOfUnsafe g e = fromMaybe (error "Error, graph with target edges function non total") $ targetOf g e


-- | Test whether a graph is empty.
null :: Graph n e -> Bool
null (Graph [] []) = True
null _ = False

-- | Given a graph, it returns the number of vertices plus the number of edges
cardinality' :: Graph n e -> Int
cardinality' g = (length (nodes g)) + (length (edges g))

-- | Test if @n@ is a node from graph @g@.
isNodeOf :: Graph n e -> NodeId -> Bool
isNodeOf g n  = n `elem` nodes g

-- | Test if @e@ is an edge from graph @g@.
isEdgeOf :: Graph n e -> EdgeId -> Bool
isEdgeOf g e  = e `elem` edges g

-- | Test if @n1@ and @n2@ are adjacent.
isAdjacentTo :: Graph n e -> NodeId -> NodeId -> Bool
isAdjacentTo g n1 n2 =
    any (\e -> nodesOf g e == Just (n1,n2)) (edges g)

-- | Test if @n@ is connected to edge @e@.
isIncidentTo :: Graph n e -> NodeId -> EdgeId -> Bool
isIncidentTo g n e =
    case res of
        Just (s, t) -> n == s || n == t
        _ -> False
  where
    res = nodesOf g e

-- | Return a list of all incident edges on @n@.
incidentEdges :: Graph n e -> NodeId -> [EdgeId]
incidentEdges g n = nub $ incomingEdges g n ++ outgoingEdges g n

instance Valid (Graph n e) where
    validate graph =
      mconcat $ map validateEdge (edgeMap graph)
      where
        validateEdge (edge, Edge src tgt _) =
          mconcat
            [ ensure (isNodeOf graph src) ("Source node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            , ensure (isNodeOf graph tgt) ("Target node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            ]
