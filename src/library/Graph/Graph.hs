{-|
An implementation of labeled directed graphs, allowing multiple parallel edges.

The implementation is based on using integer identifiers for nodes and edges, and association lists to store additional information such as payloads, sources and targets.

Operation comments contain the operation time complexity in the Big-O notation
<http://en.wikipedia.org/wiki/Big_O_notation>, denoting by /v/ the number of nodes in a graph, by
/e/ the number of edges in a graph, and by /W/ the number of bits in an 'Int' (32 or 64).
-}
module Graph.Graph (
    -- * Graph Type
      Graph(..)
    , Edge(..)
    , EdgeId (..)
    , Node(..)
    , NodeId (..)

    -- * Contexts and Graph Traversal
    -- $contexts
    , NodeInContext
    , EdgeInContext
    , NodeContext

    , incidentEdges
    , incomingEdges
    , outgoingEdges

    -- ** Neighbour nodes
    -- $neighbours

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
    , nodes
    , edges
    , nodeIds
    , edgeIds
    , nodesInContext
    , edgesInContext
    , getIncidentEdges
    , nodesOf

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
import           Data.Maybe      (fromMaybe, fromJust)


-- | Type of node identifiers, which are essentially integers.
newtype NodeId =
  NodeId Int
  deriving (Eq, Ord)


-- | Type of edge identifiers, which are essentially integers.
newtype EdgeId
  = EdgeId Int
  deriving (Eq, Ord)


instance Show NodeId where
    show (NodeId i) = show i


instance Show EdgeId where
    show (EdgeId i) = show i


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


-- | Nodes from within a graph.
data Node n =
  Node
    { nodeId :: NodeId
    , nodeInfo :: Maybe n
    } deriving (Show)


-- | Edges from within a graph.
data Edge a =
  Edge
    { edgeId :: EdgeId
    , sourceId :: NodeId
    , targetId :: NodeId
    , edgeInfo :: Maybe a
    } deriving (Show)


-- | A directed graph, allowing parallel edges. Both nodes and edges have payloads of arbitrary
-- types.
--
-- Every node and edge is identified by a unique integer. The "namespaces" of nodes and edges are
-- independent, i.e. the same number may be used to identify both a node and an edge within the
-- same graph.
data Graph a b =
  Graph
    { nodeMap :: [(NodeId, Node a)]
    , edgeMap :: [(EdgeId, Edge b)]
    }


-- | Verify equality of two lists ignoring order
eq :: (Eq t) => [t] -> [t] -> Bool
eq a b = contained a b && contained b a

contained :: Eq t => [t] -> [t] -> Bool
contained a b = False `notElem` map (`elem` b) a

instance Eq (Graph n e) where
    (Graph nodeMap1 edgeMap1) == (Graph nodeMap2 edgeMap2) =
      let
        simplifyNode (nodeId, _) = nodeId
        simplifyEdge (edgeId, edge) = (edgeId, sourceId edge, targetId edge)
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


-- * Contexts and graph traversal
{- $contexts
In order to traverse or navigate through the graph, 'NodeContext's are provided. The context of a
node provides access to its incident edges and neighbour nodes without explicitly looking them up
in the graph. Internally, nodes and edges are still being looked up, so keep in mind there's no
performance benefit. This module, however, can guarantee that this implicit lookup will work, so
there's no need to deal with `Maybe` values.

As an example, depth-first search could be implemented as follows:

>  depthFirstSearch :: NodeInContext n e -> [Node n]
>  depthFirstSearch initialNode =
>      search IntSet.empty [initialNode]
>
>    where
>      search _ [] = []
>
>      search visitedNodes ((node, context) : rest) =
>        if node `IntSet.member` visitedNodes then
>          search visitedNodes rest
>        else
>          let
>            nextNodes =
>              [ target | (_, _, target) <- outgoingEdges context ]
>
>          in
>            node :: search (IntSet.insert (nodeId node) visitedNodes) nextNodes

-}

-- | Provides access to a node's incident edges.
data NodeContext n e =
  NodeCtx NodeId (Graph n e)


-- | Shorthand for having a node along with its context.
type NodeInContext n e =
  (Node n, NodeContext n e)


-- | Shorthand for having an edge along with its source and target in context.
--
-- Because of lazyness, constructing a value of this type does __not__ evaluate the node lookup.
-- Thus, forcing the evaluation of the nodes costs /O(min(n,W))/. Keep this in mind when using
-- values of this type.
type EdgeInContext n e =
  (NodeInContext n e, Edge e, NodeInContext n e)


nodeInContext :: Graph n e -> Node n -> NodeInContext n e
nodeInContext graph node =
  (node, NodeCtx (nodeId node) graph)


edgeInContext :: Graph n e -> Edge e -> EdgeInContext n e
edgeInContext graph edge =
  let
    nodes =
      nodeMap graph
  in
    ( nodeInContext graph (fromJust $ lookup (sourceId edge) nodes)
    , edge
    , nodeInContext graph (fromJust $ lookup (targetId edge) nodes)
    )


-- | Get the edges that are incident on the current node.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
--
-- /todo/: make more efficient?
incidentEdges :: NodeContext n e -> [EdgeInContext n e]
incidentEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> sourceId edge == nodeId || targetId edge == nodeId)
  . map snd
  $ edgeMap graph


-- | Get the edges that have the current node as target.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
--
-- /todo/: make more efficient?
incomingEdges :: NodeContext n e -> [EdgeInContext n e]
incomingEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> targetId edge == nodeId)
  . map snd
  $ edgeMap graph


-- | Get the edges that have the current node as source.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
--
-- /todo/: make more efficient?
outgoingEdges :: NodeContext n e -> [EdgeInContext n e]
outgoingEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> sourceId edge == nodeId)
  . map snd
  $ edgeMap graph



{- $neighbours

In order to access the neighbour nodes of the edges, one may simply go through the edges, e.g.

>   map (\(_, _, target) -> target) . outgoingEdges
>   map (\(source, _, _) -> source) . incomingEdges

These operations were not added to the digraph API because it is not yet clear which of them are commonly used, and which names would be appropriate. If you identify the need for such an operation, submit an issue on github.
-}



-- | Infinite list of new node identifiers of a graph
newNodes :: Graph n e -> [NodeId]
newNodes g = [succ maxNode..]
  where maxNode = foldr max 0 (nodeIds g)

-- | Infinite list of new edge identifiers of a graph
newEdges :: Graph n e -> [EdgeId]
newEdges g = [succ maxEdge..]
  where maxEdge = foldr max 0 (edgeIds g)

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
    Graph (addToAL ns n (Node n Nothing)) es

-- | Insert a node @n@ in a graph @g@ with payload @p@.
insertNodeWithPayload :: NodeId -> n -> Graph n e -> Graph n e
insertNodeWithPayload n p (Graph ns es) =
    Graph (addToAL ns n (Node n (Just p))) es

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@, without payload.
insertEdge :: EdgeId -> NodeId -> NodeId -> Graph n e -> Graph n e
insertEdge e src tgt g@(Graph ns es)
    | src `elem` keysAL ns && tgt `elem` keysAL ns =
        Graph ns (addToAL es e (Edge e src tgt Nothing))
    | otherwise = g

-- | Insert an edge @e@ from @src@ to @tgt@ in graph @g@ with payload @p@.
insertEdgeWithPayload :: EdgeId -> NodeId -> NodeId -> e -> Graph n e -> Graph n e
insertEdgeWithPayload e src tgt p g@(Graph ns es)
    | src `elem` keysAL ns && tgt `elem` keysAL ns =
        Graph ns (addToAL es e (Edge e src tgt (Just p)))
    | otherwise = g

-- | If @n@ exists in @g@, and there are no incident edges on it, remove it.
-- Return @g@ otherwise.
removeNode :: NodeId -> Graph n e -> Graph n e
removeNode n g@(Graph ns es)
    | Prelude.null $ getIncidentEdges g n = Graph (delFromAL ns n) es
    | otherwise = g

-- | Remove edge @e@ from @g@.
removeEdge :: EdgeId -> Graph n e -> Graph n e
removeEdge e (Graph ns es) = Graph ns (delFromAL es e)

-- | Update the node's payload, applying the given function on it.
updateNodePayload :: NodeId -> Graph n e -> (n -> n) -> Graph n e
updateNodePayload nodeId graph@(Graph nodes _) f =
  case lookup nodeId nodes of
    Nothing ->
      graph

    Just node ->
      let
        updatedNode =
          node { nodeInfo = f <$> nodeInfo node }
      in
        graph { nodeMap = addToAL nodes nodeId updatedNode }


-- | Update the edge's payload, applying the function on it.
updateEdgePayload :: EdgeId -> Graph n e -> (e -> e) -> Graph n e
updateEdgePayload edgeId graph@(Graph _ edges) f =
  case lookup edgeId edges of
    Nothing ->
      graph

    Just edge ->
      let
        updatedEdge =
          edge { edgeInfo = f <$> edgeInfo edge }

      in
        graph { edgeMap = addToAL edges edgeId updatedEdge }


-- | Return a list of all node id's from from the graph.
nodeIds :: Graph n e -> [NodeId]
nodeIds (Graph nodes _) = keysAL nodes


-- | Return a list of all edge id's from from the graph.
edgeIds :: Graph n e -> [EdgeId]
edgeIds (Graph _ edges) = keysAL edges


-- | Return a list of all nodes from the graph.
nodes :: Graph n e -> [Node n]
nodes (Graph nodes _) = map snd nodes


-- | Return a list of all edges from the graph.
edges :: Graph n e -> [Edge e]
edges (Graph _ edges) = map snd edges


-- | Return a list of all nodes from the graph, along with their contexts.
nodesInContext :: Graph n e -> [NodeInContext n e]
nodesInContext graph@(Graph nodes _) =
  map (nodeInContext graph . snd) nodes


-- | Return a list of all edges from the graph, along with their contexts.
edgesInContext :: Graph n e -> [EdgeInContext n e]
edgesInContext graph@(Graph _ edges) =
  map (edgeInContext graph . snd) edges


-- | Return a list of all edges with @n@ as a source node.
getOutgoingEdges :: Graph n e -> NodeId -> [EdgeId]
getOutgoingEdges g n = filter (\e -> sourceOf g e == Just n) (edgeIds g)


-- | Return a list of all edges with @n@ as a target node.
getIncomingEdges :: Graph n e -> NodeId -> [EdgeId]
getIncomingEdges g n = filter (\e -> targetOf g e == Just n) (edgeIds g)


-- | Return @n@'s payload.
nodePayload :: Graph n e -> NodeId -> Maybe n
nodePayload g n = lookup n (nodeMap g) >>= nodeInfo


-- | Return @e@'s payload.
edgePayload :: Graph n e -> EdgeId -> Maybe e
edgePayload g e = lookup e (edgeMap g) >>= edgeInfo


-- | Return a pair containing @e@'s source and target nodes.
nodesOf :: Graph n e -> EdgeId -> Maybe (NodeId, NodeId)
nodesOf (Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge _ src tgt _) -> Just (src, tgt)
        _ -> Nothing


-- | Return @e@'s source.
sourceOf :: Graph n e -> EdgeId -> Maybe NodeId
sourceOf graph e =
  fst <$> nodesOf graph e


-- | Return @e@'s target.
targetOf :: Graph n e -> EdgeId -> Maybe NodeId
targetOf graph e =
  snd <$> nodesOf graph e


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
isNodeOf g n  = n `elem` nodeIds g

-- | Test if @e@ is an edge from graph @g@.
isEdgeOf :: Graph n e -> EdgeId -> Bool
isEdgeOf g e  = e `elem` edgeIds g

-- | Test if @n1@ and @n2@ are adjacent.
isAdjacentTo :: Graph n e -> NodeId -> NodeId -> Bool
isAdjacentTo g n1 n2 =
    any (\e -> nodesOf g e == Just (n1,n2)) (edgeIds g)

-- | Test if @n@ is connected to edge @e@.
isIncidentTo :: Graph n e -> NodeId -> EdgeId -> Bool
isIncidentTo g n e =
    case res of
        Just (s, t) -> n == s || n == t
        _ -> False
  where
    res = nodesOf g e

-- | Return a list of all incident edges on @n@.
getIncidentEdges :: Graph n e -> NodeId -> [EdgeId]
getIncidentEdges g n = nub $ getIncomingEdges g n ++ getOutgoingEdges g n

instance Valid (Graph n e) where
    validate graph =
      mconcat $ map validateEdge (edgeMap graph)
      where
        validateEdge (edge, Edge _ src tgt _) =
          mconcat
            [ ensure (isNodeOf graph src) ("Source node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            , ensure (isNodeOf graph tgt) ("Target node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            ]
