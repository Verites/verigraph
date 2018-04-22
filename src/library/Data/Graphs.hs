{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-|
An implementation of labeled directed graphs, allowing multiple parallel edges.

The implementation is based on using integer identifiers for nodes and edges, and association lists to store additional information such as payloads, sources and targets.

Operation comments contain the operation time complexity in the Big-O notation
<http://en.wikipedia.org/wiki/Big_O_notation>, denoting by /v/ the number of nodes in a graph, and
by /e/ the number of edges in a graph.
-}
module Data.Graphs (
    -- * Graph Type
      Graph(..)
    , NodeId (..)
    , EdgeId (..)
    , Node(..)
    , Edge(..)

    -- * Contexts and Graph Traversal
    -- $contexts
    , NodeInContext
    , EdgeInContext
    , NodeContext

    , contextGraph
    , incidentEdges
    , incomingEdges
    , outgoingEdges

    -- ** Neighbour nodes
    -- $neighbours

    -- * Query
    , Data.Graphs.null
    , isNodeOf
    , isEdgeOf
    , lookupNode
    , lookupNodeInContext
    , lookupEdge
    , lookupEdgeInContext
    , isAdjacentTo
    , isIncidentTo
    , nodesOf
    , sourceOf
    , sourceOfUnsafe
    , targetOf
    , targetOfUnsafe
    , getIncidentEdges

    -- * Construction
    , empty
    , build
    , fromNodesAndEdges

    -- ** Insertion
    , insertNode
    , insertNodeWithPayload
    , insertEdge
    , insertEdgeWithPayload

    -- ** Delete
    , removeNode
    , removeNodeForced
    , removeNodeAndIncidentEdges
    , removeEdge

    -- ** Update
    , updateNodePayload
    , updateEdgePayload

    -- * Conversion
    -- ** Lists
    , nodes
    , edges
    , nodeIds
    , edgeIds
    , nodesInContext
    , edgesInContext
    , newNodes
    , newEdges
) where

import           Data.Function             (on)
import           Data.List
import           Data.Maybe                (fromMaybe)
import           Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as PP

import           Base.Cardinality
import           Base.Valid
import           Util.List


-- | Type of node identifiers, which are essentially integers.
newtype NodeId =
  NodeId Int
  deriving (Eq, Ord, Num, Real, Integral, Pretty)


-- | Type of edge identifiers, which are essentially integers.
newtype EdgeId
  = EdgeId Int
  deriving (Eq, Ord, Num, Real, Integral, Pretty)


instance Show NodeId where
    show (NodeId i) = show i

instance Show EdgeId where
    show (EdgeId i) = show i

instance Enum NodeId where
  toEnum = NodeId
  fromEnum (NodeId x) = x

instance Enum EdgeId where
  toEnum = EdgeId
  fromEnum (EdgeId x) = x


-- | Nodes from within a graph.
data Node n =
  Node
    { nodeId   :: NodeId
    , nodeInfo :: n
    } deriving (Show)


-- | Edges from within a graph.
data Edge e =
  Edge
    { edgeId   :: EdgeId
    , sourceId :: NodeId
    , targetId :: NodeId
    , edgeInfo :: e
    } deriving (Show)


-- | A directed graph, allowing parallel edges. Both nodes and edges have optional payloads
-- of arbitrary types.
--
-- Every node and edge is identified by a unique integer. The "namespaces" of nodes and edges are
-- independent, i.e. the same number may be used to identify both a node and an edge within the
-- same graph.
--
-- Equality tests cost /O(v² + e²)/, and disregards the payloads.
data Graph n e =
  Graph
    { nodeMap :: [(NodeId, Node n)]
    , edgeMap :: [(EdgeId, Edge e)]
    }


-- | Verify equality of two lists ignoring order /O(m*n)/
eq :: (Eq t) => [t] -> [t] -> Bool
eq a b = contained a b && contained b a

-- /O(m*n)/
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
    show (Graph nodes edges) = concat $
        "Nodes:\n" : map showNode (sortBy (compare `on` fst) nodes)
        ++ "Edges:\n" : map showEdge (sortBy (compare `on` fst) edges)
      where
        showNode (n, _) =
          "\t" ++ show n ++ "\n"

        showEdge (e, Edge _ src tgt _) =
          "\t" ++ show e ++ " (" ++ show src ++ "->" ++ show tgt ++ ")\n"

instance (Pretty n, Pretty e) => Pretty (Graph n e) where
  pretty (Graph nodes edges) = PP.hsep
    [ "Nodes:"
    , PP.nest 2 $ PP.hsep (map pretty nodes)
    , "Edges:"
    , PP.nest 2 $ PP.hsep (map pretty edges)
    ]

instance {-# OVERLAPPABLE #-} Pretty n => Pretty (Node n) where
  pretty (Node n p) = pretty n <+> PP.brackets (pretty p)

instance {-# OVERLAPPABLE #-} Pretty e => Pretty (Edge e) where
  pretty (Edge e src tgt p) =
    PP.hsep [pretty e, PP.brackets (pretty p), ":", pretty src, "->", pretty tgt]

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
>
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
-- Thus, forcing the evaluation of the nodes costs /O(v)/. Keep this in mind when using values
-- of this type.
type EdgeInContext n e =
  (NodeInContext n e, Edge e, NodeInContext n e)


nodeInContext :: Graph n e -> Node n -> NodeInContext n e
nodeInContext graph node =
  (node, NodeCtx (nodeId node) graph)


edgeInContext :: Graph n e -> Edge e -> EdgeInContext n e
edgeInContext graph edge =
  ( nodeInContext graph (getNode (sourceId edge))
  , edge
  , nodeInContext graph (getNode (targetId edge))
  )
  where
    nodes = nodeMap graph
    getNode id = case lookup id nodes of
      Nothing -> error $ "edgeInContext: malformed graph, lookup of edge " ++ show id ++ " failed"
      Just n  -> n

-- | Get the graph that contains the node of the given context.
contextGraph :: NodeContext n e -> Graph n e
contextGraph (NodeCtx _ graph) = graph

-- | Get the edges that are incident on the current node.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incidentEdges :: NodeContext n e -> [EdgeInContext n e]
incidentEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> sourceId edge == nodeId || targetId edge == nodeId)
  . map snd
  $ edgeMap graph


-- | Get the edges that have the current node as target.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
incomingEdges :: NodeContext n e -> [EdgeInContext n e]
incomingEdges (NodeCtx nodeId graph) =
  map (edgeInContext graph)
  . filter (\edge -> targetId edge == nodeId)
  . map snd
  $ edgeMap graph


-- | Get the edges that have the current node as source.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
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

These operations were not added to the graph API because it is not yet clear which of them are commonly used, and which names would be appropriate. If you identify the need for such an operation, submit an issue on github.
-}



-- | Infinite list of fresh node identifiers for a graph. /O(v)/.
newNodes :: Graph n e -> [NodeId]
newNodes g = [succ maxNode..]
  where maxNode = foldr max 0 (nodeIds g)

-- | Infinite list of fresh edge identifiers for a graph. /O(e)/.
newEdges :: Graph n e -> [EdgeId]
newEdges g = [succ maxEdge..]
  where maxEdge = foldr max 0 (edgeIds g)

-- | Empty graph, with no nodes or edges. /O(1)/
empty :: Graph n e
empty = Graph [] []

-- | Build a graph from lists of nodes and edges. Edges with undefined source or target are ignored
-- and omitted from the resulting graph. /O(v + e*v)/
build :: [Int] -> [(Int,Int,Int)] -> Graph (Maybe n) (Maybe e)
build n   = foldr ((\(a,b,c) -> insertEdge a b c) . (\(a,b,c) -> (EdgeId a, NodeId b, NodeId c))) g
    where
        g = foldr (insertNode . NodeId) empty n


-- | Build a graph from lists of nodes and edges. Edges with undefined source or target are ignored
-- and omitted from the resulting graph. /O(v + e*v)/
fromNodesAndEdges :: [Node n] -> [Edge e] -> Graph n e
fromNodesAndEdges nodes edges =
  Graph
    [ (nodeId n, n) | n <- nodes ]
    [ (edgeId e, e)
        | e <- edges
        , any (\n -> nodeId n == sourceId e) nodes
        , any (\n -> nodeId n == targetId e) nodes
    ]

-- | Insert a node with given identifier into a graph, without payload. If a node with the given
-- identifier aready exists, its payload is removed. /O(v)/.
insertNode :: NodeId -> Graph (Maybe n) e -> Graph (Maybe n) e
insertNode n (Graph ns es) =
    Graph (insertByKey ns n (Node n Nothing)) es

-- | Insert a node with given identifier and payload into a graph. If a node with the given
-- identifier already exists, its payload is updated. /O(v)/.
insertNodeWithPayload :: NodeId -> n -> Graph n e -> Graph n e
insertNodeWithPayload n p (Graph ns es) =
    Graph (insertByKey ns n (Node n p)) es

-- | (@insertEdge e src tgt g@) will insert an edge with identifier @e@ from @src@ to @tgt@ in graph
-- @g@, without payload. If @src@ or @tgt@ are not nodes of @g@, the graph is not modified. If an
-- edge with identifier @e@ already exists, it is updated. /O(v + e)/.
insertEdge :: EdgeId -> NodeId -> NodeId -> Graph n (Maybe e) -> Graph n (Maybe e)
insertEdge e src tgt g@(Graph ns es)
    | src `elem` listKeys ns && tgt `elem` listKeys ns =
        Graph ns (insertByKey es e (Edge e src tgt Nothing))
    | otherwise = g

-- | (@insertEdgeWithPayload e src tgt p g@) will insert an edge with identifier @e@ from @src@ to
-- @tgt@ in graph @g@ with payload @p@. If @src@ or @tgt@ are not nodes of @g@, the graph is not
-- modified. If an edge with identifier @e@ already exists, it is updated. /O(v + e)/.
insertEdgeWithPayload :: EdgeId -> NodeId -> NodeId -> e -> Graph n e -> Graph n e
insertEdgeWithPayload e src tgt p g@(Graph ns es)
    | src `elem` listKeys ns && tgt `elem` listKeys ns =
        Graph ns (insertByKey es e (Edge e src tgt p))
    | otherwise = g

-- | Removes the given node from the graph, unless it has any incident edges. /O(v + e²)/.
removeNode :: NodeId -> Graph n e -> Graph n e
removeNode n g@(Graph ns es)
    | Prelude.null $ getIncidentEdges g n = Graph (deleteByKey ns n) es
    | otherwise = g

-- | Removes the given node from the graph, even if it has any incident edges.
-- It does not verify if the node has incident edges, thus it may generate invalid graphs.
removeNodeForced :: NodeId -> Graph n e -> Graph n e
removeNodeForced n (Graph ns es) = Graph (deleteByKey ns n) es

-- | Removes the given node and all incident edges from the graph. /O(v + e)/
removeNodeAndIncidentEdges :: NodeId -> Graph n e -> Graph n e
removeNodeAndIncidentEdges nodeId (Graph nodes edges) =
  Graph
    (deleteByKey nodes nodeId)
    (filter (\(_, e) -> sourceId e /= nodeId && targetId e /= nodeId) edges)

-- | Remove the given edge from the graph. /O(e)/.
removeEdge :: EdgeId -> Graph n e -> Graph n e
removeEdge e (Graph ns es) = Graph ns (deleteByKey es e)

-- | Update the node's payload, applying the given function on it. /O(v)/.
updateNodePayload :: NodeId -> Graph n e -> (n -> n) -> Graph n e
updateNodePayload nodeId graph@(Graph nodes _) f =
  case lookup nodeId nodes of
    Nothing ->
      graph

    Just node ->
      let
        updatedNode =
          node { nodeInfo = f (nodeInfo node) }
      in
        graph { nodeMap = insertByKey nodes nodeId updatedNode }


-- | Update the edge's payload, applying the function on it. /O(e)/.
updateEdgePayload :: EdgeId -> Graph n e -> (e -> e) -> Graph n e
updateEdgePayload edgeId graph@(Graph _ edges) f =
  case lookup edgeId edges of
    Nothing ->
      graph

    Just edge ->
      let
        updatedEdge =
          edge { edgeInfo = f (edgeInfo edge) }

      in
        graph { edgeMap = insertByKey edges edgeId updatedEdge }


-- | List of all node id's from from the graph. /O(v)/.
nodeIds :: Graph n e -> [NodeId]
nodeIds (Graph nodes _) = listKeys nodes


-- | List of all edge id's from from the graph. /O(e)/.
edgeIds :: Graph n e -> [EdgeId]
edgeIds (Graph _ edges) = listKeys edges


-- | List of all nodes from the graph. /O(v)/.
nodes :: Graph n e -> [Node n]
nodes (Graph nodes _) = map snd nodes


-- | List of all edges from the graph. /O(e)/.
edges :: Graph n e -> [Edge e]
edges (Graph _ edges) = map snd edges


-- | List of all nodes from the graph, along with their contexts. /O(v)/.
nodesInContext :: Graph n e -> [NodeInContext n e]
nodesInContext graph@(Graph nodes _) =
  map (nodeInContext graph . snd) nodes


-- | List of all edges from the graph, along with their contexts.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
edgesInContext :: Graph n e -> [EdgeInContext n e]
edgesInContext graph@(Graph _ edges) =
  map (edgeInContext graph . snd) edges



{-# DEPRECATED nodesOf, sourceOf, sourceOfUnsafe, targetOf, targetOfUnsafe, getIncidentEdges "This function performs unnecessary dictionary lookups. Try using lookupNode, lookupNodeInContext, nodes or nodesInContext instead." #-}

-- | Look up the node with given identifier in the graph. /O(v)/.
lookupNode :: NodeId -> Graph n e -> Maybe (Node n)
lookupNode id (Graph nodes _) =
  lookup id nodes


-- | Look up the edge with given identifier in the graph. /O(e)/.
lookupEdge :: EdgeId -> Graph n e -> Maybe (Edge e)
lookupEdge id (Graph _ edges) =
  lookup id edges


-- | Look up the node with given identifier, along with its context, in the graph. /O(v)/.
lookupNodeInContext :: NodeId -> Graph n e -> Maybe (NodeInContext n e)
lookupNodeInContext id graph =
  nodeInContext graph <$> lookupNode id graph


-- | Look up the edge with given identifier, along with its context, in the graph.
-- /O(e)/, plus the cost of evaluating the nodes of the result (see 'EdgeInContext').
lookupEdgeInContext :: EdgeId -> Graph n e -> Maybe (EdgeInContext n e)
lookupEdgeInContext id graph =
  edgeInContext graph <$> lookupEdge id graph


-- | Gets a pair containing the source and target of the given edge. /O(e)/.
nodesOf :: Graph n e -> EdgeId -> Maybe (NodeId, NodeId)
nodesOf (Graph _ es) e =
    let ed = lookup e es
    in case ed of
        Just (Edge _ src tgt _) -> Just (src, tgt)
        _                       -> Nothing


-- | Gets the source of the given edge. /O(e)/.
sourceOf :: Graph n e -> EdgeId -> Maybe NodeId
sourceOf graph e =
  fst <$> nodesOf graph e


-- | Gets the target of the given edge. /O(e)/.
targetOf :: Graph n e -> EdgeId -> Maybe NodeId
targetOf graph e =
  snd <$> nodesOf graph e


-- | Gets the source of the given edge, crashing if no such edge exists. /O(e)/.
sourceOfUnsafe :: Graph n e -> EdgeId -> NodeId
sourceOfUnsafe g e = fromMaybe (error "Error, graph with source edges function non total") $ sourceOf g e

-- | Gets the target of the given edge, crashing if no such edge exists. /O(e)/.
targetOfUnsafe :: Graph n e -> EdgeId -> NodeId
targetOfUnsafe g e = fromMaybe (error "Error, graph with target edges function non total") $ targetOf g e


-- | Test whether a graph is empty. /O(1)/.
null :: Graph n e -> Bool
null (Graph [] []) = True
null _             = False

-- | Given a graph, it returns the number of vertices plus the number of edges
cardinality' :: Graph n e -> Int
cardinality' g = length (nodes g) + length (edges g)

-- | Test if a node identifier is contained in the graph. /O(v)/.
isNodeOf :: Graph n e -> NodeId -> Bool
isNodeOf g n  = n `elem` nodeIds g

-- | Test if an edge identifier is contained in the graph. /O(e)/.
isEdgeOf :: Graph n e -> EdgeId -> Bool
isEdgeOf g e  = e `elem` edgeIds g

-- | Test if the given nodes are adjacent. /O(e²)/
isAdjacentTo :: Graph n e -> NodeId -> NodeId -> Bool
isAdjacentTo g n1 n2 =
    any (\e -> nodesOf g e == Just (n1,n2)) (edgeIds g)

-- | Test if the given edge has given node as source or target. /O(e)/.
isIncidentTo :: Graph n e -> NodeId -> EdgeId -> Bool
isIncidentTo g n e =
    case res of
        Just (s, t) -> n == s || n == t
        _           -> False
  where
    res = nodesOf g e

-- | Gets a list of all edges incident to the given node. /O(e²)/
getIncidentEdges :: Graph n e -> NodeId -> [EdgeId]
getIncidentEdges g n = nub $ getIncomingEdges g n ++ getOutgoingEdges g n


-- | Gets a list of all edges whose source is the given node. /O(e²)/.
getOutgoingEdges :: Graph n e -> NodeId -> [EdgeId]
getOutgoingEdges g n = filter (\e -> sourceOf g e == Just n) (edgeIds g)


-- | Gets a list of all edges whose target is the given node. /O(e²)/.
getIncomingEdges :: Graph n e -> NodeId -> [EdgeId]
getIncomingEdges g n = filter (\e -> targetOf g e == Just n) (edgeIds g)


instance Valid (Graph n e) where
    validate graph =
      mconcat $ map validateEdge (edgeMap graph)
      where
        validateEdge (edge, Edge _ src tgt _) =
          mconcat
            [ ensure (isNodeOf graph src) ("Source node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            , ensure (isNodeOf graph tgt) ("Target node #" ++ show src ++ " of edge #" ++ show edge ++ " is not a member of the graph")
            ]
