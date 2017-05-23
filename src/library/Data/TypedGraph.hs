module Data.TypedGraph
  ( TypedGraph
  , buildTypedGraph
  , emptyTypedGraph
  , fromGraphMorphism
  , toGraphMorphism
  , untypedGraph
  , extractNodeType
  , extractEdgeType
  , typeGraph
  , Data.TypedGraph.null
  , newTypedNodes
  , newTypedEdges
  , typedNodes
  , typedEdges
  , untypedNodes
  , untypedEdges
  , createNodeOnBaseGraph
  , createEdgeOnBaseGraph
  , updateNodeTyping
  , removeNodeFromBaseGraph
  , removeEdgeFromBaseGraph
  ) where

import           Base.Cardinality
import           Abstract.Valid
import           Data.Graphs          as G
import           Data.Graphs.Morphism as GM
import           Data.Maybe           (fromMaybe)
import           Data.Relation        as R


-- | A typed graph is a morphism whose codomain is the type graph.
newtype TypedGraph a b = TypedGraph { toGraphMorphism :: GraphMorphism a b}

fromGraphMorphism :: GraphMorphism a b -> TypedGraph a b
fromGraphMorphism = TypedGraph

instance Cardinality (GraphMorphism a b) where
  cardinality = cardinality . domainGraph

instance Eq (TypedGraph a b) where
  t1 == t2 = toGraphMorphism t1 == toGraphMorphism t2

instance Show (TypedGraph a b) where
  show m = show (toGraphMorphism m)

-- TODO: adapt messages to fit typed graph semantics
instance Valid (TypedGraph a b) where
    validate typedGraph =
      mconcat
        [ withContext "base graph" (validate dom)
        , withContext "type graph" (validate cod)
        , ensure (R.isFunctional nodeMap) "The relation of nodes is not functional"
        , ensure (R.isTotal nodeMap) "The function of nodes is not total on its domain"
        , ensure (R.isFunctional edgeMap) "The relation of edges is not functional"
        , ensure (R.isTotal edgeMap) "The function of edges is not total on its domain"
        , ensure incidencePreserved "The morphism doesn't preserve incidence/adjacency"
        ]
      where
        morphism@(GraphMorphism dom cod nodeMap edgeMap) = toGraphMorphism typedGraph
        incidencePreserved =
          all
            (\(Edge e domSrc domTgt _) ->
                (G.sourceOf cod =<< applyEdgeId morphism e) == applyNodeId morphism domSrc
                  && (G.targetOf cod =<< applyEdgeId morphism e) == applyNodeId morphism domTgt)
            (G.edges dom)

-- | Construct a graph morphism
buildTypedGraph :: Graph a b -> Graph a b -> [(Int,Int)] -> [(Int,Int)] -> TypedGraph a b
buildTypedGraph base typing n e = TypedGraph (buildGraphMorphism base typing n e)

-- | It receives a type graph and returns a typed graph where the base is empty
emptyTypedGraph :: Graph a b -> TypedGraph a b
emptyTypedGraph tg = TypedGraph (GM.empty G.empty tg)

-- | Obtain the untyped version of the typed graph
untypedGraph :: TypedGraph a b -> Graph a b
untypedGraph = domainGraph . toGraphMorphism

-- | Obtain the type graph from a typed graph
typeGraph :: TypedGraph a b -> Graph a b
typeGraph = codomainGraph . toGraphMorphism

-- | Test if the typed graph is empty
null :: TypedGraph a b -> Bool
null = G.null . untypedGraph

extractNodeType :: TypedGraph a b -> NodeId -> NodeId
extractNodeType gm n = fromMaybe (error "Node not typed") $ applyNodeId (toGraphMorphism gm) n

extractEdgeType :: TypedGraph a b -> EdgeId -> EdgeId
extractEdgeType gm e = fromMaybe (error "edge not typed") $ applyEdgeId (toGraphMorphism gm) e

-- | Infinite list of new node instances of a typed graph
newTypedNodes :: TypedGraph a b -> [NodeId]
newTypedNodes tg = newNodes $ untypedGraph tg

-- | Infinite list of new edge instances of a typed graph
newTypedEdges :: TypedGraph a b -> [EdgeId]
newTypedEdges tg = newEdges $ untypedGraph tg

-- | Obtain a list of tuples @(nodeId, typeId)@ for nodes in the graph.
typedNodes :: TypedGraph a b -> [(NodeId, NodeId)]
typedNodes tg = map withType $ nodeIds (untypedGraph tg)
  where withType node = (node, extractNodeType tg node)

-- | Obtain a list of tuples @(edgeId, srcId, tgtId, typeId)@ for edges in the graph.
typedEdges :: TypedGraph a b -> [(EdgeId, NodeId, NodeId, EdgeId)]
typedEdges tg = map withType $ edges graph
  where graph = untypedGraph tg
        withType edge = (edgeId edge, sourceId edge, targetId edge, extractEdgeType tg (edgeId edge))

-- | Obtain the list of untyped nodes, i.e., the list of node ids from the typed graph domain
untypedNodes :: TypedGraph a b -> [NodeId]
untypedNodes tg = nodeIds $ untypedGraph tg

-- | Obtain the list of untyped edges, i.e., the list of edge ids from the typed graph domain
untypedEdges :: TypedGraph a b -> [EdgeId]
untypedEdges tg = edgeIds $ untypedGraph tg

-- TODO: document this functions
createEdgeOnBaseGraph :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> TypedGraph a (Maybe b)
                      -> TypedGraph a (Maybe b)
createEdgeOnBaseGraph e1 s1 t1 e2 tg = TypedGraph (createEdgeOnDomain e1 s1 t1 e2 (toGraphMorphism tg))

createNodeOnBaseGraph :: G.NodeId -> G.NodeId -> TypedGraph (Maybe a) b -> TypedGraph (Maybe a) b
createNodeOnBaseGraph n1 n2 tg = TypedGraph (createNodeOnDomain n1 n2 (toGraphMorphism tg))

updateNodeTyping :: G.NodeId -> G.NodeId -> TypedGraph (Maybe a) b -> TypedGraph (Maybe a) b
updateNodeTyping node typing tg = TypedGraph (updateNodeRelation node typing (toGraphMorphism tg))

-- | Removes a node from the base graph, unless it has incident edges.
removeNodeFromBaseGraph :: G.NodeId -> TypedGraph a b -> TypedGraph a b
removeNodeFromBaseGraph n tg = TypedGraph (removeNodeFromDomain n (toGraphMorphism tg))

-- | Removes an edge from the domain of the morphism
removeEdgeFromBaseGraph :: G.EdgeId -> TypedGraph a b -> TypedGraph a b
removeEdgeFromBaseGraph e tg = TypedGraph (removeEdgeFromDomain e (toGraphMorphism tg))
