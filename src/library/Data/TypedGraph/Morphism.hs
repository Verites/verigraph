{-# LANGUAGE TypeFamilies #-}
module Data.TypedGraph.Morphism where
  -- TODO: write export list explicitly

import           Base.Valid
import           Data.Graphs
import           Data.Graphs.Morphism (GraphMorphism)
import qualified Data.Graphs.Morphism as GM
import           Data.List            (nub)
import           Data.Maybe           (fromMaybe, isJust)
import           Data.TypedGraph      as TG

data TypedGraphMorphism a b = TypedGraphMorphism {
    domainGraph   :: TypedGraph a b
  , codomainGraph :: TypedGraph a b
  , mapping       :: GraphMorphism (Maybe a) (Maybe b)
} deriving (Eq, Show)

compose :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
compose t2 t1 = TypedGraphMorphism (domainGraph t1) (codomainGraph t2) $ GM.compose (mapping t2) (mapping t1)

-- | Given two @TypedGraph@s @G1@ and @G2@ and a simple @GraphMorphism@ between them, it returns a @TypedGraphMorphism@ from @G1@ to @G2@
buildTypedGraphMorphism :: TypedGraph a b -> TypedGraph a b -> GraphMorphism (Maybe a) (Maybe b) -> TypedGraphMorphism a b
buildTypedGraphMorphism = TypedGraphMorphism

fromGraphsAndLists :: TypedGraph a b -> TypedGraph a b -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> TypedGraphMorphism a b
fromGraphsAndLists dom cod nodeMapping edgeMapping = TypedGraphMorphism dom cod $
  GM.fromGraphsAndLists (untypedGraph dom) (untypedGraph cod) nodeMapping edgeMapping

instance Valid (TypedGraphMorphism a b) where
    validate (TypedGraphMorphism dom cod m) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , ensure (dom == GM.compose cod m) "Morphism doesn't preserve typing"
        ]

-- TODO: refactor these fucntion to avoid duplication, probably with untypedGraph
-- | Return the nodes ids in the domain of a given @TypedGraphMorphism@
nodeIdsFromDomain :: TypedGraphMorphism a b -> [NodeId]
nodeIdsFromDomain = nodeIds . GM.domainGraph . domainGraph

-- | Return the nodes in the domain of a given @TypedGraphMorphism@
nodesFromDomain :: TypedGraphMorphism a b -> [Node (Maybe a)]
nodesFromDomain = nodes . GM.domainGraph . domainGraph

-- | Return the edges ids in the domain of a given @TypedGraphMorphism@
edgeIdsFromDomain :: TypedGraphMorphism a b -> [EdgeId]
edgeIdsFromDomain = edgeIds . GM.domainGraph . domainGraph

-- | Return the edges in the domain of a given @TypedGraphMorphism@
edgesFromDomain :: TypedGraphMorphism a b -> [Edge (Maybe b)]
edgesFromDomain = edges . GM.domainGraph . domainGraph

-- | Return the nodes ids in the codomain of a given @TypedGraphMorphism@
nodeIdsFromCodomain :: TypedGraphMorphism a b -> [NodeId]
nodeIdsFromCodomain = nodeIds . GM.domainGraph . codomainGraph

-- | Return the edges ids in the codomain of a given @TypedGraphMorphism@
edgeIdsFromCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgeIdsFromCodomain = edgeIds . GM.domainGraph . codomainGraph

-- | Return the edges in the codomain of a given @TypedGraphMorphism@
edgesFromCodomain :: TypedGraphMorphism a b -> [Edge (Maybe b)]
edgesFromCodomain = edges . GM.domainGraph . codomainGraph

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and a node @__n__@ in @G1@, it returns the node in @G2@ to which @__n__@ gets mapped
applyNode :: TypedGraphMorphism a b -> Node (Maybe a) -> Maybe (Node (Maybe a))
applyNode tgm = GM.applyNode (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and a nodeId @__n__@ in @G1@, it returns the nodeId in @G2@ to which @__n__@ gets mapped
applyNodeId :: TypedGraphMorphism a b -> NodeId -> Maybe NodeId
applyNodeId tgm = GM.applyNodeId (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and an edge @__e__@ in @G1@, it returns the edge in @G2@ to which @__e__@ gets mapped
applyEdge :: TypedGraphMorphism a b -> Edge (Maybe b) -> Maybe (Edge (Maybe b))
applyEdge tgm = GM.applyEdge (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and an edgeId @__e__@ in @G1@, it returns the edgeId in @G2@ to which @__e__@ gets mapped
applyEdgeId :: TypedGraphMorphism a b -> EdgeId -> Maybe EdgeId
applyEdgeId tgm = GM.applyEdgeId (mapping tgm)

-- | Return the domain graph
graphDomain :: TypedGraphMorphism a b -> Graph (Maybe a) (Maybe b)
graphDomain = untypedGraph . domainGraph

-- | Return the codomain graph
graphCodomain :: TypedGraphMorphism a b -> Graph (Maybe a) (Maybe b)
graphCodomain = untypedGraph . codomainGraph

-- | Given a @TypedGraphMorphism@ @__t__@and a node @n@ in the domain of @__t__@, return the node in the image
--of @t@ to which @n@ gets mapped or error in the case of undefined
applyNodeUnsafe :: TypedGraphMorphism a b -> Node (Maybe a) -> Node (Maybe a)
applyNodeUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

-- | Given a @TypedGraphMorphism@ @__t__@and a nodeId @n@ in the domain of @__t__@, return the nodeId in the image
--of @t@ to which @n@ gets mapped or error in the case of undefined
applyNodeIdUnsafe :: TypedGraphMorphism a b -> NodeId -> NodeId
applyNodeIdUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNodeId m n

-- | Given a @TypedGraphMorphism@ @__t__@and an edge @e@ in the domain of @__t__@, return the edge in the image
--of @t@ to which @e@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: TypedGraphMorphism a b -> Edge (Maybe b) -> Edge (Maybe b)
applyEdgeUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

-- | Given a @TypedGraphMorphism@ @__t__@and an edgeId @e@ in the domain of @__t__@, return the edgeId in the image
--of @t@ to which @e@ gets mapped or error in the case of undefined
applyEdgeIdUnsafe :: TypedGraphMorphism a b -> EdgeId -> EdgeId
applyEdgeIdUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdgeId m e

-- | Return the nodes ids of the codomain which are not in the image of the given morphism.
orphanTypedNodeIds :: TypedGraphMorphism a b -> [NodeId]
orphanTypedNodeIds tgm = GM.orphanNodeIds (mapping tgm)

-- | Return the edge ids of the codomain which are not in the image of the given morphism.
orphanTypedEdgeIds :: TypedGraphMorphism a b -> [EdgeId]
orphanTypedEdgeIds tgm = GM.orphanEdgeIds (mapping tgm)

-- | Return the edges of the codomain which are not in the image of the given morphism.
orphanTypedEdges :: TypedGraphMorphism a b -> [Edge (Maybe b)]
orphanTypedEdges tgm = GM.orphanEdges (mapping tgm)

-- | Invert a typed graph morphism
invert :: TypedGraphMorphism a b -> TypedGraphMorphism a b
invert tgm =
  TypedGraphMorphism { domainGraph = codomainGraph tgm
                     , codomainGraph = domainGraph tgm
                     , mapping = GM.invertGraphMorphism (mapping tgm)
                     }

-- | This function adds an edge e1 (with source s1, target t1 and type tp) to the domain of the typed graph morphism, and associate it to e2
--   It assumes s1, t1, e2, tp already exist, and that e1 does not exist.
createEdgeOnDomain :: EdgeId -> NodeId -> NodeId -> EdgeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnDomain e1 s1 t1 tp e2 tgm =
  tgm { domainGraph = GM.createEdgeOnDomain e1 s1 t1 tp (domainGraph tgm)
      , mapping = GM.createEdgeOnDomain e1 s1 t1 e2 (mapping tgm)
      }

-- | This function adds an edge e2 (with source s2, target t2 and type tp) to the codomain of the typed graph morphism
--   It assumes s2, t2, tp already exist, and that e2 does not exist.
createEdgeOnCodomain :: EdgeId -> NodeId -> NodeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnCodomain e2 s2 t2 tp tgm =
  tgm { codomainGraph = GM.createEdgeOnDomain e2 s2 t2 tp (codomainGraph tgm)
      , mapping = GM.createEdgeOnCodomain e2 s2 t2 (mapping tgm)
      }

-- | This function adds a node n1 (type tp) to the domain of the typed graph morphism, and associate it to n2
--   It assumes n2 and tp already exist, and that n1 does not exist.
createNodeOnDomain :: NodeId -> NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnDomain n1 tp n2 tgm =
  tgm { domainGraph = GM.createNodeOnDomain n1 tp (domainGraph tgm)
      , mapping = GM.createNodeOnDomain n1 n2 (mapping tgm)
      }

-- | This function adds a node n2 (type tp) to the codomain of the typed graph morphism
--   It assumes tp already exist, and that n2 does not exist.
createNodeOnCodomain :: NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnCodomain n2 tp tgm =
  tgm { codomainGraph = GM.createNodeOnDomain n2 tp (codomainGraph tgm)
      , mapping = GM.createNodeOnCodomain n2 (mapping tgm)
      }

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist.
updateNodeRelation :: NodeId -> NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateNodeRelation n1 n2 tp tgm =
  TypedGraphMorphism { domainGraph = GM.updateNodeRelation n1 tp (domainGraph tgm)
                     , codomainGraph = GM.updateNodeRelation n2 tp (codomainGraph tgm)
                     , mapping = GM.updateNodeRelation n1 n2 (mapping tgm)
                     }

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist and are of the same type.
untypedUpdateNodeRelation :: NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
untypedUpdateNodeRelation n1 n2 tgm =
  TypedGraphMorphism { domainGraph = domainGraph tgm
                     , codomainGraph = codomainGraph tgm
                     , mapping = GM.updateNodeRelation n1 n2 (mapping tgm)
                     }

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelation :: EdgeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateEdgeRelation e1 e2 tgm =
  tgm { mapping = GM.updateEdgeRelation e1 e2 (mapping tgm) }

-- | Remove a node from the domain of a typed graph morphism
removeNodeFromDomain :: NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromDomain n tgm =
  tgm { domainGraph = GM.removeNodeFromDomain n (domainGraph tgm)
      , mapping = GM.removeNodeFromDomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromDomain :: EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeFromDomain e tgm =
  tgm { domainGraph = GM.removeEdgeFromDomain e (domainGraph tgm)
      , mapping = GM.removeEdgeFromDomain e (mapping tgm)
      }

-- | Remove a node from the codomain of a typed graph morphism
removeNodeFromCodomain :: NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromCodomain n tgm =
  tgm { codomainGraph = GM.removeNodeFromDomain n (codomainGraph tgm)
      , mapping = GM.removeNodeFromCodomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromCodomain :: EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeFromCodomain e tgm =
  tgm { codomainGraph = GM.removeEdgeFromDomain e (codomainGraph tgm)
      , mapping = GM.removeEdgeFromCodomain e (mapping tgm) }

-- | Creates a TypedGraphMorphism mapping nodes and edges according to their identifiers.
makeInclusion :: TypedGraph a b -> TypedGraph a b -> TypedGraphMorphism a b
makeInclusion g1 g2 =
  fromGraphsAndLists g1 g2
    [ (n, n) | n <- nodeIds (untypedGraph g1) ]
    [ (e, e) | e <- edgeIds (untypedGraph g1) ]

-- | Given a TypedGraphMorphism tgm, creates an isomorphic TypedGraphMorphism tgm' where the mapping between the domain and codomain can be seen as explicit inclusion (the same ids)
-- Attention: It works only when the typing morphism is injective, otherwise it will produce an invalid TypedGraphMorphism
reflectIdsFromTypeGraph :: TypedGraphMorphism a b -> TypedGraphMorphism a b
reflectIdsFromTypeGraph tgm =
  let
    gmDomain = domainGraph tgm
    gmCodomain = codomainGraph tgm

    newNodes gm = map (GM.applyNodeIdUnsafe gm) (nodeIds (untypedGraph gm))
    newEdges gm = map (\x -> (GM.applyEdgeIdUnsafe gm (edgeId x), GM.applyNodeIdUnsafe gm (sourceId x), GM.applyNodeIdUnsafe gm (targetId x))) (edges $ untypedGraph gm)

    newDomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (untypedGraph gmDomain)) (newNodes gmDomain)) (newEdges gmDomain)
    newCodomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (untypedGraph gmCodomain)) (newNodes gmCodomain)) (newEdges gmCodomain)

    newMaps = GM.buildGraphMorphism (untypedGraph newDomain) (untypedGraph newCodomain) (map (\(NodeId x) -> (x,x)) (nodeIds $ untypedGraph newDomain)) (map (\(EdgeId x) -> (x,x)) (edgeIds $ untypedGraph newDomain))
  in buildTypedGraphMorphism newDomain newCodomain newMaps

-- | Given a TypedGraphMorphism tgm, creates an isomorphic TypedGraphMorphism tgm' where the nodes
-- and edges in the domain have the same ids as the ones in the codomain
reflectIdsFromCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
reflectIdsFromCodomain tgm =
  let
    typedA = domainGraph tgm
    typedB = codomainGraph tgm
    typeGraph = TG.typeGraph typedA
    typedB' = GM.empty empty typeGraph
    nodes = nodeIdsFromDomain tgm
    edges = edgesFromDomain tgm
    initial = buildTypedGraphMorphism typedB' typedB (GM.empty (untypedGraph typedB') (untypedGraph typedB))
    addNodes = foldr (\n -> createNodeOnDomain (applyNodeIdUnsafe tgm n) (GM.applyNodeIdUnsafe typedA n) (applyNodeIdUnsafe tgm n)) initial nodes
    addEdges = foldr (\e ->
      createEdgeOnDomain (applyEdgeIdUnsafe tgm (edgeId e))
                         (applyNodeIdUnsafe tgm (sourceId e))
                         (applyNodeIdUnsafe tgm (targetId e))
                         (GM.applyEdgeIdUnsafe typedA (edgeId e))
                         (applyEdgeIdUnsafe tgm (edgeId e))) addNodes edges
   in addEdges

reflectIdsFromDomains :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
reflectIdsFromDomains (m,e) =
  let
    typedL = domainGraph m
    typedD = domainGraph e
    typedG = codomainGraph m
    typeGraph = GM.codomainGraph typedL
    m' = invert m
    e' = invert e

    newNodes = nub (typedNodes typedL ++ typedNodes typedD)
    newEdges = nub (typedEdges typedL ++ typedEdges typedD)

    typedG' = foldr (\(e,s,ta,ty) -> GM.createEdgeOnDomain e s ta ty)
                      (foldr (uncurry GM.createNodeOnDomain) (GM.empty empty typeGraph) newNodes)
                    newEdges
    nodeR n = if isJust (applyNodeId m' n) then (n, applyNodeIdUnsafe m' n) else (n, applyNodeIdUnsafe e' n)
    edgeR e = if isJust (applyEdgeId m' e) then (e, applyEdgeIdUnsafe m' e) else (e, applyEdgeIdUnsafe e' e)

    nodeRelation = map nodeR (nodeIdsFromDomain m')
    edgeRelation = map edgeR (edgeIdsFromDomain m')

    initial = buildTypedGraphMorphism typedG typedG' (GM.empty (untypedGraph typedG) (untypedGraph typedG'))

    h' = foldr (uncurry updateEdgeRelation) (foldr (uncurry untypedUpdateNodeRelation) initial nodeRelation) edgeRelation
   in (compose h' m, compose h' e)
