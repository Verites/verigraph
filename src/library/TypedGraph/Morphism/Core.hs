{-# LANGUAGE TypeFamilies #-}
module TypedGraph.Morphism.Core where

import           Abstract.Valid
import           Category.FinitaryCategory as FC
import           Data.List                 (nub)
import           Data.Maybe                (fromMaybe, isJust)
import           Object.Graph
import           Graph.GraphMorphism       (GraphMorphism)
import qualified Graph.GraphMorphism       as GM
import           Object.TypedGraph

data TypedGraphMorphism a b = TypedGraphMorphism {
                              getDomain   :: TypedGraph a b
                            , getCodomain :: TypedGraph a b
                            , mapping     :: GraphMorphism (Maybe a) (Maybe b)
                         } deriving (Eq, Show)

-- | Given two @TypedGraph@s @G1@ and @G2@ and a simple @GraphMorphism@ between them, it returns a @TypedGraphMorphism@ from @G1@ to @G2@
buildTypedGraphMorphism :: TypedGraph a b -> TypedGraph a b -> GraphMorphism (Maybe a) (Maybe b) -> TypedGraphMorphism a b
buildTypedGraphMorphism = TypedGraphMorphism

instance FinitaryCategory (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = TypedGraph a b

    domain = getDomain
    codomain = getCodomain
    t2 <&> t1 = TypedGraphMorphism (domain t1) (codomain t2) $ mapping t2 <&> mapping t1
    identity t = TypedGraphMorphism t t (FC.identity $ domain t)
    isMonomorphism = isMonomorphism . mapping
    isEpimorphism = isEpimorphism . mapping
    isIsomorphism = isIsomorphism . mapping


instance Valid (TypedGraphMorphism a b) where
    validate (TypedGraphMorphism dom cod m) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , ensure (dom == cod <&> m) "Morphism doesn't preserve typing"
        ]

-- | Return the nodes ids in the domain of a given @TypedGraphMorphism@
nodeIdsFromDomain :: TypedGraphMorphism a b -> [NodeId]
nodeIdsFromDomain = nodeIds . domain . getDomain

-- | Return the nodes in the domain of a given @TypedGraphMorphism@
nodesFromDomain :: TypedGraphMorphism a b -> [Node (Maybe a)]
nodesFromDomain = nodes . domain . getDomain

-- | Return the edges ids in the domain of a given @TypedGraphMorphism@
edgeIdsFromDomain :: TypedGraphMorphism a b -> [EdgeId]
edgeIdsFromDomain = edgeIds . domain . getDomain

-- | Return the edges in the domain of a given @TypedGraphMorphism@
edgesFromDomain :: TypedGraphMorphism a b -> [Edge (Maybe b)]
edgesFromDomain = edges . domain . getDomain

-- | Return the nodes ids in the codomain of a given @TypedGraphMorphism@
nodeIdsFromCodomain :: TypedGraphMorphism a b -> [NodeId]
nodeIdsFromCodomain = nodeIds . domain . getCodomain

-- | Return the edges ids in the codomain of a given @TypedGraphMorphism@
edgeIdsFromCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgeIdsFromCodomain = edgeIds . domain . getCodomain

-- | Return the edges in the codomain of a given @TypedGraphMorphism@
edgesFromCodomain :: TypedGraphMorphism a b -> [Edge (Maybe b)]
edgesFromCodomain = edges . domain . getCodomain

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
graphDomain = untypedGraph . domain

-- | Return the codomain graph
graphCodomain :: TypedGraphMorphism a b -> Graph (Maybe a) (Maybe b)
graphCodomain = untypedGraph . codomain

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

-- | Given a @TypedGraphMorphism@, return its orphan nodes ids
orphanTypedNodeIds :: TypedGraphMorphism a b -> [NodeId]
orphanTypedNodeIds tgm = GM.orphanNodeIds (mapping tgm)

-- | Given a @TypedGraphMorphism@, return its orphan edges ids
orphanTypedEdgeIds :: TypedGraphMorphism a b -> [EdgeId]
orphanTypedEdgeIds tgm = GM.orphanEdgeIds (mapping tgm)

-- | Given a @TypedGraphMorphism@, return its orphan edges
orphanTypedEdges :: TypedGraphMorphism a b -> [Edge (Maybe b)]
orphanTypedEdges tgm = GM.orphanEdges (mapping tgm)

-- | Invert a typed graph morphism
invert :: TypedGraphMorphism a b -> TypedGraphMorphism a b
invert tgm =
  TypedGraphMorphism { getDomain = codomain tgm
                     , getCodomain = domain tgm
                     , mapping = GM.invertGraphMorphism (mapping tgm)
                     }

-- | This function adds an edge e1 (with source s1, target t1 and type tp) to the domain of the typed graph morphism, and associate it to e2
--   It assumes s1, t1, e2, tp already exist, and that e1 does not exist.
createEdgeOnDomain :: EdgeId -> NodeId -> NodeId -> EdgeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnDomain e1 s1 t1 tp e2 tgm =
  tgm { getDomain = GM.createEdgeOnDomain e1 s1 t1 tp (domain tgm)
      , mapping = GM.createEdgeOnDomain e1 s1 t1 e2 (mapping tgm)
      }

-- | This function adds an edge e2 (with source s2, target t2 and type tp) to the codomain of the typed graph morphism
--   It assumes s2, t2, tp already exist, and that e2 does not exist.
createEdgeOnCodomain :: EdgeId -> NodeId -> NodeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnCodomain e2 s2 t2 tp tgm =
  tgm { getCodomain = GM.createEdgeOnDomain e2 s2 t2 tp (codomain tgm)
      , mapping = GM.createEdgeOnCodomain e2 s2 t2 (mapping tgm)
      }

-- | This function adds a node n1 (type tp) to the domain of the typed graph morphism, and associate it to n2
--   It assumes n2 and tp already exist, and that n1 does not exist.
createNodeOnDomain :: NodeId -> NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnDomain n1 tp n2 tgm =
  tgm { getDomain = GM.createNodeOnDomain n1 tp (domain tgm)
      , mapping = GM.createNodeOnDomain n1 n2 (mapping tgm)
      }

-- | This function adds a node n2 (type tp) to the codomain of the typed graph morphism
--   It assumes tp already exist, and that n2 does not exist.
createNodeOnCodomain :: NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnCodomain n2 tp tgm =
  tgm { getCodomain = GM.createNodeOnDomain n2 tp (codomain tgm)
      , mapping = GM.createNodeOnCodomain n2 (mapping tgm)
      }

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist.
updateNodeRelation :: NodeId -> NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateNodeRelation n1 n2 tp tgm =
  TypedGraphMorphism { getDomain = GM.updateNodeRelation n1 tp (domain tgm)
                     , getCodomain = GM.updateNodeRelation n2 tp (codomain tgm)
                     , mapping = GM.updateNodeRelation n1 n2 (mapping tgm)
                     }

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist and are of the same type.
untypedUpdateNodeRelation :: NodeId -> NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
untypedUpdateNodeRelation n1 n2 tgm =
  TypedGraphMorphism { getDomain = domain tgm
                     , getCodomain = codomain tgm
                     , mapping = GM.updateNodeRelation n1 n2 (mapping tgm)
                     }

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelation :: EdgeId -> EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateEdgeRelation e1 e2 tgm =
  tgm { mapping = GM.updateEdgeRelation e1 e2 (mapping tgm) }

-- | Remove a node from the domain of a typed graph morphism
removeNodeFromDomain :: NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromDomain n tgm =
  tgm { getDomain = GM.removeNodeFromDomain n (domain tgm)
      , mapping = GM.removeNodeFromDomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromDomain :: EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeFromDomain e tgm =
  tgm { getDomain = GM.removeEdgeFromDomain e (domain tgm)
      , mapping = GM.removeEdgeFromDomain e (mapping tgm)
      }

-- | Remove a node from the codomain of a typed graph morphism
removeNodeFromCodomain :: NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromCodomain n tgm =
  tgm { getCodomain = GM.removeNodeFromDomain n (codomain tgm)
      , mapping = GM.removeNodeFromCodomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromCodomain :: EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeFromCodomain e tgm =
  tgm { getCodomain = GM.removeEdgeFromDomain e (codomain tgm)
      , mapping = GM.removeEdgeFromCodomain e (mapping tgm) }

-- | Creates a TypedGraphMorphism mapping nodes and edges according to their identifiers.
idMap :: TypedGraph a b -> TypedGraph a b -> TypedGraphMorphism a b
idMap gm1 gm2 =
  buildTypedGraphMorphism gm1 gm2 edgesUpdate
    where
      initialGraph = GM.empty (domain gm1) (domain gm2)
      nodesUpdate = foldr (\n -> GM.updateNodes n n) initialGraph (nodeIds (domain gm1))
      edgesUpdate = foldr (\e -> GM.updateEdges e e) nodesUpdate (edgeIds (domain gm2))

-- | Given a TypedGraphMorphism tgm, creates an isomorphic TypedGraphMorphism tgm' where the mapping between the domain and codomain can be seen as explicit inclusion (the same ids)
-- Attention: It works only when the typing morphism is injective, otherwise it will produce an invalid TypedGraphMorphism
reflectIdsFromTypeGraph :: TypedGraphMorphism a b -> TypedGraphMorphism a b
reflectIdsFromTypeGraph tgm =
  let
    gmDomain = domain tgm
    gmCodomain = codomain tgm

    newNodes gm = map (GM.applyNodeIdUnsafe gm) (nodeIds (domain gm))
    newEdges gm = map (\x -> (GM.applyEdgeIdUnsafe gm (edgeId x), GM.applyNodeIdUnsafe gm (sourceId x), GM.applyNodeIdUnsafe gm (targetId x))) (edges $ domain gm)

    newDomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (codomain gmDomain)) (newNodes gmDomain)) (newEdges gmDomain)
    newCodomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (codomain gmCodomain)) (newNodes gmCodomain)) (newEdges gmCodomain)

    newMaps = GM.buildGraphMorphism (domain newDomain) (domain newCodomain) (map (\(NodeId x) -> (x,x)) (nodeIds $ domain newDomain)) (map (\(EdgeId x) -> (x,x)) (edgeIds $ domain newDomain))
  in buildTypedGraphMorphism newDomain newCodomain newMaps

-- | Given a TypedGraphMorphism tgm, creates an isomorphic TypedGraphMorphism tgm' where the nodes
-- and edges in the domain have the same ids as the ones in the codomain
reflectIdsFromCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
reflectIdsFromCodomain tgm =
  let
    typedA = domain tgm
    typedB = codomain tgm
    typeGraph = codomain typedA
    typedB' = GM.empty empty typeGraph
    nodes = nodeIdsFromDomain tgm
    edges = edgesFromDomain tgm
    initial = buildTypedGraphMorphism typedB' typedB (GM.empty (domain typedB') (domain typedB))
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
    typedL = domain m
    typedD = domain e
    typedG = codomain m
    typeGraph = codomain typedL
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

    initial = buildTypedGraphMorphism typedG typedG' (GM.empty (domain typedG) (domain typedG'))

    h' = foldr (uncurry updateEdgeRelation) (foldr (uncurry untypedUpdateNodeRelation) initial nodeRelation) edgeRelation
   in (h' <&> m, h' <&> e)
