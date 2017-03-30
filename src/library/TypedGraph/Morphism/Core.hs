{-# LANGUAGE TypeFamilies #-}
module TypedGraph.Morphism.Core where

import           Abstract.Morphism   as M
import           Abstract.Valid
import           Data.List           (nub)
import           Data.Maybe          (fromMaybe, isJust)
import           Graph.Graph
import           Graph.GraphMorphism (GraphMorphism)
import qualified Graph.GraphMorphism as GM
import           TypedGraph.Graph

data TypedGraphMorphism a b = TypedGraphMorphism {
                              getDomain   :: TypedGraph a b
                            , getCodomain :: TypedGraph a b
                            , mapping     :: GraphMorphism a b
                         } deriving (Eq, Show)

-- | Given two @TypedGraph@s @G1@ and @G2@ and a simple @GraphMorphism@ between them, it returns a @TypedGraphMorphism@ from @G1@ to @G2@
buildTypedGraphMorphism :: TypedGraph a b -> TypedGraph a b -> GraphMorphism a b -> TypedGraphMorphism a b
buildTypedGraphMorphism = TypedGraphMorphism

instance Morphism (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = TypedGraph a b

    domain = getDomain
    codomain = getCodomain
    compose t1 t2 = TypedGraphMorphism (domain t1) (codomain t2) $ compose (mapping t1) (mapping t2)
    id t = TypedGraphMorphism t t (M.id $ domain t)
    isMonomorphism = isMonomorphism . mapping
    isEpimorphism = isEpimorphism . mapping
    isIsomorphism = isIsomorphism . mapping

instance Valid (TypedGraphMorphism a b) where
    validate (TypedGraphMorphism dom cod m) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , ensure (dom == compose m cod) "Morphism doesn't preserve typing"
        ]

-- | Return the nodes in the domain of a given @TypedGraphMorphism@
nodesFromDomain :: TypedGraphMorphism a b -> [NodeId]
nodesFromDomain = nodeIds . domain . getDomain

-- | Return the edges in the domain of a given @TypedGraphMorphism@
edgesFromDomain :: TypedGraphMorphism a b -> [EdgeId]
edgesFromDomain = edgeIds . domain . getDomain

-- | Return the nodes in the codomain of a given @TypedGraphMorphism@
nodesFromCodomain :: TypedGraphMorphism a b -> [NodeId]
nodesFromCodomain = nodeIds . domain . getCodomain

-- | Return the edges in the codomain of a given @TypedGraphMorphism@
edgesFromCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgesFromCodomain = edgeIds . domain . getCodomain

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and a node @__n__@ in @G1@, it returns the node in @G2@ to which @__n__@ gets mapped
applyNode :: TypedGraphMorphism a b -> NodeId -> Maybe NodeId
applyNode tgm = GM.applyNode (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and an edge @__e__@ in @G1@, it returns the edge in @G2@ to which @__e__@ gets mapped
applyEdge :: TypedGraphMorphism a b -> EdgeId -> Maybe EdgeId
applyEdge tgm = GM.applyEdge (mapping tgm)

-- | Return the domain graph
graphDomain :: TypedGraphMorphism a b -> Graph a b
graphDomain = untypedGraph . domain

-- | Return the codomain graph
graphCodomain :: TypedGraphMorphism a b -> Graph a b
graphCodomain = untypedGraph . codomain

-- | Given a @TypedGraphMorphism@ @__t__@and a node @n@ in the domain of @__t__@, return the node in the image
--of @t@ to which @n@ gets mapped or error in the case of undefined
applyNodeUnsafe :: TypedGraphMorphism a b -> NodeId -> NodeId
applyNodeUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

-- | Given a @TypedGraphMorphism@ @__t__@and an edge @e@ in the domain of @__t__@, return the edge in the image
--of @t@ to which @e@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: TypedGraphMorphism a b -> EdgeId -> EdgeId
applyEdgeUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

-- | Given a @TypedGraphMorphism@, return its orphan nodes
orphanTypedNodes :: TypedGraphMorphism a b -> [NodeId]
orphanTypedNodes tgm = GM.orphanNodes (mapping tgm)

-- | Given a @TypedGraphMorphism@, return its orphan edges
orphanTypedEdges :: TypedGraphMorphism a b -> [EdgeId]
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

-- | Test if a @nac@ is partial injective (injective out of @m@)
isPartialInjective :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
isPartialInjective nac q = GM.isPartialInjective (mapping nac) (mapping q)

-- | Creates a TypedGraphMorphism mapping the same elements of theirs codomains, from @tgm1@ to @tgm2@
idMap :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> TypedGraphMorphism a b
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

    newNodes gm = map (GM.applyNodeUnsafe gm) (nodeIds (domain gm))
    newEdges gm = map (\x -> (GM.applyEdgeUnsafe gm x, GM.applyNodeUnsafe gm (sourceOfUnsafe (domain gm) x), GM.applyNodeUnsafe gm (targetOfUnsafe (domain gm) x))) (edgeIds $ domain gm)

    newDomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (codomain gmDomain)) (newNodes gmDomain)) (newEdges gmDomain)
    newCodomain = foldr (\(e,s,t) -> GM.createEdgeOnDomain e s t e) (foldr (\x -> GM.createNodeOnDomain x x) (GM.empty empty (codomain gmCodomain)) (newNodes gmCodomain)) (newEdges gmCodomain)

    newMaps = GM.buildGraphMorphism (domain newDomain) (domain newCodomain) (map (\(NodeId x) -> (x,x)) (nodeIds $ domain newDomain)) (map (\(EdgeId x) -> (x,x)) (edgeIds $ domain newDomain))
  in buildTypedGraphMorphism newDomain newCodomain newMaps

-- | Given a TypedGraphMorphism tgm, creates an isomorphic TypedGraphMorphism tgm' where the nodes and edges in the domain have the same ids
-- as the ones in the codomain
reflectIdsFromCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
reflectIdsFromCodomain tgm =
  let
    typedA = domain tgm
    typedB = codomain tgm
    typeGraph = codomain typedA
    typedB' = GM.empty empty typeGraph
    nodes = nodesFromDomain tgm
    edges = edgesFromDomain tgm
    initial = buildTypedGraphMorphism typedB' typedB (GM.empty (domain typedB') (domain typedB))
    addNodes = foldr (\n -> createNodeOnDomain (applyNodeUnsafe tgm n) (GM.applyNodeUnsafe typedA n) (applyNodeUnsafe tgm n)) initial nodes
    addEdges = foldr (\e ->
      createEdgeOnDomain (applyEdgeUnsafe tgm e)
                         (applyNodeUnsafe tgm (sourceOfUnsafe (domain typedA) e))
                         (applyNodeUnsafe tgm (targetOfUnsafe (domain typedA) e))
                         (GM.applyEdgeUnsafe typedA e)
                         (applyEdgeUnsafe tgm e)) addNodes edges
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

    newNodes = nub (nodesWithType typedL ++ nodesWithType typedD)
    newEdges = nub (edgesWithType typedL ++ edgesWithType typedD)

    typedG' = foldr (\(e,s,ta,ty) -> GM.createEdgeOnDomain e s ta ty)
                      (foldr (uncurry GM.createNodeOnDomain) (GM.empty empty typeGraph) newNodes)
                    newEdges
    nodeR n = if isJust (applyNode m' n) then (n, applyNodeUnsafe m' n) else (n, applyNodeUnsafe e' n)
    edgeR e = if isJust (applyEdge m' e) then (e, applyEdgeUnsafe m' e) else (e, applyEdgeUnsafe e' e)

    nodeRelation = map nodeR (nodesFromDomain m')
    edgeRelation = map edgeR (edgesFromDomain m')

    initial = buildTypedGraphMorphism typedG typedG' (GM.empty (domain typedG) (domain typedG'))

    h' = foldr (uncurry updateEdgeRelation) (foldr (uncurry untypedUpdateNodeRelation) initial nodeRelation) edgeRelation
   in (compose m h', compose e h')
