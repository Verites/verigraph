{-# OPTIONS_GHC -fno-warn-orphans #-}
module TypedGraph.Morphism (
      TypedGraphMorphism
    , idMap
    , isPartialInjective
    , invert
    , nodesFromDomain
    , edgesFromDomain
    , nodesFromCodomain
    , edgesFromCodomain
    , graphDomain
    , graphCodomain
    , mapping
    , applyNode
    , applyEdge
    , buildTypedGraphMorphism
    , checkDeletion
    , removeNodeFromDomain
    , removeEdgeFromDomain
    , removeNodeFromCodomain
    , removeEdgeFromCodomain
    , applyNodeUnsafe
    , applyEdgeUnsafe
    , createEdgeOnDomain
    , createEdgeOnCodomain
    , createNodeOnDomain
    , createNodeOnCodomain
    , updateEdgeRelation
    , updateNodeRelation
    , orphanTypedNodes
    , orphanTypedEdges
) where

import           Abstract.AdhesiveHLR
import           Abstract.Morphism                               as M
import           Data.List                                       as L
import           Data.Maybe
import           Graph.Graph                                     as G
import qualified Graph.GraphMorphism                             as GM
import           TypedGraph.Graph
import           TypedGraph.MorphismCore
import           TypedGraph.Partitions.GraphPartition            (generateGraphPartitions)
import           TypedGraph.Partitions.GraphPartitionToVerigraph (mountTypedGraphMorphisms)
import           TypedGraph.Partitions.VerigraphToGraphPartition (createDisjointUnion,
                                                                  createSatisfyingNacsDisjointUnion)


-- | Return the domain graph
graphDomain :: TypedGraphMorphism a b -> Graph a b
graphDomain = untypedGraph . domain

-- | Return the codomain graph
graphCodomain :: TypedGraphMorphism a b -> Graph a b
graphCodomain = untypedGraph . codomain

-- | Given a @TypedGraphMorphism@ @__t__@and a node @n@ in the domain of @__t__@, return the node in the image
--of @t@ to which @n@ gets mapped or error in the case of undefined
applyNodeUnsafe :: TypedGraphMorphism a b -> G.NodeId -> G.NodeId
applyNodeUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

-- | Given a @TypedGraphMorphism@ @__t__@and an edge @e@ in the domain of @__t__@, return the edge in the image
--of @t@ to which @e@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: TypedGraphMorphism a b -> G.EdgeId -> G.EdgeId
applyEdgeUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

-- | Given a @TypedGraphMorphism@, return its orphan nodes
orphanTypedNodes :: TypedGraphMorphism a b -> [G.NodeId]
orphanTypedNodes tgm = GM.orphanNodes (mapping tgm)

-- | Given a @TypedGraphMorphism@, return its orphan edges
orphanTypedEdges :: TypedGraphMorphism a b -> [G.EdgeId]
orphanTypedEdges tgm = GM.orphanEdges (mapping tgm)

-- | Invert a typed graph morphism
invert :: TypedGraphMorphism a b -> TypedGraphMorphism a b
invert tgm =
  TypedGraphMorphism { getDomain = codomain tgm
                     , getCodomain = codomain tgm
                     , mapping = GM.invertGraphMorphism (mapping tgm)
                     }

-- | This function adds an edge e1 (with source s1, target t1 and type tp) to the domain of the typed graph morphism, and associate it to e2
--   It assumes s1, t1, e2, tp already exist, and that e1 does not exist.
createEdgeOnDomain :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnDomain e1 s1 t1 tp e2 tgm =
  tgm { getDomain = GM.createEdgeOnDomain e1 s1 t1 tp (domain tgm)
      , mapping = GM.createEdgeOnDomain e1 s1 t1 e2 (mapping tgm)
      }

-- | This function adds an edge e2 (with source s2, target t2 and type tp) to the codomain of the typed graph morphism
--   It assumes s2, t2, tp already exist, and that e2 does not exist.
createEdgeOnCodomain :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeOnCodomain e2 s2 t2 tp tgm =
  tgm { getCodomain = GM.createEdgeOnDomain e2 s2 t2 tp (codomain tgm)
      , mapping = GM.createEdgeOnCodomain e2 s2 t2 (mapping tgm)
      }

-- | This function adds a node n1 (type tp) to the domain of the typed graph morphism, and associate it to n2
--   It assumes n2 and tp already exist, and that n1 does not exist.
createNodeOnDomain :: G.NodeId -> G.NodeId -> G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnDomain n1 tp n2 tgm =
  tgm { getDomain = GM.createNodeOnDomain n1 tp (domain tgm)
      , mapping = GM.createNodeOnDomain n1 n2 (mapping tgm)
      }

-- | This function adds a node n2 (type tp) to the codomain of the typed graph morphism
--   It assumes tp already exist, and that n2 does not exist.
createNodeOnCodomain :: G.NodeId -> G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createNodeOnCodomain n2 tp tgm =
  tgm { getCodomain = GM.createNodeOnDomain n2 tp (codomain tgm)
      , mapping = GM.createNodeOnCodomain n2 (mapping tgm)
      }

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist.
updateNodeRelation :: G.NodeId -> G.NodeId -> G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateNodeRelation n1 n2 tp tgm =
  TypedGraphMorphism { getDomain = GM.updateNodeRelation n1 tp (domain tgm)
                     , getCodomain = GM.updateNodeRelation n2 tp (codomain tgm)
                     , mapping = GM.updateNodeRelation n1 n2 (mapping tgm)
                     }

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelation :: G.EdgeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateEdgeRelation e1 e2 tgm =
  tgm { mapping = GM.updateEdgeRelation e1 e2 (mapping tgm) }

-- | Remove a node from the domain of a typed graph morphism
removeNodeFromDomain :: G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromDomain n tgm =
  tgm { getDomain = GM.removeNodeFromDomain n (domain tgm)
      , mapping = GM.removeNodeFromDomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromDomain :: G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeFromDomain e tgm =
  tgm { getDomain = GM.removeEdgeFromDomain e (domain tgm)
      , mapping = GM.removeEdgeFromDomain e (mapping tgm)
      }

-- | Remove a node from the codomain of a typed graph morphism
removeNodeFromCodomain :: G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeFromCodomain n tgm =
  tgm { getCodomain = GM.removeNodeFromDomain n (codomain tgm)
      , mapping = GM.removeNodeFromCodomain n (mapping tgm)
      }

-- | Remove an edge from the domain of a typed graph morphism
removeEdgeFromCodomain :: G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
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
      nodesUpdate = foldr (\n -> GM.updateNodes n n) initialGraph (G.nodes (domain gm1))
      edgesUpdate = foldr (\e -> GM.updateEdges e e) nodesUpdate (G.edges (domain gm2))

instance AdhesiveHLR (TypedGraphMorphism a b) where

  {-
          g
      A──────▶C
      │       │
     f│   =   │g'
      ▼       ▼
      B──────▶D
          g'

     PO algorithm:
     1. invert r
     2. compose k and r^-1
     3. create node table  (C -> D)
     5. create edge table  (C -> D)
     4. associate nodes
     6. associate edges
  -}

  calculatePushout f g =
    let
        gf = compose (invert g) f                                 -- invert g and compose with f, obtain gf : C -> B
        cOnlyNodes = orphanTypedNodes g                           -- nodes in C that are not mapped from g
        cOnlyEdges = orphanTypedEdges g                           -- edges in C that are not mapped from g
        nodesInF'  = zip cOnlyNodes (newTypedNodes $ codomain gf) -- table mapping NodeIds in C to NodeIds in D
        edgesInF'  = zip cOnlyEdges (newTypedEdges $ codomain gf) -- table mapping EdgeIds in C to EdgeIds in D

        -- generate new node instances in D, associating them to the "created" nodes in C
        gf' = generateNewNodeInstances gf nodesInF'

        -- query the instance graphs C
        typemor = domain         gf'                     -- typemor is the typed graph (C -> T)
        d       = domain         typemor                 -- d  is the instance graph C
        mp      = mapping        gf'                     -- mp is the mapping of gf'  : (C -> B'), where B' = B + new nodes
        s1 e = fromJust $ G.sourceOf d e                 -- obtain source of e in C
        t1 e = fromJust $ G.targetOf d e                 -- obtain target of e in C
        s2 e = fromJust $ GM.applyNode mp (s1 e)         -- obtain source of m'(e) in D
        t2 e = fromJust $ GM.applyNode mp (t1 e)         -- obtain target of m'(e) in D
        tp e = fromJust $ GM.applyEdge typemor e         -- obtain type of e in C

        -- generate new edge table with new information
        edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e, t2 e, tp e)) edgesInF'

        -- create new morphism adding all edges
        kr''      = generateNewEdgeInstances gf' edgeTable'
    in (kr'', idMap (codomain f) (codomain kr''))

  {-
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  calculatePushoutComplement m l =
    let ml       = compose l m                                                         -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdge $ mapping m) (orphanTypedEdges l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNode $ mapping m) (orphanTypedNodes l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeFromCodomain                                          -- delete all edges, then all nodes from ml
                       (foldr removeEdgeFromCodomain ml delEdges)
                           delNodes
    in (k, idMap (codomain k) (codomain m))

  monomorphicPullback f g = (delNodesFromF', delNodesFromG')
    where
      f' = invert f
      g' = invert g
      nodes = nodesFromDomain f'
      edges = edgesFromDomain f'
      knodes = filter (\n -> isJust (applyNode f' n) && isJust (applyNode g' n)) nodes
      kedges = filter (\e -> isJust (applyEdge f' e) && isJust (applyEdge g' e)) edges
      delNodes = nodes \\ knodes
      delEdges = edges \\ kedges
      delEdgesFromF' = foldr removeEdgeFromDomain f' delEdges
      delNodesFromF' = foldr removeNodeFromDomain delEdgesFromF' delNodes
      delEdgesFromG' = foldr removeEdgeFromDomain g' delEdges
      delNodesFromG' = foldr removeNodeFromDomain delEdgesFromG' delNodes

  hasPushoutComplement (Monomorphism, g) (_, f) =
    satisfiesDanglingCondition f g

  hasPushoutComplement (_, g) (_, f) =
    satisfiesDanglingCondition f g && satisfiesIdentificationCondition f g

generateNewNodeInstances :: TypedGraphMorphism a b -> [(NodeId, NodeId)] -> TypedGraphMorphism a b
generateNewNodeInstances gf =
  foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (domain gf) a
                       in updateNodeRelation a b tp tgm) gf

generateNewEdgeInstances :: TypedGraphMorphism a b -> [(EdgeId, NodeId, NodeId, EdgeId, NodeId, NodeId, EdgeId)]
  -> TypedGraphMorphism a b
generateNewEdgeInstances =
  foldr (\(a,_,_,b,sb,tb,tp) tgm -> updateEdgeRelation a b (createEdgeOnCodomain b sb tb tp tgm) )

---- Gluing Conditions

-- | Return True if the match @m@ satifies the identification condition for existence of
-- a pushout complement
satisfiesIdentificationCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesIdentificationCondition l m =
  all (==True) (notIdentificatedNodes ++ notIdentificatedEdges)
  where
    notIdentificatedNodes =
      map (notIdentificatedElement l m nodesFromDomain applyNode) (nodesFromCodomain m)
    notIdentificatedEdges =
      map (notIdentificatedElement l m edgesFromDomain applyEdge) (edgesFromCodomain m)

-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/, two functions /domain/ (to get all elements
-- in the domain of m) and /apply/ (for applying an element in a TypedGraphMorphism),
-- and an element __/e/__ (that can be either a __/Node/__ or an __/Edge/__) it returns true if /e/ is
-- identificated (i.e. __e__ has more than one incident element on itself and at least one of them deletes it)
notIdentificatedElement :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> [t])
                         -> (TypedGraphMorphism a b -> t -> Maybe t) -> t -> Bool
notIdentificatedElement l m domain apply e = (length incidentElements <= 1) || not eIsDeleted
  where
    incidentElements = [a | a <- domain m, apply m a == Just e]
    l' = apply (invert l)
    eIsDeleted = Nothing `elem` map l' incidentElements

-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/, returns @true@ if
-- there aren't dangling edges
satisfiesDanglingCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesDanglingCondition l m = all (==True) (concat incidentDeletedEdges)
    where
        lhs = graphDomain m
        instanceGraph = graphCodomain m
        checkEdgeDeletion = map (checkDeletion l m applyEdge edgesFromDomain)
        matchedNodes = mapMaybe (applyNode m) (nodes lhs)
        deletedNodes = filter (checkDeletion l m applyNode nodesFromDomain) matchedNodes
        incidentEdgesOnDeletedNodes = map (incidentEdges instanceGraph) deletedNodes
        incidentDeletedEdges = map checkEdgeDeletion incidentEdgesOnDeletedNodes

-- | TODO: Find a better name for this function, that was repeated both here and in the GraphRule archive
-- | Given the left-hand-side morphism of a rule /l : K -> L/, a match /m : L -> G/ for this rule, an element __/e/__
-- (that can be either a __/Node/__ or an __/Edge/__) and two functions /apply/ (for applying that element in a TypedGraphMorphism) and
-- /list/ (to get all the corresponding elements in the domain of m), it returns true if /e/ is deleted by this rule for the given match
checkDeletion :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t]) -> t -> Bool
checkDeletion l m apply list e = elementInL && not elementInK
  where
    elementInL = any (\x -> apply m x == Just e) (list m)
    kToG = compose l m
    elementInK = any (\x -> apply kToG x == Just e) (list kToG)

instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createJointlyEpimorphicPairs inj m1 m2 = map (mountTypedGraphMorphisms m1 m2) (generateGraphPartitions (createDisjointUnion (m1,inj) (m2,inj)))

  createAllSubobjects inj m1 = map fst part
    where
      m2 = GM.buildGraphMorphism G.empty G.empty [] []
      part = map (mountTypedGraphMorphisms m1 m2) (generateGraphPartitions (createDisjointUnion (m1,inj) (m2,inj)))

  createJointlyEpimorphicPairsFromNAC conf r nac =
    map (mountTypedGraphMorphisms r (codomain nac))
        (generateGraphPartitions (createSatisfyingNacsDisjointUnion (r, injectiveMatch) (nac, totalInjectiveNac)))
    where
      injectiveMatch = matchRestriction conf == MonoMatches
      totalInjectiveNac = nacSatisfaction conf == MonomorphicNAC

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes,
  -- considering they have same domain
  -- and flags indicating the injective of each morphism
  calculateCommutativeSquaresAlongMonomorphism (m1,inj1) (m2,inj2) = commutativePairs
    where
      codomain1 = codomain m1
      codomain2 = codomain m2
      allPairs = map (mountTypedGraphMorphisms codomain1 codomain2)
                     (generateGraphPartitions (createDisjointUnion (codomain1,inj1) (codomain2,inj2)))
      commutativePairs = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs


instance FindMorphism (TypedGraphMorphism a b) where
  findMorphisms = findMatches
  partialInjectiveMatches = partialInjectiveMatches'

---------------------------------------------------------------------------------

-- | Finds matches __/q/__ .
--
--   Partially injective. (Injective out of __/m/__)
partialInjectiveMatches' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
partialInjectiveMatches' nac match = do
  let
    lhsNodes = nodes $ domain $ domain match
    lhsEdges = edges $ domain $ domain match
    q = preBuildQ nac match
    q' = preBuildEdges q nac match lhsEdges
    q'' = case q' of
      Just q1 -> preBuildNodes q1 nac match lhsNodes
      Nothing -> Nothing

  case q'' of
    Nothing -> []
    Just q2 -> completeMappings Monomorphism q2 (sourceNodes, sourceEdges) (targetNodes, targetEdges)
      where
        notMappedNodes tgm node = isNothing $ applyNode tgm node
        notMappedEdges tgm edge = isNothing $ applyEdge tgm edge
        sourceNodes = filter (notMappedNodes q2) (nodes $ domain $ domain q2)
        sourceEdges = filter (notMappedEdges q2) (edges $ domain $ domain q2)
        targetNodes = orphanTypedNodes q2
        targetEdges = orphanTypedEdges q2

preBuildQ :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
preBuildQ nac match = buildTypedGraphMorphism qDomain qCodomain qMapping
  where
    qDomain   = codomain nac
    qCodomain = codomain match
    qMapping  = GM.empty (domain qDomain) (domain qCodomain)

--VERIFY EDGES MAPPING N <- l AND L -> G AND BUILD A N -> G
--PARTIAL EDGES MORPHISM
preBuildEdges :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [G.EdgeId] -> Maybe (TypedGraphMorphism a b)
preBuildEdges tgm _ _ [] = Just tgm
preBuildEdges tgm nac match (h:t) = do
  let nacEdge = applyEdgeUnsafe nac h
      matchEdge   = applyEdgeUnsafe match h
      (dom, cod, _) = decomposeTypedGraphMorphism tgm
      tgm' = if (extractEdgeType dom nacEdge == extractEdgeType cod matchEdge) &&
                (isNothing (applyEdge tgm nacEdge) || (applyEdge tgm nacEdge == Just matchEdge))
             then Just $ buildTypedGraphMorphism dom cod (GM.updateEdges nacEdge matchEdge $ mapping tgm)
             else Nothing
  case tgm' of
    Just tgm'' -> preBuildEdges tgm'' nac match t
    Nothing    -> Nothing

--VERIFY NODE MAPPINGS N <- L AND L -> G AND BUILD A N -> G
--PARTIAL NODES MORPHISM
preBuildNodes :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [G.NodeId] -> Maybe (TypedGraphMorphism a b)
preBuildNodes tgm _   _     []    = Just tgm
preBuildNodes tgm nac match (h:t) = do
  let nacNode   = applyNodeUnsafe nac h
      matchNode = applyNodeUnsafe match h
      (tgmDomain, tgmCodomain, tgmMapping) = decomposeTypedGraphMorphism tgm
      tgm' = if (extractNodeType tgmDomain nacNode == extractNodeType tgmCodomain matchNode) &&
                (isNothing (applyNode tgm nacNode) || (applyNode tgm nacNode == Just matchNode))
             then Just $ buildTypedGraphMorphism tgmDomain tgmCodomain (GM.updateNodes nacNode matchNode tgmMapping)
             else Nothing
  case tgm' of
    Just tgm'' -> preBuildNodes tgm'' nac match t
    Nothing    -> Nothing

---------------------------------------------------------------------------------

-- | Finds matches __/m/__
--
--   Injective, surjective, isomorphic or all possible matches
findMatches :: MorphismType -> GM.GraphMorphism a b-> GM.GraphMorphism a b -> [TypedGraphMorphism a b]
findMatches prop graph1 graph2 =
  completeMappings prop tgm (sourceNodes, sourceEdges) (targetNodes, targetEdges)
  where
    sourceNodes = nodes $ domain graph1
    targetNodes = nodes $ domain graph2
    sourceEdges = edges $ domain graph1
    targetEdges = edges $ domain graph2

    d   = graph1
    c   = graph2
    m   = GM.empty (domain graph1) (domain graph2)
    tgm = buildTypedGraphMorphism d c m

---------------------------------------------------------------------------------

type ExpandedGraph = ([G.NodeId], [G.EdgeId])

-- | Given a TypedGraphMorphism @tgm@ from (A -> T) to (B -> T) and the two ExpandedGraphs of A and B, it completes the @tgm@
-- with all the possible mappings from (A -> T) to (B -> T)
completeMappings :: MorphismType -> TypedGraphMorphism a b -> ExpandedGraph -> ExpandedGraph -> [TypedGraphMorphism a b]
completeMappings prop tgm ([], []) targetGraph = completeFromEmptySource prop tgm targetGraph
completeMappings prop tgm (sourceNodes, []) targetGraph = completeWithRemainingNodes prop tgm (sourceNodes, []) targetGraph
completeMappings prop tgm sourceGraph targetGraph = completeFromSourceEdges prop tgm sourceGraph targetGraph

completeFromEmptySource :: MorphismType -> TypedGraphMorphism a b -> ExpandedGraph -> [TypedGraphMorphism a b]
completeFromEmptySource prop tgm (nodesT, edgesT) =
  case prop of
    GenericMorphism -> all
    Monomorphism    -> all
    Epimorphism     -> epimorphism
    Isomorphism     -> isomorphism
  where
    all = return tgm
    isomorphism | L.null nodesT && L.null edgesT = return tgm
                | otherwise = []

    epimorphism | L.null (orphanTypedNodes tgm) && L.null (orphanTypedEdges tgm) = return tgm
                | otherwise = []

completeWithRemainingNodes :: MorphismType -> TypedGraphMorphism a b -> ExpandedGraph -> ExpandedGraph -> [TypedGraphMorphism a b]
completeWithRemainingNodes prop tgm ([], _)  (nodesT, edgesT) = completeFromEmptySource prop tgm (nodesT, edgesT)
completeWithRemainingNodes _    _    _       ([],     _)      = []
completeWithRemainingNodes prop tgm (h:t, _) (nodesT, edgesT) = do
  nodeFromTarget <- nodesT
  let updatedTgm = updateNodesMapping h nodeFromTarget nodesT tgm
  case updatedTgm of
    Nothing  -> []
    Just tgm' ->
      case prop of
        GenericMorphism -> all
        Monomorphism    -> monomorphism
        Epimorphism     -> all
        Isomorphism     -> monomorphism
      where
        nodesT'   = delete nodeFromTarget nodesT
        monomorphism = completeMappings prop tgm' (t, []) (nodesT', edgesT)
        all          = completeMappings prop tgm' (t, []) (nodesT , edgesT)

completeFromSourceEdges ::  MorphismType -> TypedGraphMorphism a b -> ExpandedGraph -> ExpandedGraph -> [TypedGraphMorphism a b]
completeFromSourceEdges prop tgm (nodes, h:t) (nodesT, edgesT)
  | L.null edgesT = []
  | otherwise  = do
    edgeFromTarget <- edgesT
    let tgmN
          | isNothing tgm1 = Nothing
          | otherwise = tgm2
          where tgm1 = updateNodesMapping (extractSource d h) (extractSource c edgeFromTarget) nodesT tgm
                tgm2 = updateNodesMapping (extractTarget d h) (extractTarget c edgeFromTarget) nodesT' $ fromJust tgm1
                d = domain $ domain tgm
                c = domain $ codomain tgm
                nodesT' = case prop of
                  Monomorphism    -> L.delete (extractSource c edgeFromTarget) nodesT
                  Isomorphism     -> L.delete (extractSource c edgeFromTarget) nodesT
                  Epimorphism     -> nodesT
                  GenericMorphism -> nodesT

        --MAPPING SRC EDGE AND TGT EDGE
        tgmE
          | isNothing tgmN = Nothing
          | otherwise = updateEdgesMapping h edgeFromTarget edgesT $ fromJust tgmN

    --FOR THE COMPATIBLES MAPPINGS, GO TO THE NEXT STEP
    case tgmE of
      Just tgm' -> do
        let nodes'       = delete (extractSource d h) $ delete (extractTarget d h) nodes
            d            = domain $ domain tgm
            c            = domain $ codomain tgm
            --REMOVE THE TARGET EDGES AND NODES MAPPED (INJECTIVE MODULE)
            edgesT'      = delete edgeFromTarget edgesT
            nodesT'      = delete (extractSource c edgeFromTarget) $ delete (extractTarget c edgeFromTarget) nodesT
            monomorphism = completeMappings prop tgm' (nodes', t) (nodesT', edgesT')
            all          = completeMappings prop tgm' (nodes', t) (nodesT,  edgesT)
            --CHOSE BETWEEN INJECTIVE OR NOT
        case prop of
          GenericMorphism -> all
          Monomorphism    -> monomorphism
          Epimorphism     -> all
          Isomorphism     -> monomorphism
      Nothing  -> []

-- VALIDATION OF A NODE MAPPING
-- VERIFY IF THE TYPES OF n1 AND n2 ARE COMPATIBLE AND UPDATE MAPPING
updateNodesMapping :: G.NodeId -> G.NodeId -> [G.NodeId] -> TypedGraphMorphism a b -> Maybe (TypedGraphMorphism a b)
updateNodesMapping n1 n2 nodesT tgm =
  do
    let (d, c, m) = decomposeTypedGraphMorphism tgm
    if extractNodeType d n1 == extractNodeType c n2 &&
       ((isNothing (applyNode tgm n1) && L.elem n2 nodesT) || applyNode tgm n1 == Just n2)
    then Just $ buildTypedGraphMorphism d c $ GM.updateNodes n1 n2 m
    else Nothing

---------------------------------------------------------------------------------

-- VALIDATION OF A EDGE MAPPING
-- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLE AND UPDATE MAPPING
updateEdgesMapping :: G.EdgeId -> G.EdgeId -> [G.EdgeId] -> TypedGraphMorphism a b -> Maybe (TypedGraphMorphism a b)
updateEdgesMapping e1 e2 edgesT tgm =
  do
    let (d, c, m) = decomposeTypedGraphMorphism tgm
    if extractEdgeType d e1 == extractEdgeType c e2 &&
       ((isNothing (applyEdge tgm e1) && L.elem e2 edgesT ) || applyEdge tgm e1 == Just e2)
    then Just $ buildTypedGraphMorphism d c (GM.updateEdges e1 e2 m)
    else Nothing

decomposeTypedGraphMorphism :: TypedGraphMorphism a b -> (GM.GraphMorphism a b, GM.GraphMorphism a b, GM.GraphMorphism a b)
decomposeTypedGraphMorphism tgm = (domain tgm, codomain tgm, mapping tgm)