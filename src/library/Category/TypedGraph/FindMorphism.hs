module Category.TypedGraph.FindMorphism () where


import           Abstract.Category.NewClasses
import           Category.TypedGraph.Category
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import qualified Data.Relation                      as R
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

import           Control.Arrow
import           Data.List                          as L
import           Data.Maybe

instance FindMorphism (CatM n e) (TypedGraphMorphism n e) where
  induceSpanMorphism lefts rights = return $ induceSpan lefts rights

  findMorphisms cls dom cod = do
    cls' <- resolveClass cls
    return $ findMatches cls' dom cod

  findSpanCommuters cls left right = do
    cls' <- resolveClass cls
    return $ findSpanCommuter' cls' left right

  findCospanCommuters cls left right = do
    cls' <- resolveClass cls
    return $ findCospanCommuter' cls' left right


lookupEdgeById :: [Edge e] -> EdgeId -> Maybe (Edge e)
lookupEdgeById [] _ = Nothing
lookupEdgeById (e:es) edgeid =
  if edgeid == edgeId e then
    Just e
  else
    lookupEdgeById es edgeid

deleteEdgeById :: [Edge e] -> EdgeId -> [Edge e]
deleteEdgeById [] _ = []
deleteEdgeById (e:es) edgeid =
  if edgeid == edgeId e then
    deleteEdgeById es edgeid
  else
    e : deleteEdgeById es edgeid


data CospanBuilderState e =
  State {
    domainEdges            :: [Edge e]
  , codomainEdges          :: [Edge e]
  , unmappedDomainNodes    :: [NodeId]
  , unmappedDomainEdges    :: [EdgeId]
  , availableCodomainNodes :: [NodeId]
  , availableCodomainEdges :: [EdgeId]
  , nodeRelation           :: R.Relation NodeId
  , edgeRelation           :: R.Relation EdgeId
  , finalNodeRelation      :: R.Relation NodeId
  , finalEdgeRelation      :: R.Relation EdgeId
  }

-- | Given two TypedGraphMorphism @f : B -> A@ and @g : C -> A@ it finds a list of Morphisms
-- @hi : B -> C@ shuch that @¬g . f = hi@ for all @i@.
findCospanCommuter' :: ActualMorphismClass -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [TypedGraphMorphism n e]
findCospanCommuter' conf morphismF morphismG
  | codomain morphismF /= codomain morphismG = []
  | otherwise =
  let
    typedDomainFromF   = domain morphismF
    untypedDomainFromF = domain typedDomainFromF
    mappingFromF       = mapping morphismF

    typedDomainFromG   = domain morphismG
    untypedDomainFromG = domain typedDomainFromG
    mappingFromG       = mapping morphismG

    nodesIdsFromF = nodeIds untypedDomainFromF
    edgesIdsFromF = edgeIds untypedDomainFromF
    nodesIdsFromG = nodeIds untypedDomainFromG
    edgesIdsFromG = edgeIds untypedDomainFromG

    nodeRelationF = GM.nodeRelation mappingFromF
    edgeRelationF = GM.edgeRelation mappingFromF
    nodeRelationInvertedG = R.inverseRelation $ GM.nodeRelation mappingFromG
    edgeRelationInvertedG = R.inverseRelation $ GM.edgeRelation mappingFromG

    composedNodeRelation = R.compose nodeRelationF nodeRelationInvertedG
    composedEdgeRelation = R.compose edgeRelationF edgeRelationInvertedG

    edgesOfDomain   = edges untypedDomainFromF
    edgesOfCodomain = edges untypedDomainFromG

    initialState = State
                   edgesOfDomain edgesOfCodomain
                   nodesIdsFromF edgesIdsFromF
                   nodesIdsFromG edgesIdsFromG
                   composedNodeRelation composedEdgeRelation
                   (R.empty nodesIdsFromF nodesIdsFromG) (R.empty edgesIdsFromF edgesIdsFromG)

    edgesMapped = findCospanCommuterEdgeRelations conf initialState
    finalStates = concatMap (findCospanCommuterNodeRelations conf) edgesMapped

    buildTGMFromState state = buildTypedGraphMorphism typedDomainFromF typedDomainFromG $
      GM.fromGraphsAndRelations untypedDomainFromF untypedDomainFromG
      (finalNodeRelation state) (finalEdgeRelation state)

  in
    map buildTGMFromState finalStates

-- | Given a ActualMorphismClass and a initial @CospanBuilderState@ with final Node Relations complete,
-- finds a Relation @B -> C@ between edges of @B@ and @C@. (Auxiliary function)
findCospanCommuterEdgeRelations :: ActualMorphismClass -> CospanBuilderState e -> [CospanBuilderState e]
findCospanCommuterEdgeRelations conf state
  | L.null $ unmappedDomainEdges state =
    let isoCondition = L.null $ availableCodomainEdges state
        epiCondition = L.null . R.orphans $ finalEdgeRelation state
    in case conf of
         Isomorphisms ->
           if isoCondition then return state else []

         Epimorphisms ->
           if epiCondition then return state else []

         _ -> return state

  | otherwise =
    do
      let (edgeOnDomain:_) = unmappedDomainEdges state

      edgeOnCodomain <- R.apply (edgeRelation state) edgeOnDomain
      updatedState <- updateEdgeState conf edgeOnDomain edgeOnCodomain state

      findCospanCommuterEdgeRelations conf updatedState

-- | Given a ActualMorphismClass and a initial @CospanBuilderState@ with empty final Relations,
-- finds a Relation @B -> C@ between nodes of @B@ and @C@. (Auxiliary function)
findCospanCommuterNodeRelations :: ActualMorphismClass -> CospanBuilderState e -> [CospanBuilderState e]
findCospanCommuterNodeRelations conf state
  | L.null $ unmappedDomainNodes state =
    let isoCondition = L.null $ availableCodomainNodes state
        epiCondition = L.null . R.orphans $ finalNodeRelation state
    in case conf of
         Isomorphisms ->
           if isoCondition then return state else []

         Epimorphisms ->
           if epiCondition then return state else []

         _ -> return state

  | otherwise =
    do
      let (nodeOnDomain:_) = unmappedDomainNodes state

      nodeOnCodomain <- R.apply (nodeRelation state) nodeOnDomain
      updatedState <- updateNodeState conf nodeOnDomain nodeOnCodomain state

      findCospanCommuterNodeRelations conf updatedState

-- | Verify if a node of @B@ can be mapped to a node of @C@, if possible, updates the given @CospanBuilderState@. (Auxiliary function)
updateNodeState :: ActualMorphismClass -> NodeId -> NodeId -> CospanBuilderState e -> [CospanBuilderState e]
updateNodeState conf nodeOnDomain nodeOnCodomain state =
  let
    nodeDomainApplied = R.apply (finalNodeRelation state) nodeOnDomain

    monoCondition =
      nodeOnCodomain `elem` availableCodomainNodes state ||
      ( not (L.null nodeDomainApplied) &&
        head (R.apply (finalNodeRelation state) nodeOnDomain) == nodeOnCodomain)

    updatedGenericState =
      state { unmappedDomainNodes = delete nodeOnDomain $ unmappedDomainNodes state
            , finalNodeRelation   =
                R.updateRelation nodeOnDomain nodeOnCodomain $ finalNodeRelation state
            }

    updatedMonoState =
      updatedGenericState { availableCodomainNodes =
                              delete nodeOnCodomain $ availableCodomainNodes updatedGenericState
                          }
  in
    case (conf, monoCondition) of
      (Monomorphisms, False) ->
        []

      (Isomorphisms, False) ->
        []

      (Monomorphisms, True) ->
        return updatedMonoState

      (Isomorphisms, True) ->
        return updatedMonoState

      _ ->
        return updatedGenericState

-- | Verify if a edge of @B@ can be mapped to a node of @C@, if possible, updates the given @CospanBuilderState@. (Auxiliary function)
updateEdgeState :: ActualMorphismClass -> EdgeId -> EdgeId -> CospanBuilderState e -> [CospanBuilderState e]
updateEdgeState conf edgeOnDomain edgeOnCodomain state =
  do
    let monoCondition = edgeOnCodomain`elem` availableCodomainEdges state

        edgeDomain = fromJust $ lookupEdgeById (domainEdges state) edgeOnDomain
        sourceOnDomain = sourceId edgeDomain
        targetOnDomain = targetId edgeDomain

        edgeCodomain = fromJust $ lookupEdgeById (codomainEdges state) edgeOnCodomain
        sourceOnCodomain = sourceId edgeCodomain
        targetOnCodomain = targetId edgeCodomain

    sourceNodeUpdate <- updateNodeState conf sourceOnDomain sourceOnCodomain state
    targetNodeUpdate <- updateNodeState conf targetOnDomain targetOnCodomain sourceNodeUpdate

    let updatedGenericState =
          targetNodeUpdate { unmappedDomainEdges = delete edgeOnDomain $ unmappedDomainEdges state
                , finalEdgeRelation   =
                    R.updateRelation edgeOnDomain edgeOnCodomain (finalEdgeRelation state)
                }

        updatedMonoState =
          updatedGenericState { availableCodomainEdges =
                                  delete edgeOnCodomain $ availableCodomainEdges updatedGenericState
                              }

    case (conf, monoCondition) of
      (Monomorphisms, False) ->
        []

      (Isomorphisms, False) ->
        []

      (Monomorphisms, True) ->
        return updatedMonoState

      (Isomorphisms, True) ->
        return updatedMonoState

      _ ->
        return updatedGenericState


-- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
-- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
-- not be empty.
induceSpan :: [TypedGraphMorphism n e] ->  [TypedGraphMorphism n e] -> TypedGraphMorphism n e
induceSpan fs gs
  | Prelude.null fs = error "can not induce morphism from empty list of morphisms"
  | length fs /= length gs = error "morphisms list should have the same length"
  | otherwise = foldl buildSpanRelation morphismH (zip fs gs)
    where
      morphismH = initialSpanMorphism (head fs) (head gs)

-- | Given two TypedGraphMorphism @f : A -> B@ and @g : A -> C@ it builds a TypedGraphMorphism @h : B -> C@
-- with an empty mapping between the objects (auxiliary function)
initialSpanMorphism :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TypedGraphMorphism n e
initialSpanMorphism morphismF morphismG = buildTypedGraphMorphism domainH codomainH mappingH
  where
    domainH   = codomain morphismF
    codomainH = codomain morphismG
    mappingH  = GM.empty (domain domainH) (domain codomainH)


-- | Given a TypedGraphMorphism @h : B -> C@ and a tuple of TypedGraphMorphism (f : A -> B, g : A -> C)
-- it updates @h@ with a mapping from @B to C@ where @h . f = g@ (auxiliary function)
buildSpanRelation :: TypedGraphMorphism n e ->  (TypedGraphMorphism n e, TypedGraphMorphism n e) -> TypedGraphMorphism n e
buildSpanRelation morphismH (morphismF, morphismG) =
  buildSpanEdgeRelation (buildSpanNodeRelation morphismH (morphismF, morphismG)) (morphismF, morphismG)

-- | Given a TypedGraphMorphism @h : B -> C@ and a tuple of TypedGraphMorphism (f : A -> B, g : A -> C)
-- it updates @h@ with a mapping of edges from @B to C@ where @h . f = g@ (auxiliary function)
buildSpanNodeRelation :: TypedGraphMorphism n e ->  (TypedGraphMorphism n e, TypedGraphMorphism n e) -> TypedGraphMorphism n e
buildSpanNodeRelation morphismH (morphismF, morphismG) = foldr (uncurry untypedUpdateNodeRelation) morphismH newNodeRelation
  where
    newNodeRelation = map (applyNodeIdUnsafe morphismF &&& applyNodeIdUnsafe morphismG ) $ nodeIdsFromDomain morphismF

-- | Given a TypedGraphMorphism @h : B -> C@ and a tuple of TypedGraphMorphism (f : A -> B, g : A -> C)
-- it updates @h@ with a mapping of edges from @B to C@ where @h . f = g@ (auxiliary function)
buildSpanEdgeRelation :: TypedGraphMorphism n e ->  (TypedGraphMorphism n e, TypedGraphMorphism n e) -> TypedGraphMorphism n e
buildSpanEdgeRelation morphismH (morphismF, morphismG) = foldr (uncurry updateEdgeRelation) morphismH newEdgeRelation
  where
    newEdgeRelation = map (applyEdgeIdUnsafe morphismF &&& applyEdgeIdUnsafe morphismG ) $ edgeIdsFromDomain morphismF

------------------------------------------------------------------------------

-- FIXME: implement findSpanCommuter', which is a variation of partialInjectiveMatches'
findSpanCommuter' :: ActualMorphismClass -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [TypedGraphMorphism n e]
findSpanCommuter' = error "findSpanCommuter' unimplemented"

-- | Finds matches __/q/__ .
--
--   Partially injective. (Injective out of __/m/__)
partialInjectiveMatches' :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> [TypedGraphMorphism n e]
partialInjectiveMatches' nac match = do
  let
    lhsNodes = nodeIdsFromDomain match
    lhsEdges = edgeIdsFromDomain match
    q = initialSpanMorphism nac match
    q' = preBuildEdges q nac match lhsEdges
    q'' = case q' of
      Just q1 -> preBuildNodes q1 nac match lhsNodes
      Nothing -> Nothing

  case q'' of
    Nothing -> []
    Just q2 -> completeMappings Monomorphisms q2 (sourceNodes, sourceEdges) (targetNodes, targetEdges)
      where
        notMappedNodes tgm node = isNothing $ applyNodeId tgm node
        notMappedEdges tgm edge = isNothing $ applyEdgeId tgm edge
        sourceNodes = filter (notMappedNodes q2) (nodeIdsFromDomain q2)
        sourceEdges = filter (notMappedEdges q2 . edgeId) (edgesFromDomain q2)
        targetNodes = orphanTypedNodeIds q2
        targetEdges = orphanTypedEdges q2

--VERIFY EDGES MAPPING N <- l AND L -> G AND BUILD A N -> G
--PARTIAL EDGES MORPHISM
preBuildEdges :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [G.EdgeId] -> Maybe (TypedGraphMorphism n e)
preBuildEdges tgm _ _ [] = Just tgm
preBuildEdges tgm nac match (h:t) = do
  let nacEdge = applyEdgeIdUnsafe nac h
      matchEdge   = applyEdgeIdUnsafe match h
      (dom, cod, _) = decomposeTypedGraphMorphism tgm
      tgm' = if (extractEdgeType dom nacEdge == extractEdgeType cod matchEdge) &&
                (isNothing (applyEdgeId tgm nacEdge) || (applyEdgeId tgm nacEdge == Just matchEdge))
             then Just $ buildTypedGraphMorphism dom cod (GM.updateEdges nacEdge matchEdge $ mapping tgm)
             else Nothing
  case tgm' of
    Just tgm'' -> preBuildEdges tgm'' nac match t
    Nothing    -> Nothing

--VERIFY NODE MAPPINGS N <- L AND L -> G AND BUILD A N -> G
--PARTIAL NODES MORPHISM
preBuildNodes :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [G.NodeId] -> Maybe (TypedGraphMorphism n e)
preBuildNodes tgm _   _     []    = Just tgm
preBuildNodes tgm nac match (h:t) = do
  let nacNode   = applyNodeIdUnsafe nac h
      matchNode = applyNodeIdUnsafe match h
      (tgmDomain, tgmCodomain, tgmMapping) = decomposeTypedGraphMorphism tgm
      tgm' = if (extractNodeType tgmDomain nacNode == extractNodeType tgmCodomain matchNode) &&
                (isNothing (applyNodeId tgm nacNode) || (applyNodeId tgm nacNode == Just matchNode))
             then Just $ buildTypedGraphMorphism tgmDomain tgmCodomain (GM.updateNodes nacNode matchNode tgmMapping)
             else Nothing
  case tgm' of
    Just tgm'' -> preBuildNodes tgm'' nac match t
    Nothing    -> Nothing

---------------------------------------------------------------------------------

-- | Finds matches __/m/__
--
--   Injective, surjective, isomorphic or all possible matches
findMatches :: ActualMorphismClass -> GM.GraphMorphism (Maybe n) (Maybe e) -> GM.GraphMorphism (Maybe n) (Maybe e) -> [TypedGraphMorphism n e]
findMatches prop graph1 graph2 =
  completeMappings prop tgm (sourceNodes, sourceEdges) (targetNodes, targetEdges)
  where
    sourceNodes = nodeIds $ domain graph1
    targetNodes = nodeIds $ domain graph2
    sourceEdges = edges $ domain graph1
    targetEdges = edges $ domain graph2

    d   = graph1
    c   = graph2
    m   = GM.empty (domain graph1) (domain graph2)
    tgm = buildTypedGraphMorphism d c m

---------------------------------------------------------------------------------

type ExpandedGraph b = ([G.NodeId], [Edge b])

-- | Given a TypedGraphMorphism @tgm@ from (A -> T) to (B -> T) and the two ExpandedGraphs of A and B, it completes the @tgm@
-- with all the possible mappings from (A -> T) to (B -> T)
completeMappings :: ActualMorphismClass -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeMappings prop tgm ([], []) targetGraph = completeFromEmptySource prop tgm targetGraph
completeMappings prop tgm (sourceNodes, []) targetGraph = completeWithRemainingNodes prop tgm (sourceNodes, []) targetGraph
completeMappings prop tgm sourceGraph targetGraph = completeFromSourceEdges prop tgm sourceGraph targetGraph

completeFromEmptySource :: ActualMorphismClass -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeFromEmptySource prop tgm (nodesT, edgesT) =
  case prop of
    AllMorphisms     -> all
    Monomorphisms    -> all
    Epimorphisms     -> epimorphism
    Isomorphisms     -> isomorphism
  where
    all = return tgm
    isomorphism | L.null nodesT && L.null edgesT = return tgm
                | otherwise = []

    epimorphism | L.null (orphanTypedNodeIds tgm) && L.null (orphanTypedEdgeIds tgm) = return tgm
                | otherwise = []

completeWithRemainingNodes :: ActualMorphismClass -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeWithRemainingNodes prop tgm ([], _)  (nodesT, edgesT) = completeFromEmptySource prop tgm (nodesT, edgesT)
completeWithRemainingNodes _    _    _       ([],     _)      = []
completeWithRemainingNodes prop tgm (h:t, _) (nodesT, edgesT) = do
  nodeFromTarget <- nodesT
  let updatedTgm = updateNodesMapping h nodeFromTarget nodesT tgm
  case updatedTgm of
    Nothing  -> []
    Just tgm' ->
      case prop of
        AllMorphisms -> all
        Monomorphisms    -> monomorphism
        Epimorphisms     -> all
        Isomorphisms     -> monomorphism
      where
        nodesT'   = delete nodeFromTarget nodesT
        monomorphism = completeMappings prop tgm' (t, []) (nodesT', edgesT)
        all          = completeMappings prop tgm' (t, []) (nodesT , edgesT)

completeFromSourceEdges ::  ActualMorphismClass -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeFromSourceEdges _ _ (_, []) (_, _) = error "completeFromSourceEdges: unexpected empty node list"
completeFromSourceEdges prop tgm (nodes, e:t) (nodesT, edgesT)
  | L.null edgesT = []
  | otherwise  = do
    edgeTarget <- edgesT

    let edgeFromTarget = edgeId edgeTarget
        srcE = sourceId edgeTarget
        tgtE = targetId edgeTarget

        h = edgeId e
        srcH = sourceId e
        tgtH = targetId e

        tgmN
          | isNothing tgm1 = Nothing
          | otherwise = tgm2
          where tgm1 = updateNodesMapping srcH srcE nodesT tgm
                tgm2 = updateNodesMapping tgtH tgtE nodesT' $ fromJust tgm1
                nodesT' = case prop of
                  Monomorphisms    -> L.delete srcE nodesT
                  Isomorphisms     -> L.delete srcE nodesT
                  Epimorphisms     -> nodesT
                  AllMorphisms -> nodesT

        --MAPPING SRC Edge eND TGT EDGE
        tgmE
          | isNothing tgmN = Nothing
          | otherwise = updateEdgesMapping h edgeFromTarget edgesT $ fromJust tgmN

    --FOR THE COMPATIBLES MAPPINGS, GO TO THE NEXT STEP
    case tgmE of
      Just tgm' -> do
        let nodes'       = delete srcH $ delete tgtH nodes
            --REMOVE THE TARGET EDGES AND NODES MAPPED (INJECTIVE MODULE)
            edgesT'      = deleteEdgeById edgesT edgeFromTarget
            nodesT'      = delete srcE $ delete tgtE nodesT
            monomorphism = completeMappings prop tgm' (nodes', t) (nodesT', edgesT')
            all          = completeMappings prop tgm' (nodes', t) (nodesT,  edgesT)
            --CHOSE BETWEEN INJECTIVE OR NOT
        case prop of
          AllMorphisms -> all
          Monomorphisms    -> monomorphism
          Epimorphisms     -> all
          Isomorphisms     -> monomorphism
      Nothing  -> []

-- VALIDATION OF A NODE MAPPING
-- VERIFY IF THE TYPES OF n1 AND n2 ARE COMPATIBLE AND UPDATE MAPPING
updateNodesMapping :: G.NodeId -> G.NodeId -> [G.NodeId] -> TypedGraphMorphism n e -> Maybe (TypedGraphMorphism n e)
updateNodesMapping n1 n2 nodesT tgm =
  do
    let (d, c, m) = decomposeTypedGraphMorphism tgm
    if extractNodeType d n1 == extractNodeType c n2 &&
       ((isNothing (applyNodeId tgm n1) && L.elem n2 nodesT) || applyNodeId tgm n1 == Just n2)
    then Just $ buildTypedGraphMorphism d c $ GM.updateNodes n1 n2 m
    else Nothing

---------------------------------------------------------------------------------

-- VALIDATION OF A EDGE MAPPING
-- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLE AND UPDATE MAPPING
updateEdgesMapping :: G.EdgeId -> G.EdgeId -> [G.Edge (Maybe b)] -> TypedGraphMorphism n e -> Maybe (TypedGraphMorphism n e)
updateEdgesMapping e1 e2 edgesT tgm =
  do
    let (d, c, m) = decomposeTypedGraphMorphism tgm
    if extractEdgeType d e1 == extractEdgeType c e2 &&
       ((isNothing (applyEdgeId tgm e1) && isJust (lookupEdgeById edgesT e2)) || applyEdgeId tgm e1 == Just e2)
    then Just $ buildTypedGraphMorphism d c (GM.updateEdges e1 e2 m)
    else Nothing

decomposeTypedGraphMorphism :: TypedGraphMorphism n e -> (GM.GraphMorphism (Maybe n) (Maybe e), GM.GraphMorphism (Maybe n) (Maybe e), GM.GraphMorphism (Maybe n) (Maybe e))
decomposeTypedGraphMorphism tgm = (domain tgm, codomain tgm, mapping tgm)
