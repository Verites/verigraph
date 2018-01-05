module Category.TypedGraph.FindMorphism () where


import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Category.TypedGraph.Category
import qualified Data.Graphs                  as G
import qualified Data.Graphs.Morphism         as GM
import qualified Data.Relation                as R
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

import           Control.Arrow
import           Data.List                    as L
import           Data.Maybe

instance FindMorphism (TypedGraphMorphism a b) where
  induceSpanMorphism = induceSpan
  findMorphisms = findMatches . toMorphismType
  findSpanCommuters = findSpanCommuter' . toMorphismType
  findCospanCommuters = findCospanCommuter' . toMorphismType


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
  CospanState {
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
findCospanCommuter' :: MorphismType -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [TypedGraphMorphism n e]
findCospanCommuter' conf morphismF morphismG
  | codomain morphismF /= codomain morphismG = []
  | otherwise =
  let
    domainFromF   = domain morphismF
    untypedDomainFromF = domain domainFromF
    mappingFromF       = mapping morphismF

    domainFromG   = domain morphismG
    untypedDomainFromG = domain domainFromG
    mappingFromG       = mapping morphismG

    nodesIdsFromF = nodeIds domainFromF
    edgesIdsFromF = edgeIds domainFromF
    nodesIdsFromG = nodeIds domainFromG
    edgesIdsFromG = edgeIds domainFromG

    nodeRelationF = GM.nodeRelation mappingFromF
    edgeRelationF = GM.edgeRelation mappingFromF
    nodeRelationInvertedG = R.inverseRelation $ GM.nodeRelation mappingFromG
    edgeRelationInvertedG = R.inverseRelation $ GM.edgeRelation mappingFromG

    composedNodeRelation = R.compose nodeRelationF nodeRelationInvertedG
    composedEdgeRelation = R.compose edgeRelationF edgeRelationInvertedG

    edgesOfDomain   = map fst (edges domainFromF)
    edgesOfCodomain = map fst (edges domainFromG)

    initialState = CospanState
                   edgesOfDomain edgesOfCodomain
                   nodesIdsFromF edgesIdsFromF
                   nodesIdsFromG edgesIdsFromG
                   composedNodeRelation composedEdgeRelation
                   (R.empty nodesIdsFromF nodesIdsFromG) (R.empty edgesIdsFromF edgesIdsFromG)

    edgesMapped = findCospanCommuterEdgeRelations conf initialState
    finalStates = concatMap (findCospanCommuterNodeRelations conf) edgesMapped

    buildTGMFromState state = buildTypedGraphMorphism domainFromF domainFromG $
      GM.fromGraphsAndRelations untypedDomainFromF untypedDomainFromG
      (finalNodeRelation state) (finalEdgeRelation state)

  in
    map buildTGMFromState finalStates

-- | Given a MorphismType and a initial @CospanBuilderState@ with final Node Relations complete,
-- finds a Relation @B -> C@ between edges of @B@ and @C@. (Auxiliary function)
findCospanCommuterEdgeRelations :: MorphismType -> CospanBuilderState e -> [CospanBuilderState e]
findCospanCommuterEdgeRelations conf state
  | L.null $ unmappedDomainEdges state =
    let isoCondition = L.null $ availableCodomainEdges state
        epiCondition = L.null . R.orphans $ finalEdgeRelation state
    in case conf of
         Isomorphism ->
           if isoCondition then return state else []

         Epimorphism ->
           if epiCondition then return state else []

         _ -> return state

  | otherwise =
    do
      let (edgeOnDomain:_) = unmappedDomainEdges state

      edgeOnCodomain <- R.apply (edgeRelation state) edgeOnDomain
      updatedState <- updateEdgeState conf edgeOnDomain edgeOnCodomain state

      findCospanCommuterEdgeRelations conf updatedState

-- | Given a MorphismType and a initial @CospanBuilderState@ with empty final Relations,
-- finds a Relation @B -> C@ between nodes of @B@ and @C@. (Auxiliary function)
findCospanCommuterNodeRelations :: MorphismType -> CospanBuilderState e -> [CospanBuilderState e]
findCospanCommuterNodeRelations conf state
  | L.null $ unmappedDomainNodes state =
    let isoCondition = L.null $ availableCodomainNodes state
        epiCondition = L.null . R.orphans $ finalNodeRelation state
    in case conf of
         Isomorphism ->
           if isoCondition then return state else []

         Epimorphism ->
           if epiCondition then return state else []

         _ -> return state

  | otherwise =
    do
      let (nodeOnDomain:_) = unmappedDomainNodes state

      nodeOnCodomain <- R.apply (nodeRelation state) nodeOnDomain
      updatedState <- updateNodeState conf nodeOnDomain nodeOnCodomain state

      findCospanCommuterNodeRelations conf updatedState

-- | Verify if a node of @B@ can be mapped to a node of @C@, if possible, updates the given @CospanBuilderState@. (Auxiliary function)
updateNodeState :: MorphismType -> NodeId -> NodeId -> CospanBuilderState e -> [CospanBuilderState e]
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
      (Monomorphism, False) ->
        []

      (Isomorphism, False) ->
        []

      (Monomorphism, True) ->
        return updatedMonoState

      (Isomorphism, True) ->
        return updatedMonoState

      _ ->
        return updatedGenericState

-- | Verify if a edge of @B@ can be mapped to a node of @C@, if possible, updates the given @CospanBuilderState@. (Auxiliary function)
updateEdgeState :: MorphismType -> EdgeId -> EdgeId -> CospanBuilderState e -> [CospanBuilderState e]
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
      (Monomorphism, False) ->
        []

      (Isomorphism, False) ->
        []

      (Monomorphism, True) ->
        return updatedMonoState

      (Isomorphism, True) ->
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
    newNodeRelation = map (applyNodeIdUnsafe morphismF &&& applyNodeIdUnsafe morphismG ) . nodeIds $ domain morphismF

-- | Given a TypedGraphMorphism @h : B -> C@ and a tuple of TypedGraphMorphism (f : A -> B, g : A -> C)
-- it updates @h@ with a mapping of edges from @B to C@ where @h . f = g@ (auxiliary function)
buildSpanEdgeRelation :: TypedGraphMorphism n e ->  (TypedGraphMorphism n e, TypedGraphMorphism n e) -> TypedGraphMorphism n e
buildSpanEdgeRelation morphismH (morphismF, morphismG) = foldr (uncurry updateEdgeRelation) morphismH newEdgeRelation
  where
    newEdgeRelation = map (applyEdgeIdUnsafe morphismF &&& applyEdgeIdUnsafe morphismG ) . edgeIds $ domain morphismF



------------------------------------------------------------------------------



data SpanBuilderState n e =
  SpanState {
    morphismF                  :: TypedGraphMorphism n e
  , morphismG                  :: TypedGraphMorphism n e
  , morphismH                  :: TypedGraphMorphism n e
  , availableSpanCodomainEdges :: [G.EdgeId]
  , availableSpanCodomainNodes :: [G.NodeId]
  }


-- |          f     g
-- |       B <-- A --> C
-- |       \           ^
-- |        \_________/
-- |             h

findSpanCommuter' :: MorphismType -> TypedGraphMorphism n e -> TypedGraphMorphism n e -> [TypedGraphMorphism n e]
findSpanCommuter' prop morphismF morphismG  = do
  let
    codomainNodes = nodeIds $ codomain morphismG
    codomainEdges = edgeIds $ codomain morphismG

    initialMorphism = initialSpanMorphism morphismF morphismG
    initialState = SpanState morphismF morphismG initialMorphism codomainEdges codomainNodes

    centerEdges = edgeIds $ domain morphismG
    centerNodes = nodeIds $ domain morphismG

    state'' =
      do
        state' <- preBuildEdges prop initialState centerEdges
        preBuildNodes prop state' centerNodes

  case state'' of
    Nothing -> []
    Just state''' ->
      completeMappings
        prop
        (morphismH state''')
        (notMappedSourceNodes, notMappedSourceEdges) (notMappedTargetNodes, notMappedTargetEdges)
      where
        finalTGM = morphismH state'''
        notMappedSourceNodes = filter (isNotMappedNode finalTGM) (nodeIds $ domain finalTGM)
        notMappedSourceEdges = filter (isNotMappedEdge finalTGM . edgeId) (map fst . edges $ domain finalTGM)
        notMappedTargetNodes = orphanTypedNodeIds finalTGM
        notMappedTargetEdges = orphanTypedEdges finalTGM

        isNotMappedNode :: TypedGraphMorphism n e -> NodeId -> Bool
        isNotMappedNode tgm node = isNothing $ applyNodeId tgm node

        isNotMappedEdge :: TypedGraphMorphism n e -> EdgeId -> Bool
        isNotMappedEdge tgm edge = isNothing $ applyEdgeId tgm edge


-- | Given two TypedGraphMorphism @f : A -> B@ and @g : A -> C@ it builds a TypedGraphMorphism @h : B -> C@
-- with an empty mapping between the objects (auxiliary function)
initialSpanMorphism :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TypedGraphMorphism n e
initialSpanMorphism morphismF morphismG = buildTypedGraphMorphism domainH codomainH mappingH
  where
    domainH   = codomain morphismF
    codomainH = codomain morphismG
    mappingH  = GM.empty (domain domainH) (domain codomainH)



-- |          f     g
-- |       B <-- A --> C
-- |       \           ^
-- |        \_________/
-- |             h
-- |
-- | verify edges mapping on f:B <- A and g:A -> C to build a h:B -> C
-- | partial edges morphism

preBuildEdges :: MorphismType -> SpanBuilderState n e -> [G.EdgeId] -> Maybe (SpanBuilderState n e)
preBuildEdges _ state [] = Just state
preBuildEdges prop state (edge:es) =
  do
    state' <- updateEdgesSpanState prop state edge
    preBuildEdges prop state' es

-- TODO: FORCE NODE MAPPINGS (VERIFY IF IT IS NECESSARY)
updateEdgesSpanState :: MorphismType -> SpanBuilderState n e -> G.EdgeId -> Maybe (SpanBuilderState n e)
updateEdgesSpanState prop state edge = do
  let edgeOnF = applyEdgeIdUnsafe (morphismF state) edge
      edgeOnG = applyEdgeIdUnsafe (morphismG state) edge
      updatedMapping = updateEdgeRelation edgeOnF edgeOnG (morphismH state)

      updatedMonoState = state { morphismH = updatedMapping
                               , availableSpanCodomainEdges = delete edgeOnG (availableSpanCodomainEdges state)
                               }

      updatedGenericState = state { morphismH = updatedMapping }

      monoCondition = edgeOnG `elem` availableSpanCodomainEdges state

  if (extractEdgeType (domain $ morphismH state) edgeOnF == extractEdgeType (codomain $ morphismH state) edgeOnG) &&
     (isNothing (applyEdgeId (morphismH state) edgeOnF) || (applyEdgeId (morphismH state) edgeOnF == Just edgeOnG))
    then
    case (prop, monoCondition) of
      (Isomorphism, False) ->
        Nothing

      (Monomorphism, False) ->
        Nothing

      (Isomorphism, True) ->
        return updatedMonoState

      (Monomorphism, True) ->
        return updatedMonoState

      _ ->
        return updatedGenericState

    else Nothing

-- |          f     g
-- |       B <-- A --> C
-- |       \           ^
-- |        \_________/
-- |             h
-- |
-- | verify nodes mapping on f:B <- A and g:A -> C to build a h:B -> C
-- | partial nodes morphism
preBuildNodes :: MorphismType -> SpanBuilderState n e -> [G.NodeId] -> Maybe (SpanBuilderState n e)
preBuildNodes _ state []    = return state
preBuildNodes prop state (n:ns) =
  do
    state' <- updateNodesSpanState prop state n
    preBuildNodes prop state' ns

updateNodesSpanState  :: MorphismType -> SpanBuilderState n e -> G.NodeId -> Maybe (SpanBuilderState n e)
updateNodesSpanState prop state node = do
  let nodeOnF = applyNodeIdUnsafe (morphismF state) node
      nodeOnG = applyNodeIdUnsafe (morphismG state) node
      updatedMapping = untypedUpdateNodeRelation nodeOnF nodeOnG (morphismH state)


      updatedMonoState = state { morphismH = updatedMapping
                               , availableSpanCodomainNodes = delete nodeOnG (availableSpanCodomainNodes state)
                               }

      updatedGenericState = state { morphismH = updatedMapping }

      monoCondition = nodeOnG `elem` availableSpanCodomainNodes state || (applyNodeIdUnsafe (morphismH state) nodeOnF == nodeOnG)

  if (extractNodeType (domain $ morphismH state) nodeOnF == extractNodeType (codomain $ morphismH state) nodeOnG) &&
     (isNothing (applyNodeId (morphismH state) nodeOnF) || (applyNodeId (morphismH state) nodeOnF == Just nodeOnG))
    then
    case (prop, monoCondition) of
        (Isomorphism, False) ->
          Nothing

        (Monomorphism, False) ->
          Nothing

        (Isomorphism, True) ->
          return updatedMonoState

        (Monomorphism, True) ->
          return updatedMonoState

        _ ->
          return updatedGenericState

  else Nothing



---------------------------------------------------------------------------------



-- | Finds matches __/m/__
--
--   Injective, surjective, isomorphic or all possible matches
findMatches :: MorphismType -> GM.GraphMorphism (Maybe n) (Maybe e) -> GM.GraphMorphism (Maybe n) (Maybe e) -> [TypedGraphMorphism n e]
findMatches prop graph1 graph2 =
  completeMappings prop tgm (sourceNodes, sourceEdges) (targetNodes, targetEdges)
  where
    sourceNodes = nodeIds graph1
    targetNodes = nodeIds graph2
    sourceEdges = map fst (edges graph1)
    targetEdges = map fst (edges graph2)

    m   = GM.empty (domain graph1) (domain graph2)
    tgm = buildTypedGraphMorphism graph1 graph2 m

---------------------------------------------------------------------------------

type ExpandedGraph b = ([G.NodeId], [Edge b])

-- | Given a TypedGraphMorphism @tgm@ from (A -> T) to (B -> T) and the two ExpandedGraphs of A and B, it completes the @tgm@
-- with all the possible mappings from (A -> T) to (B -> T)
completeMappings :: MorphismType -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeMappings prop tgm ([], []) targetGraph = completeFromEmptySource prop tgm targetGraph
completeMappings prop tgm (sourceNodes, []) targetGraph = completeWithRemainingNodes prop tgm (sourceNodes, []) targetGraph
completeMappings prop tgm sourceGraph targetGraph = completeFromSourceEdges prop tgm sourceGraph targetGraph

completeFromEmptySource :: MorphismType -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeFromEmptySource prop tgm (nodesT, edgesT) =
  case prop of
    AnyMorphism     -> all
    Monomorphism    -> all
    Epimorphism     -> epimorphism
    Isomorphism     -> isomorphism
  where
    all = return tgm
    isomorphism | L.null nodesT && L.null edgesT = return tgm
                | otherwise = []

    epimorphism | L.null (orphanTypedNodeIds tgm) && L.null (orphanTypedEdgeIds tgm) = return tgm
                | otherwise = []

completeWithRemainingNodes :: MorphismType -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
completeWithRemainingNodes prop tgm ([], _)  (nodesT, edgesT) = completeFromEmptySource prop tgm (nodesT, edgesT)
completeWithRemainingNodes _    _    _       ([],     _)      = []
completeWithRemainingNodes prop tgm (h:t, _) (nodesT, edgesT) = do
  nodeFromTarget <- nodesT
  let updatedTgm = updateNodesMapping h nodeFromTarget nodesT tgm
  case updatedTgm of
    Nothing  -> []
    Just tgm' ->
      case prop of
        AnyMorphism -> all
        Monomorphism    -> monomorphism
        Epimorphism     -> all
        Isomorphism     -> monomorphism
      where
        nodesT'   = delete nodeFromTarget nodesT
        monomorphism = completeMappings prop tgm' (t, []) (nodesT', edgesT)
        all          = completeMappings prop tgm' (t, []) (nodesT , edgesT)

completeFromSourceEdges ::  MorphismType -> TypedGraphMorphism n e -> ExpandedGraph (Maybe b) -> ExpandedGraph (Maybe b) -> [TypedGraphMorphism n e]
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
                  Monomorphism    -> L.delete srcE nodesT
                  Isomorphism     -> L.delete srcE nodesT
                  Epimorphism     -> nodesT
                  AnyMorphism -> nodesT

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
          AnyMorphism -> all
          Monomorphism    -> monomorphism
          Epimorphism     -> all
          Isomorphism     -> monomorphism
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
