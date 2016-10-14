{-# OPTIONS_GHC -Wno-orphans #-}
module TypedGraph.Morphism.FindMorphism where

import           Abstract.AdhesiveHLR
import           Abstract.Morphism                               as M
import           Graph.Graph                                     as G
import qualified Graph.GraphMorphism                             as GM
import           TypedGraph.Graph
import           TypedGraph.Morphism.Core

import           Data.List                                       as L
import           Data.Maybe


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
