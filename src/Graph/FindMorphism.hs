{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.FindMorphism () where

import           Abstract.Morphism
import           Data.List                as L
import           Data.Maybe
import           Graph.Graph              as G
import qualified Graph.GraphMorphism      as GM
import           Graph.TypedGraphMorphism as TGM

instance FindMorphism (TGM.TypedGraphMorphism a b) where
  matches = matches'
  partInjMatches = partInjMatches'

--ALIAS OF MOST USED FUNCTIONS --

-- TODO: following functions should be part of the Graph interface
srcE, tgtE :: Graph a b -> EdgeId -> NodeId
srcE gm e = fromJust $ G.sourceOf gm e
tgtE gm e = fromJust $ G.targetOf gm e

-- TODO: following function should be part of TypedGraph interface
typeN :: GM.GraphMorphism a b -> NodeId -> NodeId
typeN gm n = fromMaybe (error "NODE NOT TYPED") $ GM.applyNode gm n

-- TODO: following function should be part of TypedGraph interface
typeE :: GM.GraphMorphism a b -> EdgeId -> EdgeId
typeE gm e = fromMaybe (error "EDGE NOT TYPED") $ GM.applyEdge gm e

---------------------------------------------------------------------------------

-- | Finds matches __/q/__ .
--
--   Partially injective. (Injective out of __/m/__)
partInjMatches' :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
               -> [TGM.TypedGraphMorphism a b]
partInjMatches' nac match =
  do
    let
      --NODES AND EDGES FROM lhs OF A RULE
      nodesL = nodes $ domain $ domain match
      edgesL = edges $ domain $ domain match

      --PRÉ-BUILD @q@
      domQ   = codomain nac
      codQ   = codomain match
      mapQ   = GM.empty (domain domQ) (domain codQ)
      q      = TGM.typedMorphism domQ codQ mapQ

      --VERIFY EDGES MAPPING N <- l AND L -> G AND BUILD A N -> G
      --PARTIAL EDGES MORPHISM
      composeEdges :: TGM.TypedGraphMorphism a b -> [G.EdgeId]
                   -> Maybe (TGM.TypedGraphMorphism a b)
      composeEdges tgm [] = Just tgm
      composeEdges tgm (h:t) =
        do
          let edgeNac = fromMaybe (error "EDGE NOT MAPPING L -> N") $
                                  applyEdgeTGM nac h
              edgeG   = fromMaybe (error "EDGE NOT MAPPING L -> G") $
                                  applyEdgeTGM match h

              dom     = domain tgm
              cod     = codomain tgm

              tgm' = if (typeE dom edgeNac == typeE cod edgeG) &&
                        (isNothing (applyEdgeTGM tgm edgeNac) ||
                         (applyEdgeTGM tgm edgeNac == Just edgeG))
                     then Just $ TGM.typedMorphism dom cod
                                 (GM.updateEdges edgeNac edgeG $ mapping tgm)
                     else Nothing

          case tgm' of
            Just tgm'' -> composeEdges tgm'' t
            Nothing -> Nothing

      --VERIFY NODES MAPPING N <- l AND L -> G AND BUILD A N -> G
      --PARTIAL NODES MORPHISM
      composeNodes :: TGM.TypedGraphMorphism a b -> [G.NodeId]
                   -> Maybe (TGM.TypedGraphMorphism a b)
      composeNodes tgm [] = Just tgm
      composeNodes tgm (h:t) =
        do
          let nodeNac = fromMaybe (error "NODE NOT MAPPED L->N") $
                        applyNodeTGM nac h
              nodeG   = fromMaybe (error "NODE NOT MAPPED L->G") $
                        applyNodeTGM match h

              dom     = domain tgm
              cod     = codomain tgm
              m       = mapping tgm

              tgm' = if (typeN dom nodeNac == typeN cod nodeG) &&
                        (isNothing (applyNodeTGM tgm nodeNac) ||
                         (applyNodeTGM tgm nodeNac == Just nodeG))
                     then Just $ TGM.typedMorphism dom cod
                                 (GM.updateNodes nodeNac nodeG m)
                     else Nothing
          case tgm' of
            Just tgm'' -> composeNodes tgm'' t
            Nothing -> Nothing

      --PRE-BUILD EDGES MAPPING OF @q@
      q' = composeEdges q edgesL

      --PRE-BUILD NODES MAPPING OF @q@
      q'' = case q' of
        Just q1 -> composeNodes q1 nodesL
        Nothing -> Nothing

    case q'' of
      Nothing -> []
      Just q2 -> buildMappings MONO nodesSrc edgesSrc nodesTgt edgesTgt q2
        where
          --DELETE FROM QUEUE ALREADY MAPPED SOURCE NODES (NODES FROM NAC)
          nodesSrc = filter (notMappedNodes q2) (nodes $ domain domQ)
            where
              notMappedNodes tgm node = isNothing $ applyNodeTGM tgm node
          --DELETE FROM QUEUE ALREADY MAPPED SOURCE EDGES (EDGES FROM NAC)
          edgesSrc = filter (notMappedEdges q2) (edges $ domain domQ)
            where
              notMappedEdges tgm edge = isNothing $ applyEdgeTGM tgm edge

          --REMOVE FROM TARGET LIST NODES ALREADY MAPPED (NODES FROM G)
          nodesTgt = orphanNodesTyped q2

          --REMOVE FROM TARGET LIST EDGES ALREADY MAPPED (EDGES FROM G)
          edgesTgt = orphanEdgesTyped q2

---------------------------------------------------------------------------------

-- | Finds matches __/m/__
--
--   Injective, surjective, isomorphic or all possible matches
matches' :: PROP -> GM.GraphMorphism a b-> GM.GraphMorphism a b
        -> [TGM.TypedGraphMorphism a b]
matches' prop graph1 graph2 =
  buildMappings prop nodesSrc edgesSrc nodesTgt edgesTgt tgm
  where
    nodesSrc = nodes $ domain graph1
    nodesTgt = nodes $ domain graph2
    edgesSrc = edges $ domain graph1
    edgesTgt = edges $ domain graph2

    d   = graph1
    c   = graph2
    m   = GM.empty (domain graph1) (domain graph2)
    tgm = TGM.typedMorphism d c m




---------------------------------------------------------------------------------

buildMappings :: PROP -> [G.NodeId] -> [G.EdgeId] -> [G.NodeId] -> [G.EdgeId]
              -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]

--IF NO HAS FREE NODES OR FREE EDGES TO MAP, RETURN THE FOUND MORPHISMO
buildMappings prop [] [] nodesT edgesT tgm =
      case prop of
        ALL  -> all
        MONO -> all
        EPI  -> epimorphism
        ISO  -> isomorphism
      where
        all = return tgm

        isomorphism | L.null nodesT && L.null edgesT = return tgm
                    | otherwise = []

        epimorphism | L.null (orphanNodesTyped tgm) &&
                      L.null (orphanEdgesTyped tgm) = return tgm
                    | otherwise = []

---------------------------------------------------------------------------------

--IF HAS FREE NODES, MAP ALL FREE NODES TO ALL DESTINATION NODES
buildMappings prop (h:t) [] nodesT edgesT tgm
  | L.null nodesT = []
  | otherwise  = do
      y <- nodesT

      --MAP FREE NODES TO ALL TYPE COMPATIBLE DESTINATION NODES
      let tgmN = updateNodesMapping h y nodesT tgm

      case tgmN of
        Just tgm' ->
          --CHOSE BETWEEN INJECTIVE OR NOT
          case prop of
            ALL  -> all
            MONO -> monomorphism
            EPI  -> all
            ISO  -> monomorphism
          where
            monomorphism = buildMappings prop t [] nodesT' edgesT tgm'
            all          = buildMappings prop t [] nodesT  edgesT tgm'
            --REMOVE THE TARGET NODES MAPPED (INJECTIVE MODULE)
            nodesT'   = delete y nodesT
        Nothing  -> []

---------------------------------------------------------------------------------

--IF HAS FREE NODES, AND FREE EDGES, VERIFY THE CURRENT STATUS
buildMappings prop nodes (h:t) nodesT edgesT tgm
  | L.null edgesT = []
  | otherwise  =
    do  --VERIFY THE POSSIBILITY OF A MAPPING BETWEEN h AND THE DESTINATION EDGES
      y <- edgesT
      --MAPPING SRC AND TGT NODES
      let tgmN
            | isNothing tgm1 = Nothing
            | otherwise = tgm2
            where tgm1 = updateNodesMapping (srcE d h) (srcE c y) nodesT tgm
                  tgm2 = updateNodesMapping (tgtE d h) (tgtE c y) nodesT' $ fromJust tgm1
                  d = domain $ domain tgm
                  c = domain $ codomain tgm
                  nodesT' = case prop of
                    MONO -> L.delete (srcE c y) nodesT
                    ISO  -> L.delete (srcE c y) nodesT
                    EPI  -> nodesT
                    ALL  -> nodesT

          --MAPPING SRC EDGE AND TGT EDGE
          tgmE
            | isNothing tgmN = Nothing
            | otherwise = updateEdgesMapping h y edgesT $ fromJust tgmN

      --FOR THE COMPATIBLES MAPPINGS, GO TO THE NEXT STEP
      case tgmE of
        Just tgm' -> do
          let nodes'       = delete (srcE d h) $ delete (tgtE d h) nodes
              d            = domain $ domain tgm
              c            = domain $ codomain tgm
              --REMOVE THE TARGET EDGES AND NODES MAPPED (INJECTIVE MODULE)
              edgesT'      = delete y edgesT
              nodesT'      = delete (srcE c y) $ delete (tgtE c y) nodesT
              monomorphism = buildMappings prop nodes' t nodesT' edgesT' tgm'
              all          = buildMappings prop nodes' t nodesT  edgesT  tgm'
              --CHOSE BETWEEN INJECTIVE OR NOT
          case prop of
            ALL  -> all
            MONO -> monomorphism
            EPI  -> all
            ISO  -> monomorphism
        Nothing  -> []

---------------------------------------------------------------------------------

-- VALIDATION OF A NODE MAPPING
-- VERIFY IF THE TYPES OF n1 AND n2 ARE COMPATIBLE AND UPDATE MAPPING
updateNodesMapping :: G.NodeId -> G.NodeId -> [G.NodeId] -> TGM.TypedGraphMorphism a b
                   -> Maybe (TGM.TypedGraphMorphism a b)
updateNodesMapping n1 n2 nodesT tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm

    if typeN d n1 == typeN c n2 &&
       (((isNothing $ applyNodeTGM tgm n1) && L.elem n2 nodesT) ||
        applyNodeTGM tgm n1 == Just n2)
      then Just $ TGM.typedMorphism d c $ GM.updateNodes n1 n2 m
      else Nothing

---------------------------------------------------------------------------------

-- VALIDATION OF A EDGE MAPPING
-- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLE AND UPDATE MAPPING
updateEdgesMapping :: G.EdgeId -> G.EdgeId -> [G.EdgeId] -> TGM.TypedGraphMorphism a b
                   -> Maybe (TGM.TypedGraphMorphism a b)
updateEdgesMapping e1 e2 edgesT tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm

    if typeE d e1 == typeE c e2 &&
       (((isNothing $ applyEdgeTGM tgm e1) && L.elem e2 edgesT ) ||
        applyEdgeTGM tgm e1 == Just e2)
      then Just $ TGM.typedMorphism d c (GM.updateEdges e1 e2 m)
      else Nothing
