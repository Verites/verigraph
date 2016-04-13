module Graph.FindMorphism () where

import           Abstract.Morphism
import           Data.List
import           Data.Maybe
import qualified Graph.Graph              as G
import qualified Graph.GraphMorphism      as GM
import           Graph.GraphRule
import qualified Graph.TypedGraphMorphism as TGM

instance FindMorphism (TGM.TypedGraphMorphism a b) where
  matches = matches'
  partInjMatches = partInjMatches'

--ALIAS OF MOST USED FUNCTIONS --
nodes g = G.nodes g --NODES OF A GRAPH
edges g = G.edges g --EDGES OF A GRAPH

tgtE gm e = fromJust $ G.targetOf gm e --TARGET OF A EDGE
srcE gm e = fromJust $ G.sourceOf gm e --SOURCE OF A EDGE

--TYPE OF A NODE
typeN gm n = fromMaybe (error "NODE NOT TYPED") $ GM.applyNode gm n

--TYPE OF A EDGE
typeE gm e = fromMaybe (error "EDGE NOT TYPED") $ GM.applyEdge gm e


mapping  tgm = TGM.mapping tgm --MAPPING OF A TYPEDGRAPHMORPHISM

nodeMapping = TGM.applyNodeTGM --GET A MAPPING BETWEEN TWO NODES OF TYPEDGRAPH
edgeMapping = TGM.applyEdgeTGM --GET A MAPPING BETWEEN TWO EDGES OF TYPEDGRAPH

orphanNodes = TGM.orphanNodesTyped --GET ORPHANS NODES OF A TYPEDGRAPHMORPHISM
orphanEdges = TGM.orphanEdgesTyped --GET ORPHANS EDGES OF A TYPEDGRAPHMORPHISM




---------------------------------------------------------------------------------

-- | Finds matches __/q/__ .
--
--   Partially injective. (Injective out of __/m/__)
partInjMatches' :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
               -> [TGM.TypedGraphMorphism a b]
partInjMatches' nac match =
  do
    let
      --NODES AND EDGES FROM NAC
      nodesN = nodes $ domain $ codomain nac
      edgesN = edges $ domain $ codomain nac

      --NODES AND EDGES FROM INSTANCE GRAPH
      nodesG = nodes $ domain $ codomain match
      edgesG = edges $ domain $ codomain match

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
      composeEdges tgm edge@(h:t) =
        do
          let edgeNac = fromMaybe (error "EDGE NOT MAPPING L -> N") $
                                  edgeMapping nac h
              edgeG   = fromMaybe (error "EDGE NOT MAPPING L -> G") $
                                  edgeMapping match h

              dom     = domain tgm
              cod     = codomain tgm
              m       = mapping tgm

              tgm' = if (typeE dom edgeNac == typeE cod edgeG) &&
                        (isNothing (edgeMapping tgm edgeNac) ||
                         (edgeMapping tgm edgeNac == Just edgeG))
                     then Just $ TGM.typedMorphism dom cod
                                 (GM.updateEdges edgeNac edgeG m)
                     else Nothing

          case tgm' of
            Just tgm'' -> composeEdges tgm'' t
            Nothing -> Nothing

      --VERIFY NODES MAPPING N <- l AND L -> G AND BUILD A N -> G
      --PARTIAL NODES MORPHISM
      composeNodes :: TGM.TypedGraphMorphism a b -> [G.NodeId]
                   -> Maybe (TGM.TypedGraphMorphism a b)
      composeNodes tgm [] = Just tgm
      composeNodes tgm node@(h:t) =
        do
          let nodeNac = fromMaybe (error "NODE NOT MAPPED L->N") $
                        nodeMapping nac h
              nodeG   = fromMaybe (error "NODE NOT MAPPED L->G") $
                        nodeMapping match h

              dom     = domain tgm
              cod     = codomain tgm
              m       = mapping tgm

              tgm' = if (typeN dom nodeNac == typeN cod nodeG) &&
                        (isNothing (nodeMapping tgm nodeNac) ||
                         (nodeMapping tgm nodeNac == Just nodeG))
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
              notMappedNodes tgm node = isNothing $ nodeMapping tgm node
          --DELETE FROM QUEUE ALREADY MAPPED SOURCE EDGES (EDGES FROM NAC)
          edgesSrc = filter (notMappedEdges q2) (edges $ domain domQ)
            where
              notMappedEdges tgm edge = isNothing $ edgeMapping tgm edge

          --REMOVE FROM TARGET LIST NODES ALREADY MAPPED (NODES FROM G)
          nodesTgt = orphanNodes q2

          --REMOVE FROM TARGET LIST EDGES ALREADY MAPPED (EDGES FROM G)
          edgesTgt = orphanEdges q2

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

        isomorphism | null nodesT && null edgesT = return tgm
                    | otherwise = []

        epimorphism | null (orphanNodes tgm) &&
                      null (orphanEdges tgm) = return tgm
                    | otherwise = []

---------------------------------------------------------------------------------

--IF HAS FREE NODES, MAP ALL FREE NODES TO ALL DESTINATION NODES
buildMappings prop nodes@(h:t) [] nodesT edgesT tgm
  | null nodesT = []
  | otherwise  = do
      y <- nodesT

      --MAP FREE NODES TO ALL TYPE COMPATIBLE DESTINATION NODES
      let tgmN = if isNothing tgm1
                 then Nothing
                 else tgm1
            where
              tgm1 = updateNodesMapping h y tgm

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
buildMappings prop nodes edges@(h:t) nodesT edgesT tgm
  | null edgesT = []
  | otherwise  =
    do  --VERIFY THE POSSIBILITY OF A MAPPING BETWEEN h AND THE DESTINATION EDGES
      y <- edgesT
      --MAPPING SRC AND TGT NODES
      let tgmN
            | isNothing tgm1 = Nothing
            | isNothing tgm2 = Nothing
            | otherwise = tgm2
            where tgm1 = updateNodesMapping (srcE d h) (srcE c y) tgm
                  tgm2 = updateNodesMapping (tgtE d h) (tgtE c y) $ fromJust tgm1
                  d = domain $ domain tgm
                  c = domain $ codomain tgm

          --MAPPING SRC EDGE AND TGT EDGE
          tgmE
            | isNothing tgmN = Nothing
            | isNothing tgm3 = Nothing
            | otherwise = tgm3
            where tgm3 = updateEdgesMapping h y $fromJust tgmN

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
updateNodesMapping :: G.NodeId -> G.NodeId -> TGM.TypedGraphMorphism a b
                   -> Maybe (TGM.TypedGraphMorphism a b)
updateNodesMapping n1 n2 tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm

    if (typeN d n1 == typeN c n2) &&
       (isNothing (nodeMapping tgm n1) || (nodeMapping tgm n1 == Just n2))
      then Just $ TGM.typedMorphism d c (GM.updateNodes n1 n2 m)
      else Nothing

---------------------------------------------------------------------------------

-- VALIDATION OF A EDGE MAPPING
-- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLE AND UPDATE MAPPING
updateEdgesMapping :: G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b
                   -> Maybe (TGM.TypedGraphMorphism a b)
updateEdgesMapping e1 e2 tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm

    if (typeE d e1 == typeE c e2) &&
       (isNothing (edgeMapping tgm e1) || (edgeMapping tgm e1 == Just e2))
      then Just $ TGM.typedMorphism d c (GM.updateEdges e1 e2 m)
      else Nothing
