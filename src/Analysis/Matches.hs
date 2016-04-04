--MORFISMOS DE GRAFOS TIPADOS (HOMO, ISO, INJ, SOBR)
--LEONARDO MARQUES RODRIGUES
--DEZEMBRO/2015

{-# LANGUAGE FlexibleInstances #-}

module Analysis.Matches--(
  --    PROP(..)
  --  , matches
  --  , partInjMatches
  --)
  where

import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.GraphMorphism as GM
import qualified Graph.Graph as G
import Abstract.Morphism
import Graph.GraphRule
import Data.List
import Data.Maybe



-- | Data type definition to choose specifics propertys of a morphism
data PROP = FREE | INJ | SOB | ISO

--ALIAS OF MOST USED FUNCTIONS --

nodes g = G.nodes g --NODES OF A GRAPH
edges g = G.edges g --EDGES OF A GRAPH

mapping  tgm = TGM.mapping tgm --MAPPING OF A TYPEDGRAPHMORPHISM

getNodeMapping = TGM.applyNodeTGM --GET A MAPPING BETWEEN TWO NODES OF TYPEDGRAPH
getEdgeMapping = TGM.applyEdgeTGM --GET A MAPPING BETWEEN TWO EDGES OF TYPEDGRAPH

orphanNodes = TGM.orphanNodesTyped --GET ORPHANS NODES OF A TYPEDGRAPHMORPHISM
orphanEdges = TGM.orphanEdgesTyped --GET ORPHANS EDGES OF A TYPEDGRAPHMORPHISM

tgtE gm e = fromJust $ G.targetOf gm e --TARGET OF A EDGE
srcE gm e = fromJust $ G.sourceOf gm e --SOURCE OF A EDGE

typeN gm n = fromJust $ GM.applyNode gm n --TYPE OF A NODE
typeE gm e = fromJust $ GM.applyEdge gm e --TYPE OF A EDGE




-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist.
updateNodeRelation :: G.NodeId -> G.NodeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
updateNodeRelation n1 n2 tgm =
  let dom = domain tgm
      cod = codomain tgm
      m   = mapping tgm
  in TGM.typedMorphism dom cod (GM.updateNodes n1 n2 m)

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelation :: G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
updateEdgeRelation e1 e2 tgm =
  let dom = domain tgm
      cod = codomain tgm
      m   = mapping tgm
  in TGM.typedMorphism dom cod (GM.updateEdges e1 e2 m)






---------------------------------------------------------------------------------
partInjMatches :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]
partInjMatches n m =
  do
    let buildMap :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
        buildMap n m = q
          where
            nodesN = nodes $ domain $ codomain n
            edgesN = edges $ domain $ codomain n
       
            nodesG = nodes $ domain $ codomain m
            edgesG = edges $ domain $ codomain m
    
            nodesL = nodes $ domain $ domain n
            edgesL = edges $ domain $ domain n

            domQ   = codomain n
            codQ   = codomain m
            mapQ   = GM.empty (domain domQ) (domain codQ)
            q      = TGM.typedMorphism domQ codQ mapQ


            q'     = foldl (\x -> updateEdgeRelation (fromJust $ getEdgeMapping n x) (fromJust $ getEdgeMapping m x) q) (q) (edgesL)


foldl (aux) (TGM.typedMorphism dom cod mapping) nodes_rule
    
            
    aux :: TGM.TypedGraphMorphism a b -> G.NodeId -> TGM.TypedGraphMorphism a b
    aux match_nac n =  updateMappingNodes x y match_nac
      where
        (Just x) = GM.applyNode (TGM.mapping nac) n
        (Just y) = GM.applyNode (TGM.mapping match) n

    aux1 :: TGM.TypedGraphMorphism a b -> G.EdgeId -> TGM.TypedGraphMorphism a b
    aux1 match_nac e =  updateMappingEdges x y match_nac
      where
        (Just x) = GM.applyEdge (TGM.mapping nac) e
        (Just y) = GM.applyEdge (TGM.mapping match) e





    
    []
      




































---------------------------------------------------------------------------------
matches :: PROP -> GM.GraphMorphism a b-> GM.GraphMorphism a b-> [TGM.TypedGraphMorphism a b]
matches prop graph1 graph2 = concat $! mapAlt prop nodesSrc edgesSrc nodesTgt edgesTgt tgm
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
mapAlt :: PROP -> [G.NodeId] -> [G.EdgeId] -> [G.NodeId] -> [G.EdgeId]
          -> TGM.TypedGraphMorphism a b -> [[TGM.TypedGraphMorphism a b]]

--IF NO HAS FREE NODES OR FREE EDGES TO MAP, RETURN THE FOUND MORPHISMO
mapAlt prop [] [] nodesT edgesT tgm =
      case prop of
        FREE -> free
        INJ  -> free
        SOB  -> surjective
        ISO  -> isomorphic
      where
        free = return [tgm]

        isomorphic | null nodesT && null edgesT = return [tgm]
                   | otherwise = return []

        surjective | null (orphanNodes tgm) && null (orphanEdges tgm) = return [tgm]
                   | otherwise = return []

--IF HAS FREE NODES, AND FREE EDGES, VERIFY THE CURRENT STATUS
mapAlt prop nodes edges@(h:t) nodesT edgesT tgm
      | null edgesT = return []
      | otherwise  = do
                       y <- edgesT

                       --VERIFY THE POSSIBILITY OF A MAPPING BETWEEN h AND THE DESTINATION EDGES

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
                           let nodes'    = delete (srcE d h) $ delete (tgtE d h) nodes
                               d         = domain $ domain tgm
                               c         = domain $ codomain tgm

                               --REMOVE THE TARGET EDGES AND NODES MAPPED (INJECTIVE MODULE)
                               edgesT'   = delete y edgesT
                               nodesT'   = delete (srcE c y) $ delete (tgtE c y) nodesT
                               injective = return $ concat $ mapAlt prop nodes' t nodesT' edgesT' tgm'
                               free      = return $ concat $ mapAlt prop nodes' t nodesT  edgesT  tgm'
                           --CHOSE BETWEEN INJECTIVE OR NOT
                           case prop of
                               FREE -> free
                               INJ  -> injective
                               SOB  -> free
                               ISO  -> injective
                         Nothing  -> return []

--IF HAS FREE NODES, MAP ALL FREE NODES TO ALL DESTINATION NODES
mapAlt prop nodes@(h:t) [] nodesT edgesT tgm
      | null nodesT = return []
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
                             FREE -> free
                             INJ  -> injective
                             SOB  -> free
                             ISO  -> injective
                           where
                                injective = mapAlt prop t [] nodesT' edgesT tgm'
                                free      = mapAlt prop t [] nodesT  edgesT tgm'
                                --REMOVE THE TARGET NODES MAPPED (INJECTIVE MODULE) 
                                nodesT'   = delete y nodesT
                         Nothing  -> return []


-- VALIDATION OF A NODE MAPPING
-- VERIFY IF THE TYPES OF n1 AND n2 ARE COMPATIBLE AND UPDATE MAPPING
updateNodesMapping :: G.NodeId -> G.NodeId -> TGM.TypedGraphMorphism a b -> Maybe (TGM.TypedGraphMorphism a b)
updateNodesMapping n1 n2 tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm
        
    if (typeN d n1 == typeN c n2) &&
       (isNothing (getNodeMapping tgm n1) || (getNodeMapping tgm n1 == Just n2))
      then Just $ TGM.typedMorphism d c (GM.updateNodes n1 n2 m)
      else Nothing
  
-- VALIDATION OF A EDGE MAPPING
-- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLE AND UPDATE MAPPING
updateEdgesMapping :: G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> Maybe (TGM.TypedGraphMorphism a b)
updateEdgesMapping e1 e2 tgm =
  do
    let d = domain tgm
        c = codomain tgm
        m = mapping tgm
        
    if (typeE d e1 == typeE c e2) &&
       (isNothing (getEdgeMapping tgm e1) || (getEdgeMapping tgm e1 == Just e2))
      then Just $ TGM.typedMorphism d c (GM.updateEdges e1 e2 m)
      else Nothing



