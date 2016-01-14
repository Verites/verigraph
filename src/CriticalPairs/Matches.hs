--MORFISMOS DE GRAFOS TIPADOS (HOMO, ISO, INJ, SOBR)
--LEONARDO MARQUES RODRIGUES
--DEZEMBRO/2015

module CriticalPairs.Matches (matches,
                PROP(..))
       where


import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.GraphMorphism as GM
import qualified Graph.Graph as G
import qualified Abstract.Morphism as M
import Graph.GraphRule
import Data.List
import Data.Maybe
import CriticalPairs.Estruturas


----------------------------------------------------------------------
--INTERFACE--

--IN--
interfaceIN :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> (TGraphs, TGraphs)
interfaceIN t1 t2 = (graph1,graph2)
   where
     graph1 = TGraph nodes1 edges1
     nodes1 = toTNodes t1 (G.nodes (M.domain t1))
     edges1 = toTEdges t1 (G.edges (M.domain t1)) nodes1
     graph2 = TGraph nodes2 edges2
     nodes2 = toTNodes t2 (G.nodes (M.domain t2))
     edges2 = toTEdges t2 (G.edges (M.domain t2)) nodes2

toTNodes :: GM.GraphMorphism a b -> [G.NodeId] -> [TNodes]
toTNodes t [] = []
toTNodes t id@((G.NodeId n):ns) = case (GM.applyNode t (G.NodeId n)) of
  Just (G.NodeId n') -> (TNode n n'):toTNodes t ns
  Nothing -> toTNodes t ns

toTEdges :: GM.GraphMorphism a b -> [G.EdgeId] -> [TNodes] -> [TEdges]
toTEdges t [] _ = []
toTEdges t id@((G.EdgeId n):ns) nodes = case (GM.applyEdge t (G.EdgeId n)) of
  Just (G.EdgeId n') -> (TEdge n src tgt n'):toTEdges t ns nodes
    where
      src = fromJust $ find (\x -> (name x) == s1) nodes
      tgt = fromJust $ find (\x -> (name x) == t1) nodes
      (G.NodeId s1) = fromJust $ G.sourceOf (M.domain t) (G.EdgeId n)
      (G.NodeId t1) = fromJust $ G.targetOf (M.domain t) (G.EdgeId n)
  Nothing -> toTEdges t ns nodes

--FALTA A INTERFACE PARA A INSTANCIAÇÃO PARCIAL DE ENTRADA --

--OUT--

interfaceOUT :: TFN -> TFE -> GM.GraphMorphism a b -> GM.GraphMorphism a b -> GM.GraphMorphism a b
interfaceOUT [] [] t1 t2 = GM.empty (M.domain t1) (M.domain t2)
interfaceOUT [] (fe:fes) t1 t2  = GM.updateEdges (G.EdgeId $ label $ fst fe) (G.EdgeId $ label $ snd fe) (interfaceOUT [] fes t1 t2)
interfaceOUT (fn:fns) fe t1 t2 = GM.updateNodes (G.NodeId $ name $ fst fn) (G.NodeId $ name $ snd fn) (interfaceOUT fns fe t1 t2)

---------------------------------------------------------------------------------

--ENTRADA, CALCULO E SAIDA--

matches :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> PROP -> [TGM.TypedGraphMorphism a b]
matches x y p = do
  let (t1,t2) = interfaceIN x y
      morph = morphism t1 t2 p
      morphs = map (\a -> interfaceOUT (end_n a) (end_e a) x y) morph
  rtrn <- map (\b -> TGM.typedMorphism x y b) morphs
  return rtrn




  
-------------------------------------------------------------------

make_st :: TGraphs -> TGraphs -> TFN -> TFE -> (TGraphs,TGraphs)
make_st graph1 graph2 fn fe = (graph1',graph2')
  where
    nodes1 = make_n_e fst (nodes graph1) fn
    edges1 = make_n_e fst (edges graph1) fe
    graph1' = TGraph nodes1 edges1

    nodes2 = make_n_e snd (nodes graph2) fn
    edges2 = make_n_e snd (edges graph2) fe
    graph2' = TGraph nodes2 edges2

    make_n_e :: Eq a => ((a,a) -> a) -> [a] -> [(a,a)] -> [a]
    make_n_e _ x [] = x
    make_n_e _ [] _ = []
    make_n_e pos x f@(h:t) = make_n_e pos x' t
      where x' = delete (pos h) x




-------------------------------------------------------------------
--MORPHISM CALC--

morphism :: TGraphs -> TGraphs ->  PROP ->[TMF]
morphism graph1 graph2 injective =
  let
    nodesSrc = nodes graph1
    nodesTgt = nodes graph2
    
    edgesSrc = edges graph1
    edgesTgt = edges graph2

    mapAlt :: [TNodes] -> [TEdges] ->[TNodes] -> [TEdges] -> TFN -> TFE -> PROP -> [[TMF]]

    --IF NO HAS FREE NODES OR FREE EDGES TO MAP, RETURN THE FOUND MORPHISMO
    mapAlt [] [] nodesT edgesT fn fe prop =
      case prop of
        FREE -> free
        INJ  -> free
        SOB  -> surjective nodesT edgesT fn fe
        ISO  -> isomorphic
      where
        free = return $! [TMF fn fe]
        
        isomorphic | null nodesT && null edgesT = return $! [TMF fn fe]
                   | otherwise = return $! []

        surjective n e fns fes | surj' n fns && surj' e fes = return $! [TMF fn fe]
                               | otherwise = return $! []
          where
            surj' [] _ = True
            surj' _ [] = False
            surj' x ((_,y):ys) = surj' x' ys
              where
                x' = delete y x
                
    --IF HAS FREE NODES, AND FREE EDGES, VERIFY THE CURRENT STATUS
    mapAlt nodes edges@(h:t) nodesT edgesT fn fe prop
      | null edgesT = return []
      | otherwise  = do
                       y <- edgesT
                       
                       --VERIFY THE POSSIBILITY OF A MAPPING BETWEEN h AND THE DESTINATION EDGES
                       let nodes_map = do
                                         fn1 <- update_nodes_mapping (srce h, srce y) fn
                                         fn2 <- update_nodes_mapping (tagt h, tagt y) fn1
                                         return fn2
                           edges_map = do
                                         fe1 <- update_edges_mapping (h,y) fe
                                         return fe1
                           
                       --FOR THE COMPATIBLES MAPPINGS, GO TO THE NEXT STEP
                       case nodes_map of
                         Just fn' -> case edges_map of
                                       Just fe' -> do
                                                     let nodos'  = delete (srce h) $ delete (tagt h) nodes
                                                         fn_temp = [(srce h, srce y),(tagt h, tagt y)]
                                                         --REMOVE THE TARGET EDGES AND NODES MAPPED (INJECTIVE MODULE)
                                                         edgesT'   = delete y edgesT
                                                         nodesT'   = delete (srce y) $! delete (tagt y) nodesT
                                                         injective = return $! concat $!
                                                                     mapAlt nodos' t nodesT' edgesT' fn' fe' prop
                                                         free      = return $! concat $!
                                                                     mapAlt nodos' t nodesT edgesT fn' fe' prop
                                                     --CHOSE BETWEEN INJECTIVE OR NOT
                                                     case prop of
                                                       FREE -> free
                                                       INJ  -> injective
                                                       SOB  -> free
                                                       ISO  -> injective
                                       Nothing  -> return $! []
                         Nothing  -> return $! []

    --IF HAS FREE NODES, MAP ALL FREE NODES TO ALL DESTINATION NODES
    mapAlt nodes@(h:t) [] nodesT edgesT fn fe prop
      | null nodesT = return []
      | otherwise  = do
                       y <- nodesT
                       --MAP FREE NODES TO ALL TYPE COMPATIBLE DESTINATION NODES
                       let nodes_map = do
                                         fn1 <- update_nodes_mapping (h,y) fn
                                         return fn1
                           --REMOVE THE TARGET NODES MAPPED (INJECTIVE MODULE) 
                           nodesT' = delete y nodesT
                       case nodes_map of
                         Just fn' ->
                           --CHOSE BETWEEN INJECTIVE OR NOT
                           case prop of
                             FREE -> free
                             INJ  -> injective
                             SOB  -> free
                             ISO  -> injective
                           where
                                injective = mapAlt t [] nodesT' edgesT fn' fe prop
                                free      = mapAlt t [] nodesT edgesT fn' fe prop
                         Nothing  -> return $! []

    --VALIDATION OF A NODE MAPPING
    update_nodes_mapping :: (TNodes,TNodes) -> TFN -> Maybe TFN

    --IF HAS NO MAPPING YET, VERIFY THE TYPES OF NODES
    update_nodes_mapping (n1,n2) []          = if (typeN n1) == (typeN n2) then Just [(n1,n2)]
                                                                           else Nothing
    --IF HAS A MAPPING (OR MORE), VERIFY IF n1 ALREADY WAS MAPPED AND THE COMPATIBILITY
    --OF THE EXISTENT MAPPING
    update_nodes_mapping (n1,n2) l@((a,b):t) = if n1==a then if n2==b then Just l
                                                                      else Nothing
                                                        else do l <- update_nodes_mapping (n1,n2) t
                                                                return $! (a,b):l

    --VALIDATION OF A EDGE MAPPING
    update_edges_mapping :: (TEdges,TEdges) -> TFE -> Maybe TFE

    -- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLES
    update_edges_mapping (e1,e2) l = if (typeE e1) == (typeE e2) then Just ((e1,e2):l)
                                                                 else Nothing

  in  concat $! mapAlt nodesSrc edgesSrc nodesTgt edgesTgt [] [] injective



-----------------
--TESTS--

iN = G.insertNode
iE = G.insertEdge
uN = GM.updateNodes
uE = GM.updateEdges

gtn1 = G.NodeId 1
gtn2 = G.NodeId 2
gtn3 = G.NodeId 3
gtn4 = G.NodeId 4
gte1 = G.EdgeId 1
gte2 = G.EdgeId 2
gte3 = G.EdgeId 3
gte4 = G.EdgeId 4
gte5 = G.EdgeId 5

grafotipo :: G.Graph a b
grafotipo = iE gte5 gtn3 gtn4 $ iE gte4 gtn2 gtn4 $ iE gte3 gtn2 gtn3 $ iE gte2 gtn2 gtn1 $ iE gte1 gtn3 gtn1 $ iN gtn4 $ iN gtn3 $ iN gtn2 $ iN gtn1 G.empty


n200 = G.NodeId 200
n201 = G.NodeId 201

n300 = G.NodeId 300

n400 = G.NodeId 400

lr5 :: G.Graph a b
lr5 = iN n201 $ iN n200 G.empty

kr5 :: G.Graph a b
kr5 = iN n300 G.empty

rr5 :: G.Graph a b
rr5 = iN n400 G.empty

--tipagem
tlr5 :: GM.GraphMorphism a b
tlr5 = uN n201 gtn1 $ uN n200 gtn1 $ GM.empty lr5 grafotipo

tkr5 :: GM.GraphMorphism a b
tkr5 = uN n300 gtn1 $ GM.empty kr5 grafotipo

trr5 :: GM.GraphMorphism a b
trr5 = uN n400 gtn1 $ GM.empty rr5 grafotipo

--LR
kr5_lr5 :: GM.GraphMorphism a b
kr5_lr5 = uN n300 n200 $ GM.empty kr5 lr5

l5 :: TGM.TypedGraphMorphism a b
l5 = TGM.typedMorphism tkr5 tlr5 kr5_lr5

kr5_rr5 :: GM.GraphMorphism a b
kr5_rr5 = uN n300 n400 $ GM.empty kr5 rr5

r5 :: TGM.TypedGraphMorphism a b
r5 = TGM.typedMorphism tkr5 trr5 kr5_rr5

teste :: GraphRule a b
teste = graphRule l5 r5 []

{-sendMSG-}
n11 = G.NodeId 11
n13 = G.NodeId 13
n14 = G.NodeId 14
e11 = G.EdgeId 11

n21 = G.NodeId 21
n23 = G.NodeId 23
n24 = G.NodeId 24

n31 = G.NodeId 31
n33 = G.NodeId 33
n34 = G.NodeId 34
e35 = G.EdgeId 35

lr1 :: G.Graph a b
lr1 = iE e11 n13 n11 $ iN n14 $ iN n13 $ iN n11 G.empty

kr1 :: G.Graph a b
kr1 = iN n24 $ iN n23 $ iN n21 G.empty

rr1 :: G.Graph a b
rr1 = iE e35 n33 n34 $ iN n34 $ iN n33 $ iN n31 G.empty

--tipagem
tlr1 :: GM.GraphMorphism a b
tlr1 = uE e11 gte1 $ uN n14 gtn4 $ uN n13 gtn3 $ uN n11 gtn1 $ GM.empty lr1 grafotipo

tkr1 :: GM.GraphMorphism a b
tkr1 = uN n24 gtn4 $ uN n23 gtn3 $ uN n21 gtn1 $ GM.empty kr1 grafotipo

trr1 :: GM.GraphMorphism a b
trr1 = uE e35 gte5 $ uN n34 gtn4 $ uN n33 gtn3 $ uN n31 gtn1 $ GM.empty rr1 grafotipo

--LR
kr1_lr1 :: GM.GraphMorphism a b
kr1_lr1 = uN n24 n14 $ uN n23 n13 $ uN n21 n11 $ GM.empty kr1 lr1

l1 :: TGM.TypedGraphMorphism a b
l1 = TGM.typedMorphism tkr1 tlr1 kr1_lr1

kr1_rr1 :: GM.GraphMorphism a b
kr1_rr1 = uN n24 n34 $ uN n23 n33 $ uN n21 n31 $ GM.empty kr1 rr1

r1 :: TGM.TypedGraphMorphism a b
r1 = TGM.typedMorphism tkr1 trr1 kr1_rr1

sendMsg :: GraphRule a b
sendMsg = graphRule l1 r1 []

{-receiveMSG-}
n71 = G.NodeId 71
n72 = G.NodeId 72
n73 = G.NodeId 73
n74 = G.NodeId 74
e73 = G.EdgeId 73
e75 = G.EdgeId 75

n81 = G.NodeId 81
n82 = G.NodeId 82
n83 = G.NodeId 83
n84 = G.NodeId 84
e83 = G.EdgeId 83

n91 = G.NodeId 91
n92 = G.NodeId 92
n93 = G.NodeId 93
n94 = G.NodeId 94
e93 = G.EdgeId 93
e91 = G.EdgeId 91

lr3 :: G.Graph a b
lr3 = iE e75 n73 n74 $ iE e73 n72 n73 $ iN n71 $ iN n72 $ iN n73 $ iN n74 G.empty

kr3 :: G.Graph a b
kr3 = iE e83 n82 n83 $ iN n84 $ iN n83 $ iN n82 $ iN n81 G.empty

rr3 :: G.Graph a b
rr3 = iE e91 n93 n91 $ iE e93 n92 n93 $ iN n94 $ iN n93 $ iN n92 $ iN n91 G.empty

--tipagem
tlr3 :: GM.GraphMorphism a b
tlr3 = uE e75 gte5 $ uE e73 gte3 $ uN n74 gtn4 $ uN n73 gtn3 $ uN n72 gtn2 $ uN n71 gtn1 $ GM.empty lr3 grafotipo

tkr3 :: GM.GraphMorphism a b
tkr3 = uE e83 gte3 $ uN n84 gtn4 $ uN n83 gtn3 $ uN n82 gtn2 $ uN n81 gtn1 $ GM.empty kr3 grafotipo

trr3 :: GM.GraphMorphism a b
trr3 = uE e91 gte1 $ uE e93 gte3 $ uN n94 gtn4 $ uN n93 gtn3 $ uN n92 gtn2 $ uN n91 gtn1 $ GM.empty rr3 grafotipo

--LR
kr3_lr3 :: GM.GraphMorphism a b
kr3_lr3 = uE e83 e73 $ uN n81 n71 $ uN n82 n72 $ uN n83 n73 $ uN n84 n74 $ GM.empty kr3 lr3

l3 :: TGM.TypedGraphMorphism a b
l3 = TGM.typedMorphism tkr3 tlr3 kr3_lr3

kr3_rr3 :: GM.GraphMorphism a b
kr3_rr3 = uE e83 e93 $ uN n81 n91 $ uN n81 n91 $ uN n82 n92 $ uN n83 n93 $ uN n84 n94 $ GM.empty kr3 rr3

r3 :: TGM.TypedGraphMorphism a b
r3 = TGM.typedMorphism tkr3 trr3 kr3_rr3

receiveMSG :: GraphRule a b
receiveMSG = graphRule l3 r3 []

l = M.codomain (left receiveMSG)
r = M.codomain (right receiveMSG)
