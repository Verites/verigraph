--MORFISMOS DE GRAFOS TIPADOS (HOMO, ISO, INJ, SOBR)
--LEONARDO MARQUES RODRIGUES
--DEZEMBRO/2015

{-# LANGUAGE FlexibleInstances #-}

module Analysis.Matches(
      PROP(..)
    , matches
    , partInjMatches
  )
  where

import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.GraphMorphism as GM
import qualified Graph.Graph as G
import qualified Abstract.Morphism as M
import Graph.GraphRule
import Data.List
import Data.Maybe

--DATA DEFINITION 
-----------------------------------------------------------------------------------------

--DEFINIÇÃO DO TIPO DE DADOS PARA TYPED NODES
data TNodes = TNode {
                     name  :: Int, --Identificação de um Node
                     typeN :: Int  --Tipo de um Node
                    } deriving (Eq)

--Instancia para exibição dos Nodes                             
instance Show TNodes where
  show (TNode name typ) = show name ++ "(" ++ show typ ++ ")"

-----------------------------------------------------------------------------------------

--DEFINIÇÃO DO TIPO DE DADOS PARA EDGES
data TEdges = TEdge {
                      label :: Int, --Identificação de uma Edge
                      srce  :: TNodes, --Node de origem de uma Edge
                      tagt  :: TNodes, --Node de destino de uma Edge
                      typeE :: Int    --Tipo de um aresta
                    } deriving (Eq)
                             
--Instancia para exibição das Edges
instance Show TEdges where
  show (TEdge lbl src tgt typ) = " "   ++ (show $ name src) ++
                                 "--"  ++ show lbl ++"("++ show typ ++")"++
                                 "-->" ++ (show $ name tgt) ++" "

-----------------------------------------------------------------------------------------

--DEFINIÇÃO DO TIPO DE DADOS PARA GRAPHS
data TGraphs = TGraph {
                        nodes :: [TNodes], --Nodes de um Grafo
                        edges :: [TEdges]  --Edges de um Grafo
                      } deriving (Show,Eq)
                               
-----------------------------------------------------------------------------------------
--Flag para Escolha de tipos de Morfismo a ser calculado

data PROP = FREE | INJ | SOB | ISO

------------------------------------------------------------------------------------------
--Mapeamento final de Nodes
type TFN = [(TNodes,TNodes)]

instance {-# OVERLAPPING #-} Show (TNodes,TNodes) where
  show (src,tgt) ="{ " ++ (show src) ++ "/" ++ (show tgt) ++ " }"
  
--Mapeamento final de Edges
type TFE = [(TEdges,TEdges)]

instance {-# OVERLAPPING #-} Show (TEdges,TEdges) where
  show (src,tgt) = "{" ++ (show src) ++ "/" ++ (show tgt) ++ "}"

-----------------------------------------------------------------------------------------

--Definição de dados para um morfismo (exibição dos Resultados em lista)
data TMF = TMF {  --MF = morfismo
                 end_n :: TFN, --Morfismo de nodos
                 end_e :: TFE  --Morfismo de edges
               } deriving (Eq)

--Instancia para exibição dos Resultados em lista
instance Show TMF where
  show (TMF fn fe) = " MF " ++ (show fn) ++ " \n" ++
                     "     " ++ (show fe) ++ " \n"

----------------------------------------------------------------------
--INTERFACE--

--IN--
toTGraph  :: GM.GraphMorphism a b -> TGraphs
toTGraph  t1  = graph
   where
     graph = TGraph nodes edges
     nodes = toTNodes t1 (G.nodes (M.domain t1))
     edges = toTEdges t1 (G.edges (M.domain t1)) nodes


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






in_nac :: TGM.TypedGraphMorphism a b-> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
in_nac nac match = foldl (aux1) new_nodes edges_rule
  where
    dom = M.codomain nac
    cod = M.codomain match
    mapping = GM.empty (M.domain dom) (M.domain cod)
    nodes_rule = G.nodes $ M.domain $ M.domain nac
    edges_rule = G.edges $ M.domain $ M.domain nac

    new_nodes = foldl (aux) (TGM.typedMorphism dom cod mapping) nodes_rule

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


updateMappingNodes :: G.NodeId -> G.NodeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
updateMappingNodes  n1 n2 tgm =
  TGM.typedMorphism  dom cod (GM.updateNodes n1 n2 m)
  where
    dom = M.domain tgm
    cod = M.codomain tgm
    m = TGM.mapping tgm

updateMappingEdges :: G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
updateMappingEdges  e1 e2 tgm =
  TGM.typedMorphism  dom cod (GM.updateEdges e1 e2 m)
  where
    dom = M.domain tgm
    cod = M.codomain tgm
    m = TGM.mapping tgm


partInjMatches :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]
partInjMatches nac m = do
  let
    (TGraph nodes_nac edges_nac) = toTGraph (M.codomain nac)
    (TGraph nodes_inst edges_inst) = toTGraph (M.codomain m)

    tgm = in_nac nac m
    x = M.domain tgm
    y = M.codomain tgm
    
  
    fn :: [TNodes]-> [TNodes] -> TGM.TypedGraphMorphism a b -> TFN
    fn [] _ _=[]
    fn (n:ns) ns' tgm =
      (case GM.applyNode (TGM.mapping tgm) (G.NodeId (name n)) of
          Nothing -> []
          (Just (G.NodeId n')) -> [(n, fromJust $ find (\x -> n' == name x) ns')]) ++ fn ns ns' tgm

    fe :: [TEdges] -> [TEdges] -> TGM.TypedGraphMorphism a b -> TFE
    fe [] _ _=[]
    fe (e:es)  es' tgm =
      (case GM.applyEdge (TGM.mapping tgm) (G.EdgeId (label e)) of
          Nothing -> []
          (Just (G.EdgeId e')) -> [(e, fromJust $ find (\x -> e' == label x) es')]) ++ fe es es' tgm 

    morph :: [TMF]
    morph = concat $ mapAlt nodes_nac edges_nac nodes_inst edges_inst (fn nodes_nac nodes_inst tgm) (fe edges_nac edges_inst tgm) INJ

    morphs = map (\a -> interfaceOUT (end_n a) (end_e a) x y) morph

  rtrn <- map (\b -> TGM.typedMorphism x y b) morphs
  return rtrn

--OUT--

interfaceOUT :: TFN -> TFE -> GM.GraphMorphism a b -> GM.GraphMorphism a b -> GM.GraphMorphism a b
interfaceOUT [] [] t1 t2 = GM.empty (M.domain t1) (M.domain t2)
interfaceOUT [] (fe:fes) t1 t2  = GM.updateEdges (G.EdgeId $ label $ fst fe) (G.EdgeId $ label $ snd fe) (interfaceOUT [] fes t1 t2)
interfaceOUT (fn:fns) fe t1 t2 = GM.updateNodes (G.NodeId $ name $ fst fn) (G.NodeId $ name $ snd fn) (interfaceOUT fns fe t1 t2)

---------------------------------------------------------------------------------

--ENTRADA, CALCULO E SAIDA--

matches :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> PROP -> [TGM.TypedGraphMorphism a b]
matches x y p = do
  let (t1,t2) = (toTGraph x,  toTGraph y)
  let morph = morphism t1 t2 p
  let morphs = map (\a -> interfaceOUT (end_n a) (end_e a) x y) morph
  rtrn <- map (\b -> TGM.typedMorphism x y b) morphs
  return rtrn




  
-------------------------------------------------------------------
{-
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
-}



-------------------------------------------------------------------
--MORPHISM CALC--

morphism :: TGraphs -> TGraphs ->  PROP ->[TMF]
morphism graph1 graph2 injective =
  let
    nodesSrc = nodes graph1
    nodesTgt = nodes graph2
    
    edgesSrc = edges graph1
    edgesTgt = edges graph2

  in  concat $! mapAlt nodesSrc edgesSrc nodesTgt edgesTgt [] [] injective

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
update_nodes_mapping (n1,n2) l@((a,b):t) = if n1==a then if n2==b then Just l else Nothing else
                                             do
                                               l <- update_nodes_mapping (n1,n2) t
                                               return $ (a,b):l

    --VALIDATION OF A EDGE MAPPING
update_edges_mapping :: (TEdges,TEdges) -> TFE -> Maybe TFE

    -- VERIFY IF THE TYPES OF e1 AND e2 ARE COMPATIBLES
update_edges_mapping (e1,e2) l = if (typeE e1) == (typeE e2) then Just ((e1,e2):l)
                                 else Nothing
