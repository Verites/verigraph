{-# LANGUAGE FlexibleInstances #-}

module CriticalPairs.Estruturas  where


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

--instance {-# OVERLAPPING #-} Show (TNodes,TNodes) where
--  show (src,tgt) ="{ " ++ (show src) ++ "/" ++ (show tgt) ++ " }"
  
--Mapeamento final de Edges
type TFE = [(TEdges,TEdges)]

--instance {-# OVERLAPPING #-} Show (TEdges,TEdges) where
--  show (src,tgt) = "{" ++ (show src) ++ "/" ++ (show tgt) ++ "}"

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

-----------------------------------------------------------------------------------------
