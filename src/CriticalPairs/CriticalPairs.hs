module CriticalPairs.CriticalPairs (
   CP (..),
   CriticalPair (..),
   criticalPairs,
   countCP,
   dpo
   ) where

import Graph.GraphRule
import qualified CriticalPairs.GraphPart as GP
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Abstract.Relation as R
import qualified Abstract.Morphism as M
import qualified CriticalPairs.Deletion as D
import CriticalPairs.VeriToGP
import CriticalPairs.GPToVeri
import Graph.Graph
import qualified CriticalPairs.Matches as MT
import Data.List.Utils (countElem)

data CP = FOL | UseDelete | ProduceForbid | Both deriving(Eq,Show)

data CriticalPair a b = CriticalPair {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b,
    cp :: CP
    } deriving (Eq,Show)
    
--instance Show (CriticalPair a b) where
--  show (CriticalPair m1 m2 cp) = "{"++(show $ TGM.mapping m1)++(show $ TGM.mapping m2)++(show cp)++"}"

{-Retorna (número de UseDeletes, número de ProduceForbid)-}
countCP :: GraphRule a b -> GraphRule a b -> (Int,Int)
countCP l r = (useDel+both,proFor+both)
    where
        useDel = countElem UseDelete     list
        proFor = countElem ProduceForbid list
        both   = countElem Both          list
        list   = map (\x -> cp x) (criticalPairs l r)

{-Cálculo dos Pares Críticos-}
criticalPairs :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
criticalPairs l r = criticalPairsAux l r (D.setDeletion l r (GP.genEqClass $ mixLeftRule l r))

criticalPairsAux :: GraphRule a b -> GraphRule a b -> [GP.EqClassGraphMap] -> [CriticalPair a b]
criticalPairsAux l r []     = []
criticalPairsAux l r (x:xs) = (if (satsGluingCondBoth (l,m1) (r,m2) g) then [newCP] else []) ++ (criticalPairsAux l r xs)
  where
    newCP = CriticalPair m1 m2 cp
    cp = criticalPair l r g
    g = GP.eqGraph x
    m1 = mountTGM (left l) "Left"  x
    m2 = mountTGM (left r) "Right" x
    {-m1 = TGM.typedMorphism (M.codomain (left l)) typeMorphismL morphismL
    m2 = TGM.typedMorphism (M.codomain (left r)) typeMorphismR morphismR
    typeMorphismL = setType x (GM.empty (M.codomain morphismL) (M.codomain $ M.domain (left l)))
    typeMorphismR = setType x (GM.empty (M.codomain morphismR) (M.codomain $ M.domain (left r)))
    morphismL = toMorphism x "Left"
    morphismR = toMorphism x "Right"-}

{-Calcula o tipo do conflito de um par epi-}
criticalPair :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> CP
criticalPair l r g = case (useDelete l r g, produceForbid l r g) of
                       (True,True)   -> Both
                       (True,False)  -> UseDelete
                       (False,True)  -> ProduceForbid
                       (False,False) -> FOL

useDelete :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> Bool
useDelete l r ( []  , []  ) = False
useDelete l r ( []  ,edges) = (delByLeft && isInMatchRight) || (useDelete l r ([], tail edges))
  where
    delByLeft      = True `elem` (map D.edelByLeft (head edges))
    isInMatchRight = True `elem` (map GP.eRight (head edges))
useDelete l r (nodes,edges) = (delByLeft && isInMatchRight) || (useDelete l r (tail nodes, edges))
  where
    delByLeft      = True `elem` (map D.ndelByLeft (head nodes))
    isInMatchRight = True `elem` (map GP.nRight (head nodes))

produceForbid :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> Bool
produceForbid l r g = False
    where
        gDoQuadro = map (\x -> D.setDeletion l r $ GP.genEqClass $ mixTGM (right r) x) (nacs l) --[[GP.EqClassGraphMap]]
        firstNacG = head gDoQuadro
        --m' = ..
        firstNacM' = map (\x -> mountTGM (right r) "Right" x) firstNacG --[TGM.TypedGraphMorphism]
        firstNacM = map (dpo l) firstNacM' --[TGM.TypedGraphMorphism]
        firstNacM1 = map (\x -> MT.matches (M.codomain (left l)) (M.codomain x) MT.FREE) firstNacM --[[TGM.TypedGraphMorphism]]
        umM1 = head firstNacM1 --[TGM.TypedGraphMorphism]
        valid = satsGluingCondBoth (l,head umM1) (r,head firstNacM) g

dpo :: GraphRule a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
dpo _ b = b

satsGluingCondBoth :: (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      GP.EqClassGraph ->
                      Bool
satsGluingCondBoth (l,m1) (r,m2) g = (satsGluingCond l "Left" g m1) && (satsGluingCond r "Right" g m2)

satsGluingCond :: GraphRule a b -> String -> GP.EqClassGraph -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCond rule side g m = identificationCondition && danglingCondition && nacsCondition
   where
      identificationCondition = satsDelItems rule side g
      danglingCondition       = satsIncEdges rule side g
      nacsCondition           = satsNacs     rule g m

{-Old version
satsGluingCond :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCond l r g m1 m2 = identificationCondition && danglingCondition && nacsCondition
   where
      identificationCondition = (satsDelItems l "Left" g) && (satsDelItems r "Right" g)
      danglingCondition       = (satsIncEdges l "Left" g) && (satsIncEdges r "Right" g)
      nacsCondition           = (satsNacs l g m1) && (satsNacs r g m2)-}

{-retorna True se as NACs não impossibilitam a aplicação das regras-}
satsNacs :: GraphRule a b -> GP.EqClassGraph -> TGM.TypedGraphMorphism a b -> Bool
satsNacs rule g m = False `notElem` (map (satOneNac rule m) (nacs rule))

{-Old version
satsNacs :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satsNacs l r g m1 m2 = satsNacsL && satsNacsR
   where
      satsNacsL = False `notElem` (map (satOneNac l m1) (nacs l))
      satsNacsR = False `notElem` (map (satOneNac r m2) (nacs r))-}

satOneNac :: GraphRule a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satOneNac rule m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.FREE
      typeNac = M.codomain nac
      typeG   = M.codomain m

{-retorna True se não existirem elementos deletados e preservados ao mesmo tempo pela mesma regra no grafo-}
satsDelItems :: GraphRule a b -> String -> GP.EqClassGraph -> Bool
satsDelItems r side (nodes,edges) = True `notElem` ((map nodeDelPres nodes) ++ (map edgeDelPres edges))
  where
    nodeDelPres nods = isDelAndMore (nleft nods)
    nleft       nods = if side == "Right" then map D.ndelByRight (filter GP.nRight nods) else map D.ndelByLeft (filter GP.nLeft nods)
    edgeDelPres edgs = isDelAndMore (eleft edgs)
    eleft       edgs = if side == "Right" then map D.edelByRight (filter GP.eRight edgs) else map D.edelByLeft (filter GP.eLeft edgs)

isDelAndMore :: [Bool] -> Bool
isDelAndMore l = True `elem` l && length(l) > 1

{-Old version
satsDelItems :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> Bool
satsDelItems l r (nodes,edges) = True `notElem` ((map nodeDelPres nodes) ++ (map edgeDelPres edges))
  where
    nodeDelPres nods = isDelAndMore (nleft nods) || isDelAndMore (nright nods)
    nleft       nods = map D.ndelByLeft  (filter GP.nLeft  nods)
    nright      nods = map D.ndelByRight (filter GP.nRight nods)
    edgeDelPres edgs = isDelAndMore (eleft edgs) || isDelAndMore (eright edgs)
    eleft       edgs = map D.edelByLeft  (filter GP.eLeft  edgs)
    eright      edgs = map D.edelByRight (filter GP.eRight edgs)-}

{- older version
satsDelItems l r ( []  , []  ) = True
satsDelItems l r ( []  ,edges) = if edgeDelPres then False else satsDelItems l r ([],tail edges)
  where
    edgeDelPres = isDelAndMore left || isDelAndMore right
    left        = map D.edelByLeft  (filter GP.eLeft  (head edges))
    right       = map D.edelByRight (filter GP.eRight (head edges))
    
satsDelItems l r (nodes,edges) = if nodeDelPres then False else satsDelItems l r (tail nodes,edges)
  where
    nodeDelPres = isDelAndMore left || isDelAndMore right
    left        = map D.ndelByLeft  (filter GP.nLeft  (head nodes))
    right       = map D.ndelByRight (filter GP.nRight (head nodes))
-}

{-retorna True se não existirem dangling edges no grafo-}
satsIncEdges :: GraphRule a b -> String -> GP.EqClassGraph -> Bool
satsIncEdges l side ( []  ,  _  ) = True
satsIncEdges l side (nodes,edges) = if (ruleDel side) then False else satsIncEdges l side (tail nodes,edges)
  where
    ruleDel              s    = nodeWillBeDeleted s && hasIncidentEdge s
    nodeWillBeDeleted "Right" = True `elem` (map D.ndelByRight (head nodes))
    nodeWillBeDeleted "Left"  = True `elem` (map D.ndelByLeft  (head nodes))
    hasIncidentEdge   "Right" = hasIncEdge D.edelByRight edges (head nodes)
    hasIncidentEdge   "Left"  = hasIncEdge D.edelByLeft  edges (head nodes)

{-Old version
satsIncEdges :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> Bool
satsIncEdges l r ( []  ,  _  ) = True
satsIncEdges l r (nodes,edges) = if (ruleDel "Left") || (ruleDel "Right") then False else satsIncEdges l r (tail nodes,edges)
  where
    ruleDel              s    = nodeWillBeDeleted s && hasIncidentEdge s
    nodeWillBeDeleted "Right" = True `elem` (map D.ndelByRight (head nodes))
    nodeWillBeDeleted "Left"  = True `elem` (map D.ndelByLeft  (head nodes))
    hasIncidentEdge   "Right" = hasIncEdge D.edelByRight edges (head nodes)
    hasIncidentEdge   "Left"  = hasIncEdge D.edelByLeft  edges (head nodes)-}

hasIncEdge :: (GP.Edge -> Bool) -> [[GP.Edge]] -> [GP.Node] -> Bool
hasIncEdge f e []     = False
hasIncEdge f e (n:ns) = (hasIncEdgeEachNode f n e) || (hasIncEdge f e ns)

hasIncEdgeEachNode :: (GP.Edge -> Bool) -> GP.Node -> [[GP.Edge]] -> Bool
hasIncEdgeEachNode f n []     = False
hasIncEdgeEachNode f n (e:xs) = (hasIncEdgeEachNodeEveryEdge (map f e) n e) || (hasIncEdgeEachNode f n xs)

hasIncEdgeEachNodeEveryEdge :: [Bool] -> GP.Node -> [GP.Edge] -> Bool
hasIncEdgeEachNodeEveryEdge _ _  []    = False
hasIncEdgeEachNodeEveryEdge l n (e:xs) = ((isSource || isTarget) && edgeBeDeleted) || (hasIncEdgeEachNodeEveryEdge l n xs)
   where
      isSource      = (GP.source e) == n
      isTarget      = (GP.target e) == n
      edgeBeDeleted = True `notElem` l
