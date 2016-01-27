module CriticalPairs.CriticalPairsTeste
 (
   --CP2 (..),
   --CriticalPair2 (..),
   criticalPairs2,
   countCP2,
   satsGluingCondBoth,
   satsGluingCond,
   f1,
   f2,
   f3
   )
    where

import Graph.GraphRule
import qualified CriticalPairs.GraphPart as GP
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.Rewriting as RW
import qualified Abstract.Relation as R
import qualified Abstract.Morphism as M
import qualified CriticalPairs.Deletion as D
import CriticalPairs.VeriToGP
import CriticalPairs.GPToVeri
import Graph.Graph
import qualified CriticalPairs.Matches as MT
import Data.List.Utils (countElem)
import Data.Maybe (mapMaybe)

import CriticalPairs.CriticalPairs (CP(..),CriticalPair(..))

{-data CP = FOL | UseDelete | ProduceForbid | Both deriving(Eq,Show)

data CriticalPair a b = CriticalPair {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b,
    cp :: CP
    } deriving (Eq,Show)-}

criticalPairs2 = criticalPairs
countCP2 = countCP

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
criticalPairs l r = criticalPairsAux l r ms
    where
        graphEqClass = GP.genEqClass $ mixLeftRule l r
        ms = map (mountTGMBoth (left l) (left r)) graphEqClass

criticalPairsAux :: GraphRule a b -> GraphRule a b ->
                    [(TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)] ->
                    [CriticalPair a b]
criticalPairsAux l r []           = []
criticalPairsAux l r ((m1,m2):xs) = (if (satsGluingCondBoth (l,m1) (r,m2)) then [newCP] else []) ++ (criticalPairsAux l r xs)
  where
    newCP = CriticalPair m1 m2 (criticalPair l r (m1,m2))

{-Calcula o tipo do conflito de um par epi-}
criticalPair :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> CP
criticalPair l r (m1,m2) = case (useDelete l r (m1,m2), produceForbid l r (m1,m2)) of
                       (True,True)   -> Both
                       (True,False)  -> UseDelete
                       (False,True)  -> ProduceForbid
                       (False,False) -> FOL

useDelete :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
useDelete l r (m1,m2) = True `elem` (nods ++ edgs)
    where
        nods = useDeleteAux l m1 m2 GM.applyNode TGM.nodesDomain TGM.nodesCodomain
        edgs = useDeleteAux l m1 m2 GM.applyEdge TGM.edgesDomain TGM.edgesCodomain

useDeleteAux :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
                     -> (GM.GraphMorphism a b -> t -> Maybe t)
                     -> (TGM.TypedGraphMorphism a b -> [t]) --domain
                     -> (TGM.TypedGraphMorphism a b -> [t]) --codomain
                     -> [Bool]
useDeleteAux l m1 m2 apply dom cod = map (\x -> delByLeft x && isInMatchRight x) (cod m1)
    where
        delByLeft = ruleDeletes l m1 apply dom
        isInMatchRight n = apply (GM.inverse $ TGM.mapping m2) n /= Nothing

produceForbid :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
produceForbid l r (m1,m2) = if Prelude.null (nacs r) then False else exp2
    where
        graphEqClass = map (\x -> GP.genEqClass (mixTGM x (right l))) (nacs r)
        ms = map (map (mountTGM (right l) "Right")) graphEqClass
        ms' = map (filter (satsGluingCond l)) ms
        m2s' = map (map (\x -> RW.dpo x (inverseGR l))) ms'
        matchss = map (map (\x -> MT.matches (M.codomain (left r)) (M.codomain x) MT.FREE)) m2s'
        exp = f3 l r m2s' matchss
        exp2 = True `elem` (concat exp)

f1 l = (\x -> True `elem` (map (satsGluingCond l) x))

f2 :: GraphRule a b -> GraphRule a b -> [TGM.TypedGraphMorphism a b] -> [[TGM.TypedGraphMorphism a b]] -> [Bool]
f2 l r x y =
    do
        a <- x
        b <- y
        c <- b
        return (satsGluingCondBoth (l,a) (r,c))

f3 :: GraphRule a b -> GraphRule a b -> [[TGM.TypedGraphMorphism a b]] -> [[[TGM.TypedGraphMorphism a b]]] -> [[Bool]]
f3 l r x y =
    do
        a <- x
        b <- y
        return (f2 l r a b)

satsGluingCondBoth :: (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      Bool
satsGluingCondBoth (l,m1) (r,m2) = (satsGluingCond l m1) && (satsGluingCond r m2)

satsGluingCond :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCond rule m = identificationCondition && danglingCondition && nacsCondition
    where
        identificationCondition = satsDelItems rule m
        danglingCondition       = satsIncEdges rule m
        nacsCondition           = satsNacs     rule m

{-retorna True se não existirem elementos deletados e preservados ao mesmo tempo pela mesma regra no grafo-}
satsDelItems :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsDelItems rule m = False `notElem` (nodesDelPres ++ edgesDelPres)
    where
        nodesDelPres = map (satsDelItemsAux rule m TGM.nodesDomain GM.applyNode (TGM.nodesDomain m)) (TGM.nodesCodomain m)
        edgesDelPres = map (satsDelItemsAux rule m TGM.edgesDomain GM.applyEdge (TGM.edgesDomain m)) (TGM.edgesCodomain m)

satsDelItemsAux :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b
                         -> (TGM.TypedGraphMorphism a b -> [t])
                         -> (GM.GraphMorphism a b -> t -> Maybe t) 
                         -> [t] -> t -> Bool
satsDelItemsAux rule m dom apply l n = (length incident <= 1) || (not someIsDel)
    where
        incident = [a | a <- dom m, apply (TGM.mapping m) a == (Just n)]
        ruleDel = apply (GM.inverse (TGM.mapping (left rule)))
        someIsDel = Nothing `elem` (map ruleDel incident)

{-retorna True se não existirem dangling edges no grafo-}
satsIncEdges :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsIncEdges r m = False `notElem` (concat incidentEdgesDel)
    where
        mapM = TGM.mapping m
        l = M.domain mapM
        g = M.codomain mapM
        matchedLInG = mapMaybe (GM.applyNode mapM) (nodes l)
        delNodes = filter (ruleDeletes r m GM.applyNode TGM.nodesDomain) matchedLInG
        hasIncEdges = map (incidentEdges g) delNodes
        verEdgeDel = map (ruleDeletes r m GM.applyEdge TGM.edgesDomain)
        incidentEdgesDel = map verEdgeDel hasIncEdges

{-t do tipo NodeId ou EdgeId e deve ser um elemento em G.
  list deve listar os elementos do domínio de m-}
ruleDeletes :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b
                  -> (GM.GraphMorphism a b -> t -> Maybe t)
                  -> (TGM.TypedGraphMorphism a b -> [t])
                  -> t -> Bool
ruleDeletes rule m apply list n = inL && (not isPreserv)
    where
        inL = any (\x -> apply (TGM.mapping m) x == (Just n)) (list m)
        kToG = M.compose (left rule) m
        isPreserv = any (\x -> apply (TGM.mapping kToG) x == (Just n)) (list kToG)

{-retorna True se as NACs não impossibilitam a aplicação das regras-}
satsNacs :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsNacs rule m = False `notElem` (map (satOneNac rule m) (nacs rule))

satOneNac :: GraphRule a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satOneNac rule m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.FREE
      typeNac = M.codomain nac
      typeG   = M.codomain m
