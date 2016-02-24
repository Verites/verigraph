module CriticalPairs.CriticalPairs
{- (
   CP (..),
   CriticalPair (..),
   criticalPairs,
   countCP,
   satsGluingCondBoth,
   satsGluingCond,
   f1,
   f2,
   f3
   )
    -}where

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
        list   = map cp (criticalPairs l r)

{-Cálculo dos Pares Críticos-}
criticalPairs :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
criticalPairs l r = (allUseDelete l r) ++ (allProduceForbid l r)

allUseDelete :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allUseDelete l r = map (\(m1,m2) -> CriticalPair m1 m2 UseDelete) useDel
    where
        pairs = createPairs (left l) (left r)
        gluing = filter (\(m1,m2) -> satsGluingCondBoth (l,m1) (r,m2)) pairs
        useDel = filter (useDelete l r) gluing

{-UseDelete através dos diagramas, falta que RW.poc retorne o morfismo d2
useDelete :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
useDelete l r (m1,m2) = Prelude.null filt
    where
        d2 = RW.poc m2 (left r)
        l1TOd2 = MT.matches (M.codomain (left l)) (M.domain d2) MT.FREE
        filt = filter (\x -> m1 == M.compose x d2) l1TOd2-}

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

------------------------------------------------------------------------
adjForAGG = True --Verificar o sentido

allProduceForbid :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allProduceForbid l r =
    if adjForAGG
        then concat (map (produceForbidOneNac r l) (nacs r))
        else concat (map (produceForbidOneNac l r) (nacs l))

produceForbidOneNac :: GraphRule a b -> GraphRule a b
                    -> TGM.TypedGraphMorphism a b
                    -> [CriticalPair a b]
produceForbidOneNac l r n = map (\(m1,m2) -> CriticalPair m1 m2 ProduceForbid) filtM1
    where
        pairs = createPairs n (right r)
        inverseRule = inverseGR r
        filtPairs = filter (\(_,x) -> satsGluingCond inverseRule x) pairs
        m2 = map (\(_,x) -> RW.dpo x inverseRule) filtPairs
        filtM2 = filter (satsNacs r) m2
        --Check existence of h12 : L1 -> D2 s.t. e2 . h12 = q12 . n1 - If not existent, then abort
        --código abaixo é provisório
        m1 = concat (map (\x -> createM1 (MT.matches (M.codomain (left l)) (M.codomain x) MT.FREE) x) filtM2)--pares (m1,m2)
        filtM1 = filter (\(x,y) -> satsGluingCond l x) m1

createM1 matchs m2 = map (\x -> (x,m2)) matchs

createPairs :: TGM.TypedGraphMorphism a b
     -> TGM.TypedGraphMorphism a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
createPairs l r = map (mountTGMBoth l r) g
    where
        g = GP.genEqClass (mixTGM l r)

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
satsNacs rule m = False `notElem` (map (satsOneNac m) (nacs rule))

satsOneNac :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satsOneNac m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.INJ
      typeNac = M.codomain nac
      typeG   = M.codomain m
