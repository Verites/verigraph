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

{-Calcula o tipo do conflito de um par epi
criticalPair :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> CP
criticalPair l r (m1,m2) = case (useDelete l r (m1,m2), produceForbid l r (m1,m2)) of
                       (True,True)   -> Both
                       (True,False)  -> UseDelete
                       (False,True)  -> ProduceForbid
                       (False,False) -> FOL-}

allUseDelete :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allUseDelete l r = allUseDeleteAux l r ms
    where
        graphEqClass = GP.genEqClass $ mixLeftRule l r
        ms = map (mountTGMBoth (left l) (left r)) graphEqClass

allUseDeleteAux :: GraphRule a b -> GraphRule a b
                -> [(TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)]
                -> [CriticalPair a b]
allUseDeleteAux l r []           = []
allUseDeleteAux l r ((m1,m2):xs) = (if isUseDelete l r m1 m2 then [newCP] else []) ++ (allUseDeleteAux l r xs)
    where
        isUseDelete l r m1 m2 = satsGluingCondBoth (l,m1) (r,m2) && useDelete l r (m1,m2)
        newCP = CriticalPair m1 m2 UseDelete

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
adjForAGG = True

allProduceForbid :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allProduceForbid l r =
    if adjForAGG
        then concat (map (produceForbidOneNac r l) (nacs r))
        else concat (map (produceForbidOneNac l r) (nacs l))

produceForbidOneNac :: GraphRule a b -> GraphRule a b
                    -> TGM.TypedGraphMorphism a b
                    -> [CriticalPair a b]
produceForbidOneNac l r n = concat (concat filtMatches)
    where
        pairs = createPairs r n
        filtPairs = filter (\x -> satsGluingCond (inverseGR r) (snd x)) pairs
        m2 = map (\(_,x) -> RW.dpo x (inverseGR r)) filtPairs
        filtM2 = filter (satsNacs r) m2 --testar nacs testeCreate
        --Check existence of h12 : L1 -> D2 s.t. e2 . h12 = q12 . n1 - If not existent, then abort
        m1 = map (\x -> MT.matches (M.codomain (left l)) (M.codomain x) MT.FREE) filtM2
        filtM1 = map (filter (satsNacs l)) m1 --testar nacs sendMsg
        c x1 x2 = if satsGluingCond x1 x2 then [CriticalPair n n ProduceForbid] else [] --nao sei com quais morfismos criar o CP
        filtMatches = map (map (c l)) filtM1

createPairs :: GraphRule a b
     -> TGM.TypedGraphMorphism a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
createPairs r nac = map (mountTGMBoth nac (right r)) g
    where
        g = GP.genEqClass (mixTGM nac (right r))

{-allProduceForbid :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allProduceForbid l r = if Prelude.null (nacs r) then [] else allProduceForbidAux l r pairs
    where
        pairs = concat (map (createPairs l) (nacs r))

createPairs :: GraphRule a b
     -> TGM.TypedGraphMorphism a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
createPairs l r = map (\x -> mountTGMBoth (right l) r x) g
    where
        g = GP.genEqClass (mixTGM (right l) r)

allProduceForbidAux :: GraphRule a b -> GraphRule a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
     -> [CriticalPair a b]
allProduceForbidAux l r []          = []
allProduceForbidAux l r ((ms,_):xs) = isProduceForbid ++ (allProduceForbidAux l r xs)
    where
        gluing = satsGluingCond (inverseGR l) ms
        isProduceForbid = if gluing then produceForbid l r ms else []

produceForbid :: GraphRule a b -> GraphRule a b
     -> TGM.TypedGraphMorphism a b
     -> [CriticalPair a b]
produceForbid l r m' = newCP
    where
        m = RW.dpo m' (inverseGR l)
        matches = MT.matches (M.codomain (left r)) (M.codomain m) MT.FREE
        filtered = filter (\x -> satsGluingCondBoth (l,m) (r,x)) matches
        newCP = map (\x -> CriticalPair m x ProduceForbid) filtered-}

------------------------------------------------------------------------

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
