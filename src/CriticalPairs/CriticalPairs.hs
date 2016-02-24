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
import qualified Abstract.Morphism as M
import CriticalPairs.VeriToGP
import CriticalPairs.GPToVeri
import Graph.Graph
import qualified CriticalPairs.Matches as MT
import Data.List.Utils (countElem)
import Data.Maybe (mapMaybe)

data CP = FOL | DeleteUse | ProduceForbid | Both deriving(Eq,Show)

-- | A Critical Pair is defined as two matches from the left side of their rules to a same graph
-- CP indicates the type of the Critical Pair
-- It assumes that the derivation of the rule with match @m1@ causes a conflict with the rule with match @m2@
data CriticalPair a b = CriticalPair {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b,
    cp :: CP
    } deriving (Eq,Show)

--instance Show (CriticalPair a b) where
--  show (CriticalPair m1 m2 cp) = "{"++(show $ TGM.mapping m1)++(show $ TGM.mapping m2)++(show cp)++"}"

-- | Return a pair: (amount of DeleteUse, amount of ProduceForbid)
countCP :: GraphRule a b -> GraphRule a b -> (Int,Int)
countCP l r = (delUse+both,proFor+both)
    where
        delUse = countElem DeleteUse     list
        proFor = countElem ProduceForbid list
        both   = countElem Both          list
        list   = map cp (criticalPairs l r)

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairs :: TGM.TypedGraphMorphism a b
     -> TGM.TypedGraphMorphism a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
createPairs m1 m2 = map (mountTGMBoth m1 m2) (GP.genEqClass (mixTGM m1 m2))

-- | All Critical Pairs
criticalPairs :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
criticalPairs l r = (allDeleteUse l r) ++ (allProduceForbid l r)

---- Delete-Use

-- | All DeleteUse caused by the derivation of @l@ before @r@
allDeleteUse :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allDeleteUse l r = map (\(m1,m2) -> CriticalPair m1 m2 DeleteUse) delUse
    where
        pairs = createPairs (left l) (left r)                                --get all jointly surjective pairs of L1 and L2
        gluing = filter (\(m1,m2) -> satsGluingCondBoth (l,m1) (r,m2)) pairs --filter the pairs that not satisfie gluing conditions of L and R
        delUse = filter (deleteUse l r) gluing                               --select just the pairs that are in DeleteUse conflict

-- | DeleteUse using a most aproximated algorithm of the categorial diagram
-- missing that RW.poc returns the morsphism d2
{-deleteUse :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
deleteUse l r (m1,m2) = Prelude.null filt
    where
        d2 = RW.poc m2 (left r)
        l1TOd2 = MT.matches (M.codomain (left l)) (M.domain d2) MT.FREE
        filt = filter (\x -> m1 == M.compose x d2) l1TOd2-}

-- | Rule @l@ causes a delete-use conflict with @r@ if rule @l@ deletes something that is used by @r@
-- @(m1,m2)@ is a pair of matches on the same graph
deleteUse :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
deleteUse l r (m1,m2) = True `elem` (nods ++ edgs)
    where
        nods = deleteUseAux l m1 m2 GM.applyNode TGM.nodesDomain TGM.nodesCodomain
        edgs = deleteUseAux l m1 m2 GM.applyEdge TGM.edgesDomain TGM.edgesCodomain

-- | Verify if some element in a graph is deleted by @l@ and is in the match of @r@
deleteUseAux :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
                     -> (GM.GraphMorphism a b -> t -> Maybe t)
                     -> (TGM.TypedGraphMorphism a b -> [t]) --domain
                     -> (TGM.TypedGraphMorphism a b -> [t]) --codomain
                     -> [Bool]
deleteUseAux l m1 m2 apply dom cod = map (\x -> delByLeft x && isInMatchRight x) (cod m1)
    where
        delByLeft = ruleDeletes l m1 apply dom
        isInMatchRight n = apply (GM.inverse $ TGM.mapping m2) n /= Nothing

---- Produce-Forbid

-- | All ProduceForbid caused by the derivation of @l@ before @r@
-- rule @l@ causes a produce-forbid conflict with @r@ if some NAC in @r@ fails to be satisfied after the aplication of @l@
allProduceForbid :: GraphRule a b -> GraphRule a b -> [CriticalPair a b]
allProduceForbid l r = concat (map (produceForbidOneNac l r) (nacs r))

-- | Check ProduceForbid for a NAC @n@ in @r@
produceForbidOneNac :: GraphRule a b -> GraphRule a b
                    -> TGM.TypedGraphMorphism a b
                    -> [CriticalPair a b]
produceForbidOneNac l r n = map (\(m1,m2) -> CriticalPair m1 m2 ProduceForbid) filtM2
    where
        pairs = createPairs (right l) n
        inverseRule = inverseGR l
        filtPairs = filter (\(m'1,_) -> satsGluingCond inverseRule m'1) pairs
        m1 = map (\(m'1,_) -> RW.dpo m'1 inverseRule) filtPairs
        filtM1 = filter (satsNacs l) m1
        --Check existence of h12 : L1 -> D2 s.t. e2 . h12 = q12 . n1 - If not existent, then abort
        --código abaixo é provisório
        m2 = concat (map (\x -> createM2 x (MT.matches (M.codomain (left r)) (M.codomain x) MT.FREE)) filtM1)--pairs (m1,m2)
        filtM2 = filter (\(x,y) -> satsGluingCond r y) m2

createM2 m1 matchs = map (\x -> (m1,x)) matchs

---- Gluing Conditions

-- | Check gluing conditions for a pair of matches
satsGluingCondBoth :: (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      (GraphRule a b, TGM.TypedGraphMorphism a b) ->
                      Bool
satsGluingCondBoth (l,m1) (r,m2) = (satsGluingCond l m1) && (satsGluingCond r m2)

-- | Check gluing conditions for a match
satsGluingCond :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCond rule m = identificationCondition && danglingCondition && nacsCondition
    where
        identificationCondition = satsDelItems rule m
        danglingCondition       = satsIncEdges rule m
        nacsCondition           = satsNacs     rule m

-- | Return True if the match @m@ satifies the identification condition
satsDelItems :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsDelItems rule m = False `notElem` (nodesDelPres ++ edgesDelPres)
    where
        nodesDelPres = map (satsDelItemsAux rule m TGM.nodesDomain GM.applyNode (TGM.nodesDomain m)) (TGM.nodesCodomain m)
        edgesDelPres = map (satsDelItemsAux rule m TGM.edgesDomain GM.applyEdge (TGM.edgesDomain m)) (TGM.edgesCodomain m)

-- | Check if in the match @m@, a element @n@ is deleted and at same time have another incident element on himself
satsDelItemsAux :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b
                         -> (TGM.TypedGraphMorphism a b -> [t])
                         -> (GM.GraphMorphism a b -> t -> Maybe t)
                         -> [t] -> t -> Bool
-- if just one element is incident in @n@, so it is not deleted and preserved at same match
-- otherwise, is needed to verify if in the list of incident elements, if some is deleting @n@
-- if two or more incident elements delete the element @n@ return False
satsDelItemsAux rule m dom apply l n = (length incident <= 1) || (not someIsDel)
    where
        incident = [a | a <- dom m, apply (TGM.mapping m) a == (Just n)] 
        ruleDel = apply (GM.inverse (TGM.mapping (left rule)))
        someIsDel = Nothing `elem` (map ruleDel incident)

-- | Return True if do not exist dangling edges by the derivation of @r@ with match @m@
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

-- | Return True if the element @n@ is deleted by the rule @rule@ with match @m@
-- assumes that @n@ has type NodeId or EdgeId
-- @n@ not is necessarily element of G (the graph matched by @m@), in this case return False
-- @list@ must get all element in the domain of @m@
ruleDeletes :: Eq t => GraphRule a b -> TGM.TypedGraphMorphism a b
                  -> (GM.GraphMorphism a b -> t -> Maybe t)
                  -> (TGM.TypedGraphMorphism a b -> [t])
                  -> t -> Bool
ruleDeletes rule m apply list n = inL && (not isPreserv)
    where
        inL = any (\x -> apply (TGM.mapping m) x == (Just n)) (list m)
        kToG = M.compose (left rule) m
        isPreserv = any (\x -> apply (TGM.mapping kToG) x == (Just n)) (list kToG)

-- | Return True if all NACs of @rule@ are satified by @m@
satsNacs :: GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsNacs rule m = False `notElem` (map (satsOneNac m) (nacs rule))

-- | Return True if the NAC @nac@ is satified by @m@
-- Get all injective matches (q) from @nac@ to G (codomain of @m@)
-- and check if some of them commutes: @m@ == q . @nac@
satsOneNac :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satsOneNac m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.INJ
      typeNac = M.codomain nac
      typeG   = M.codomain m
