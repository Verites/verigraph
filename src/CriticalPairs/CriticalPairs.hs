{-|
Module      : CriticalPairs.CriticalPairs
Description : CriticalPairs
License     : GPL-3
Maintainer  : acosta@inf.ufrgs.br
Stability   : development

This module provides a structure to calculate all Critical Pairs of two matches.
It considers the DPO approach for graph transformation.
-}
module CriticalPairs.CriticalPairs
 {-(
   CP,
   CriticalPair,
   criticalPairs,
   countCP,
   satsGluingCond,
   satsGluingCondBoth,
   satsNacs,
   createPairs,
   allDeleteUse,
   allProduceForbid,
   allProdEdgeDelNode,
   getM1,
   getM2,
   getCP
   ) -}where

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
import Data.List (elemIndex)
import Data.List.Utils (countElem)
import Data.Maybe (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CP = FOL | DeleteUse | ProduceForbid | ProduceEdgeDeleteNode deriving(Eq,Show)

-- | A Critical Pair is defined as two matches (m1,m2) from the left side of their rules to a same graph.
-- It assumes that the derivation of the rule with match @m1@ causes a conflict with the rule with match @m2@
data CriticalPair a b = CriticalPair {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b,
    cp :: CP
    } deriving (Eq,Show)

-- | Returns the left morphism of a 'CriticalPair'
getM1 :: CriticalPair a b -> TGM.TypedGraphMorphism a b
getM1 = m1

-- | Returns the right morphism of a 'CriticalPair'
getM2 :: CriticalPair a b -> TGM.TypedGraphMorphism a b
getM2 = m2

-- | Returns the type of a 'CriticalPair'
getCP :: CriticalPair a b -> CP
getCP = cp

--instance Show (CriticalPair a b) where
--  show (CriticalPair m1 m2 cp) = "{"++(show $ TGM.mapping m1)++(show $ TGM.mapping m2)++(show cp)++"}"


-- | Return a pair: amount of (DeleteUse, ProduceForbid, ProduceEdgeDeleteNode)
countCP :: GraphRule a b -- ^ left rule
        -> GraphRule a b -- ^ right rule
        -> (Int,Int,Int)
countCP l r = (delUse,proFor,proEdg)
    where
        delUse = countElem DeleteUse             list
        proFor = countElem ProduceForbid         list
        proEdg = countElem ProduceEdgeDeleteNode list
        list   = map getCP (criticalPairs l r)

-- | Create all jointly surjective pairs of @m1@ and @m2@
createPairs :: TGM.TypedGraphMorphism a b
     -> TGM.TypedGraphMorphism a b
     -> [(TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)]
createPairs m1 m2 = map (mountTGMBoth m1 m2) (GP.genEqClass (mixTGM m1 m2))

-- to do: return all critical pairs
namedCriticalPairs :: (String, GraphRule a b) -> (String, GraphRule a b) -> (String,String,[CriticalPair a b])
namedCriticalPairs (name1,l) (name2,r) = (name1,name2,allDeleteUse l r)

-- | All Critical Pairs
criticalPairs :: GraphRule a b -- ^ left rule
              -> GraphRule a b -- ^ right rule
              -> [CriticalPair a b]
criticalPairs l r = (allDeleteUse l r) ++ (allProduceForbid l r) ++ (allProdEdgeDelNode l r)

---- Delete-Use

-- | All DeleteUse caused by the derivation of @l@ before @r@
allDeleteUse :: GraphRule a b -- ^ left rule
             -> GraphRule a b -- ^ right rule
             -> [CriticalPair a b]
allDeleteUse l r = map (\(m1,m2) -> CriticalPair m1 m2 DeleteUse) delUse
    where
        pairs = createPairs (left l) (left r)                                --get all jointly surjective pairs of L1 and L2
        gluing = filter (\(m1,m2) -> satsGluingCondBoth (l,m1) (r,m2)) pairs --filter the pairs that not satisfie gluing conditions of L and R
        delUse = filter (deleteUse l r) gluing                               --select just the pairs that are in DeleteUse conflict

-- | DeleteUse using a most aproximated algorithm of the categorial diagram
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2
deleteUse2 :: GraphRule a b -> GraphRule a b
           -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)
           -> Bool
deleteUse2 l r (m1,m2) = Prelude.null filt
    where
        (_,d2) = RW.poc m2 (left r) --get only the morphism D2 to G
        l1TOd2 = MT.matches (M.domain m1) (M.domain d2) MT.FREE
        filt = filter (\x -> m1 == M.compose x d2) l1TOd2

-- | Rule @l@ causes a delete-use conflict with @r@ if rule @l@ deletes something that is used by @r@
-- @(m1,m2)@ is a pair of matches on the same graph
deleteUse :: GraphRule a b -> GraphRule a b
          -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)
          -> Bool
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
allProduceForbid :: GraphRule a b -- ^ left rule
                 -> GraphRule a b -- ^ right rule
                 -> [CriticalPair a b]
allProduceForbid l r = concat (map (produceForbidOneNac l r) (nacs r))

-- | Check ProduceForbid for a NAC @n@ in @r@
produceForbidOneNac :: GraphRule a b -> GraphRule a b
                    -> TGM.TypedGraphMorphism a b
                    -> [CriticalPair a b]
produceForbidOneNac l r n = let
        inverseLeft = inverseGR l

        -- Consider for a NAC n (L2 -> N2) of r any jointly surjective
        -- pair of morphisms (h1: R1 -> P1, q21: N2 -> P1) with q21 inj
        pairs = createPairs (right l) n
        filtMono = filter (\(_,q) -> M.monomorphism q) pairs --(h1,q21)

        -- Check gluing cond for (h1,r1). Construct PO complement D1.
        filtPairs = filter (\(h1,_) -> satsGluingCond inverseLeft h1) filtMono

        poc = map (\(h1,q21) -> let (k,r') = RW.poc h1 (left inverseLeft) in
                                 (h1,q21,k,r'))
                 filtPairs --(h1,q21,k,r')

        -- Construct PO K and abort if m1 not sats NACs l
        po = map (\(h1,q21,k,r') ->
                   let (m1,l') = RW.po k (right inverseLeft) in
                     (h1,q21,k,r',m1,l'))
                 poc --(h1,q21,k,r',m1,l')

        filtM1 = filter (\(_,_,_,_,m1,_) -> satsNacs l m1) po

        --  Check existence of h21: L2 -> D1 st. e1 . h21 = q21 . n2
        h21 = concat
                 (map (\(h1,q21,k,r',m1,l') ->
                         let hs = MT.matches (M.domain n) (M.codomain k) MT.FREE in
                           if Prelude.null hs
                             then []
                             else [(h1,q21,k,r',m1,l',hs)])
                 filtM1) --(h1,q21,k,r',m1,l1,[hs])

        validH21 = concat $ map (\(h1,q21,k,r',m1,l',hs) ->
                              let list = map (\h -> M.compose h r' == M.compose n q21) hs in
                                case (elemIndex True list) of
                                  Just ind -> [(h1,q21,k,r',m1,l',hs!!ind)]
                                  Nothing -> [])
                            h21

        -- Define m2 = d1 . h21: L2 -> K and abort if m2 not sats NACs r
        m1m2 = map (\(h1,q21,k,r',m1,l',l2d1) -> (m1, M.compose l2d1 l')) validH21
        --filtM2 = filter (\(m1,m2) -> satsNacs r m2) m1m2

        -- Check gluing condition for m2 and r
        filtM2 = filter (\(m1,m2) -> satsGluingCond r m2) m1m2

        in map (\(m1,m2) -> CriticalPair m1 m2 ProduceForbid) filtM2

---- Produce Edge Delete Node

allProdEdgeDelNode :: GraphRule a b -- ^ left rule
                   -> GraphRule a b -- ^ right rule
                   -> [CriticalPair a b]
allProdEdgeDelNode l r = map (\(m1,m2) -> CriticalPair m1 m2 ProduceEdgeDeleteNode) conflictPairs
    where
        pairs = createPairs (left l) (left r)
        filteredPairs = filter (\(m1,m2) -> satsGluingCondBoth (l,m1) (r,m2)) pairs
        conflictPairs = filter (prodEdgeDelNode l r) filteredPairs

prodEdgeDelNode :: GraphRule a b -> GraphRule a b -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b) -> Bool
prodEdgeDelNode l r (m1,m2) = not (satsIncEdges r lp)
    where
        (_,p,l',r') = RW.dpo m1 l
        gd = TGM.invertTGM l'
        ld = M.compose m2 gd
        lp = M.compose ld r'

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
satsOneNac2 :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
satsOneNac2 m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.INJ
      typeNac = M.codomain nac
      typeG   = M.codomain m

-- | Return True if the NAC @nac@ is satified by @m@
-- Check for all partial injective matches from @nac@ to G
-- The Nac matches (N -> G) are injective only out of image of the rule match (L -> G)
satsOneNac :: TGM.TypedGraphMorphism a b -- ^ m
           -> TGM.TypedGraphMorphism a b -- ^ nac
           -> Bool
satsOneNac m nac = True `notElem` checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches_nac nac m
