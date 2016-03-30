module Analysis.GluingConditions
 (
   satsGluingCond,
   satsGluingCondBoth,
   satsGluingCondWithoutNac,
   satsNacs,
   ruleDeletes,
   satsIncEdges
 ) where

import qualified Abstract.Morphism as M
import qualified Analysis.Matches as MT
import           Data.Maybe (mapMaybe)
import           Graph.Graph (nodes,incidentEdges)
import qualified Graph.GraphMorphism as GM
import           Graph.GraphRule
import           Graph.TypedGraphMorphism
import qualified Graph.Rewriting as RW

---- Gluing Conditions

-- | Check gluing conditions for a pair of matches
satsGluingCondBoth :: Bool ->
                      (GraphRule a b, TypedGraphMorphism a b) ->
                      (GraphRule a b, TypedGraphMorphism a b) ->
                      Bool
satsGluingCondBoth nacInj (l,m1) (r,m2) = (satsGluingCond nacInj l m1) && (satsGluingCond nacInj r m2)

-- | Check gluing conditions for a match
satsGluingCond :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> Bool
satsGluingCond nacInj rule m = identificationCondition && danglingCondition && nacsCondition
    where
        identificationCondition = satsDelItems        rule m
        danglingCondition       = satsIncEdges        rule m
        nacsCondition           = satsNacs     nacInj rule m

satsGluingCondWithoutNac :: GraphRule a b -> TypedGraphMorphism a b -> Bool
satsGluingCondWithoutNac rule m = identificationCondition && danglingCondition
    where
        identificationCondition = satsDelItems rule m
        danglingCondition       = satsIncEdges rule m

-- | Return True if the match @m@ satifies the identification condition
satsDelItems :: GraphRule a b -> TypedGraphMorphism a b -> Bool
satsDelItems rule m = all (==True) (nodesDelPres ++ edgesDelPres)
    where
        nodesDelPres = map (satsDelItemsAux rule m nodesDomain GM.applyNode (nodesDomain m)) (nodesCodomain m)
        edgesDelPres = map (satsDelItemsAux rule m edgesDomain GM.applyEdge (edgesDomain m)) (edgesCodomain m)

-- | Check if in the match @m@, a element @n@ is deleted and at same time have another incident element on himself
satsDelItemsAux :: Eq t => GraphRule a b -> TypedGraphMorphism a b
                         -> (TypedGraphMorphism a b -> [t])
                         -> (GM.GraphMorphism a b -> t -> Maybe t)
                         -> [t] -> t -> Bool
-- if just one element is incident in @n@, so it is not deleted and preserved at same match
-- otherwise, is needed to verify if in the list of incident elements, if some is deleting @n@
-- if two or more incident elements delete the element @n@ return False
satsDelItemsAux rule m dom apply l n = (length incident <= 1) || (not someIsDel)
    where
        incident = [a | a <- dom m, apply (mapping m) a == (Just n)]
        ruleDel = apply (GM.inverse (mapping (left rule)))
        someIsDel = any (==Nothing) (map ruleDel incident)

-- | Return True if do not exist dangling edges by the derivation of @r@ with match @m@
satsIncEdges :: GraphRule a b -> TypedGraphMorphism a b -> Bool
satsIncEdges r m = all (==True) (concat incidentEdgesDel)
    where
        l = graphDomain m
        g = graphCodomain m
        matchedLInG = mapMaybe (applyNodeTGM m) (nodes l)
        delNodes = filter (ruleDeletes r m GM.applyNode nodesDomain) matchedLInG
        hasIncEdges = map (incidentEdges g) delNodes
        verEdgeDel = map (ruleDeletes r m GM.applyEdge edgesDomain)
        incidentEdgesDel = map verEdgeDel hasIncEdges

-- | Return True if the element @n@ is deleted by the rule @rule@ with match @m@
-- assumes that @n@ has type NodeId or EdgeId
-- @n@ not is necessarily element of G (the graph matched by @m@), in this case return False
-- @list@ must get all element in the domain of @m@
ruleDeletes :: Eq t => GraphRule a b -> TypedGraphMorphism a b
                  -> (GM.GraphMorphism a b -> t -> Maybe t)
                  -> (TypedGraphMorphism a b -> [t])
                  -> t -> Bool
ruleDeletes rule m apply list n = inL && (not isPreserv)
    where
        inL = any (\x -> apply (mapping m) x == (Just n)) (list m)
        kToG = M.compose (left rule) m
        isPreserv = any (\x -> apply (mapping kToG) x == (Just n)) (list kToG)

-- | Return True if all NACs of @rule@ are satified by @m@
satsNacs :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> Bool
satsNacs nacInj rule m = all (==True) (map (satsFun m) (nacs rule))
  where
    satsFun = if nacInj then satsOneNacInj else satsOneNacPartInj

-- | Return True if the NAC @nac@ is satified by @m@
-- Get all injective matches (q) from @nac@ to G (codomain of @m@)
-- and check if some of them commutes: @m@ == q . @nac@
satsOneNacInj :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satsOneNacInj m nac = all (==False) checkCompose
   where
      checkCompose = map (\x -> (M.compose nac x) == m) matches
      matches = MT.matches typeNac typeG MT.INJ
      typeNac = M.codomain nac
      typeG   = M.codomain m

-- | Return True if the NAC @nac@ is satified by @m@
-- Check for all partial injective matches from @nac@ to G
-- The Nac matches (N -> G) are injective only out of image of the rule match (L -> G)
satsOneNacPartInj :: TypedGraphMorphism a b -- ^ m
           -> TypedGraphMorphism a b -- ^ nac
           -> Bool
satsOneNacPartInj m nac = all (==False) check
   where
      check = map (partialInjectiveTGM nac) checkCompose
      checkCompose = filter (\x -> (M.compose nac x) == m) matches
      matches = MT.partInjMatches nac m --generating some non partial injective matches
