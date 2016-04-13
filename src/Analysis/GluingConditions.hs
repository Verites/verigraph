module Analysis.GluingConditions
 ( satsGluingAndNacs,
   satsGluingNacsBoth,
   satsGluing,
   satsNacs,
   ruleDeletes,
   satsDelItems,
   satsIncEdges
 ) where

import Abstract.AdhesiveHLR
import Abstract.DPO

import qualified Abstract.Morphism        as M
import qualified Analysis.Matches         as MT
import           Data.Maybe               (mapMaybe)
import           Graph.Graph              (incidentEdges, nodes)
import qualified Graph.GraphMorphism      as GM
import           Graph.GraphRule
import           Graph.TypedGraphMorphism


instance DPO (TypedGraphMorphism a b) where
  satsGluing inj m prod = (inj || identificationCondition) && danglingCondition
    where
        identificationCondition = satsDelItems (left prod) m
        danglingCondition       = satsIncEdges (left prod) m


---- Gluing Conditions

-- | Check gluing conditions and the NACs satisfability for a pair of matches
-- @inj@ only indicates the match, this function does not checks if the pair is injective
satsGluingNacsBoth :: Bool -> Bool ->
                      (GraphRule a b, TypedGraphMorphism a b) ->
                      (GraphRule a b, TypedGraphMorphism a b) ->
                      Bool
satsGluingNacsBoth nacInj inj (l,m1) (r,m2) =
  satsGluingAndNacs nacInj inj l m1 && satsGluingAndNacs nacInj inj r m2

-- | Check gluing conditions and the NACs satisfability for a match
-- @inj@ only indicates the match, this function does not checks if the match is injective
satsGluingAndNacs :: Bool -> Bool -> GraphRule a b -> TypedGraphMorphism a b -> Bool
satsGluingAndNacs nacInj inj rule m = gluingCond && nacsCondition
    where
        gluingCond    = satsGluing inj m rule
        nacsCondition = satsNacs nacInj rule m

-- | Return True if the match @m@ satifies the identification condition
satsDelItems :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satsDelItems l m = all (==True) (nodesDelPres ++ edgesDelPres)
    where
        nodesDelPres = map (satsDelItemsAux l m nodesDomain applyNodeTGM) (nodesCodomain m)
        edgesDelPres = map (satsDelItemsAux l m edgesDomain applyEdgeTGM) (edgesCodomain m)

-- | Check if in the match @m@, a element @n@ is deleted and at same time have another incident element on himself
satsDelItemsAux :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b
                         -> (TypedGraphMorphism a b -> [t])
                         -> (TypedGraphMorphism a b -> t -> Maybe t)
                         -> t -> Bool
-- if just one element is incident in @n@, so it is not deleted and preserved at same match
-- otherwise, is needed to verify if in the list of incident elements, if some is deleting @n@
-- if two or more incident elements delete the element @n@ return False
satsDelItemsAux l m dom apply n = (length incident <= 1) || not someIsDel
    where
        incident = [a | a <- dom m, apply m a == Just n]
        ruleDel = apply (invertTGM l)
        someIsDel = any (==Nothing) (map ruleDel incident)

-- | Return True if do not exist dangling edges by the derivation of @r@ with match @m@
satsIncEdges :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satsIncEdges leftR m = all (==True) (concat incidentEdgesDel)
    where
        l = graphDomain m
        g = graphCodomain m
        matchedLInG = mapMaybe (applyNodeTGM m) (nodes l)
        delNodes = filter (ruleDeletes leftR m applyNodeTGM nodesDomain) matchedLInG
        hasIncEdges = map (incidentEdges g) delNodes
        verEdgeDel = map (ruleDeletes leftR m applyEdgeTGM edgesDomain)
        incidentEdgesDel = map verEdgeDel hasIncEdges

-- | Return True if the element @n@ is deleted by the rule @rule@ with match @m@
-- assumes that @n@ has type NodeId or EdgeId
-- @n@ not is necessarily element of G (the graph matched by @m@), in this case return False
-- @list@ must get all element in the domain of @m@
ruleDeletes :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b
                  -> (TypedGraphMorphism a b -> t -> Maybe t)
                  -> (TypedGraphMorphism a b -> [t])
                  -> t -> Bool
ruleDeletes l m apply list n = inL && not isPreserv
    where
        inL = any (\x -> apply m x == Just n) (list m)
        kToG = M.compose l m
        isPreserv = any (\x -> apply kToG x == Just n) (list kToG)

-- | Return True if all NACs of @rule@ are satified by @m@
satsNacs :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> Bool
satsNacs nacInj rule m = all (==True) (map (satsFun m) (nacs rule))
  where
    satsFun = if nacInj then satsOneNacInj else satsOneNacPartInj

-- | Return True if the NAC @nac@ is satified by @m@
-- Get all injective matches (q) from @nac@ to G (codomain of @m@)
-- and check if some of them commutes: @m@ == q . @nac@
satsOneNacInj :: TypedGraphMorphism a b -- ^ m
              -> TypedGraphMorphism a b -- ^ nac
              -> Bool
satsOneNacInj m nac = all (==False) checkCompose
   where
      checkCompose = map (\x -> M.compose nac x == m) matches
      matches = MT.matches MT.MONO typeNac typeG
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
      checkCompose = filter (\x -> M.compose nac x == m) matches
      matches = MT.partInjMatches nac m --generating some non partial injective matches
