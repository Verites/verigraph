{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedGraph.GraphRule (
    -- * Types
      GraphRule
    -- * Basic Functions
    , inverseWithoutNacs
    , deletedNodes
    , deletedEdges
    , createdNodes
    , createdEdges

    , ruleDeletes

    -- * Gluing condition
    , satsDelItems
    , satsIncEdges
) where

import           Data.Maybe               (mapMaybe)
import           Abstract.Morphism
import           Abstract.DPO
import           Graph.Graph              as G
import           TypedGraph.Morphism as TGM
import           TypedGraph.FindMorphism       ()
--import           TypedGraph.EpiPairs           ()
-- TODO: resolve cyclic dependency

type GraphRule a b = Production (TypedGraphMorphism a b)

-- | Return the nodes deleted by a rule
deletedNodes :: GraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanNodesTyped (left r)

-- | Return the nodes created by a rule
createdNodes :: GraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanNodesTyped (right r)

-- | Return the edges deleted by a rule
deletedEdges :: GraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanEdgesTyped (left r)

-- | Return the edges created by a rule
createdEdges :: GraphRule a b -> [G.EdgeId]
createdEdges r = TGM.orphanEdgesTyped (right r)

instance DPO (TypedGraphMorphism a b) where
  satsGluing inj left m = (inj == MonoMatches || identificationCondition) && danglingCondition
    where
        identificationCondition = satsDelItems left m
        danglingCondition       = satsIncEdges left m

  inverse nacInj inj r = production (right r) (left r) (concatMap (shiftLeftNac nacInj inj r) (nacs r))

  shiftLeftNac _ inj rule n = [comatch n rule | satsGluing inj (left rule) n]

  partiallyMonomorphic = partialInjectiveTGM

---- Gluing Conditions

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
        kToG = compose l m
        isPreserv = any (\x -> apply kToG x == Just n) (list kToG)
