{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedGraph.GraphRule (
    -- * Types
      GraphRule
    , left
    , right
    , nacs

    -- * Basic Functions
    , invertProductionWithoutNacs
    , deletedNodes
    , deletedEdges
    , createdNodes
    , createdEdges

    , ruleDeletes
) where


import           Abstract.DPO             as DPO
import           Abstract.Morphism
import           Graph.Graph              as G
import           TypedGraph.Morphism      as TGM

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
  invertProduction config r = production (right r) (left r) (concatMap (shiftLeftNac config r) (nacs r))

  shiftLeftNac config rule n = [calculateComatch n rule | satisfiesGluingConditions config rule n]

  partiallyMonomorphic = partialInjectiveTGM


-- | Return True if the element @n@ is deleted by the rule @rule@ with match @m@
-- assumes that @n@ has type NodeId or EdgeId
-- @n@ not is necessarily element of G (the graph matched by @m@), in this case return False
-- @list@ must get all element in the domain of @m@
--
-- FIXME: documentation doesn't match implementation
ruleDeletes :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b
          -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t])
          -> t -> Bool
ruleDeletes l m apply list n =
  inL && not isPreserv

  where
    inL = any (\x -> apply m x == Just n) (list m)
    kToG = compose l m
    isPreserv = any (\x -> apply kToG x == Just n) (list kToG)
