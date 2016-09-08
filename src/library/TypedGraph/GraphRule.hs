{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedGraph.GraphRule (
    -- * Types
      GraphRule
    , getLHS
    , getRHS
    , getNACs

    -- * Basic Functions
    , invertProductionWithoutNacs
    , deletedNodes
    , deletedEdges
    , createdNodes
    , createdEdges

    , checkDeletion
) where


import           Abstract.DPO        as DPO
import           Abstract.Morphism
import           Graph.Graph         as G
import           TypedGraph.Morphism as TGM

type GraphRule a b = Production (TypedGraphMorphism a b)

-- | Return the nodes deleted by a rule
deletedNodes :: GraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanNodesTyped (getLHS r)

-- | Return the nodes created by a rule
createdNodes :: GraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanNodesTyped (getRHS r)

-- | Return the edges deleted by a rule
deletedEdges :: GraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanEdgesTyped (getLHS r)

-- | Return the edges created by a rule
createdEdges :: GraphRule a b -> [G.EdgeId]
createdEdges r = TGM.orphanEdgesTyped (getRHS r)

instance DPO (TypedGraphMorphism a b) where

  invertProduction conf rule =
    buildProduction (getRHS rule) (getLHS rule) (concatMap (shiftNacOverProduction conf rule) (getNACs rule))

  shiftNacOverProduction conf rule nac = [calculateComatch nac rule | satisfiesGluingConditions conf rule nac]

  isPartiallyMonomorphic = partialInjectiveTGM


-- | Given the left-hand-side morphism of a rule /l : K -> L/, a match /m : L -> G/ for this rule, an element __/e/__
-- (that can be either a __/Node/__ or an __/Edge/__) and two functions /apply/ (for applying that element in a TypedGraphMorphism) and
-- /list/ (to get all the corresponding elements in the domain of m), it returns true if /e/ is deleted by this rule for the given match
checkDeletion :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t]) -> t -> Bool
checkDeletion l m apply list e = elementInL && not elementInK
  where
    elementInL = any (\x -> apply m x == Just e) (list m)
    kToG = compose l m
    elementInK = any (\x -> apply kToG x == Just e) (list kToG)
