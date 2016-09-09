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
import           Graph.Graph         as G
import           TypedGraph.Morphism as TGM

type GraphRule a b = Production (TypedGraphMorphism a b)

-- | Return the nodes deleted by a rule
deletedNodes :: GraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanTypedNodes (getLHS r)

-- | Return the nodes created by a rule
createdNodes :: GraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanTypedNodes (getRHS r)

-- | Return the edges deleted by a rule
deletedEdges :: GraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanTypedEdges (getLHS r)

-- | Return the edges created by a rule
createdEdges :: GraphRule a b -> [G.EdgeId]
createdEdges r = TGM.orphanTypedEdges (getRHS r)

instance DPO (TypedGraphMorphism a b) where

  invertProduction conf rule =
    buildProduction (getRHS rule) (getLHS rule) (concatMap (shiftNacOverProduction conf rule) (getNACs rule))

  shiftNacOverProduction conf rule nac = [calculateComatch nac rule | satisfiesGluingConditions conf rule nac]

  isPartiallyMonomorphic = isPartialInjective
