{-# LANGUAGE FlexibleInstances #-}

module TypedGraph.DPO.GraphRule (
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

    , emptyGraphRule
    , nullGraphRule

    , checkDeletion
) where


import           Abstract.DPO        as DPO
import           Abstract.Morphism   as M
import           Graph.Graph         as G
import qualified Graph.GraphMorphism as GM
import           TypedGraph.Graph    as GM
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

-- | Returns a empty GraphRule
emptyGraphRule :: Graph a b -> Production (TypedGraphMorphism a b)
emptyGraphRule typegraph = emptyRule
  where
    emptyGraph = empty
    emptyGM = GM.empty emptyGraph typegraph
    emptyTGM = idMap emptyGM emptyGM
    emptyRule = buildProduction emptyTGM emptyTGM []

-- | Checks if is a null rule
nullGraphRule :: GraphRule a b -> Bool
nullGraphRule rule = null l && null k && null r
  where
    null = G.null . untypedGraph
    l = codomain $ getLHS rule
    k = domain $ getLHS rule
    r = codomain $ getRHS rule

instance DPO (TypedGraphMorphism a b) where

  invertProduction conf rule =
    buildProduction (getRHS rule) (getLHS rule) (concatMap (shiftNacOverProduction conf rule) (getNACs rule))

  shiftNacOverProduction conf rule nac = [calculateComatch nac rule | satisfiesGluingConditions conf rule nac]

  isPartiallyMonomorphic = isPartialInjective
