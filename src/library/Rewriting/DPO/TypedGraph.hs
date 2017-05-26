{-# LANGUAGE FlexibleInstances #-}

module Rewriting.DPO.TypedGraph
(-- * Types
  TypedGraphRule
, NamedTypedGraphRule
, getLHS
, getRHS
, getNACs

-- * Basic Functions
, invertProductionWithoutNacs
, deletedNodes
, deletedEdges
, createdNodes
, createdEdges
, preservedNodes
, preservedEdges
, emptyGraphRule
, nullGraphRule
, isDeleted
) where

import           Abstract.Category.FinitaryCategory as FC
import           Abstract.Rewriting.DPO             as DPO
import           Category.TypedGraph                ()
import           Category.TypedGraph.AdhesiveHLR
import           Category.TypedGraph.FindMorphism   ()
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph                    as TG
import           Data.TypedGraph.Morphism           as TGM

type TypedGraphRule a b = Production (TypedGraphMorphism a b)
type NamedTypedGraphRule a b = NamedProduction (TypedGraphMorphism a b)

-- | Return the nodes deleted by a rule
deletedNodes :: TypedGraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanTypedNodeIds (getLHS r)

-- | Return the nodes created by a rule
createdNodes :: TypedGraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanTypedNodeIds (getRHS r)

-- | Return the edges deleted by a rule
deletedEdges :: TypedGraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanTypedEdgeIds (getLHS r)

-- | Return the edges created by a rule
createdEdges :: TypedGraphRule a b -> [G.EdgeId]
createdEdges = TGM.orphanTypedEdgeIds . getRHS

preservedNodes :: TypedGraphRule a b -> [G.NodeId]
preservedNodes = nodeIdsFromDomain . getLHS

preservedEdges :: TypedGraphRule a b -> [G.EdgeId]
preservedEdges = edgeIdsFromDomain . getLHS

-- | Returns an empty TypedGraphRule
emptyGraphRule :: Graph (Maybe a) (Maybe b) -> TypedGraphRule a b
emptyGraphRule typegraph = emptyRule
  where
    emptyGraph = G.empty
    emptyGM = GM.empty emptyGraph typegraph
    emptyTGM = idMap emptyGM emptyGM
    emptyRule = buildProduction emptyTGM emptyTGM []

-- | Checks if it is a null rule
nullGraphRule :: TypedGraphRule a b -> Bool
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
