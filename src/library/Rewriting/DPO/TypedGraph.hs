{-# LANGUAGE FlexibleInstances #-}

module Rewriting.DPO.TypedGraph (
    -- * Types
      TypedGraphRule
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
    , buildGraphRule

    , isDeleted
) where

import           Abstract.Category.FinitaryCategory as FC
import           Abstract.Rewriting.DPO             as DPO
import           Category.TypedGraph                ()
import           Category.TypedGraph.AdhesiveHLR
import           Category.TypedGraph.FindMorphism   ()
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph                    as GM
import           Data.TypedGraph.Morphism           as TGM

type TypedGraphRule a b = Production (TypedGraphMorphism a b)

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
emptyGraphRule :: Graph (Maybe a) (Maybe b) -> Production (TypedGraphMorphism a b)
emptyGraphRule typegraph = emptyRule
  where
    emptyGraph = empty
    emptyGM = GM.empty emptyGraph typegraph
    emptyTGM = idMap emptyGM emptyGM
    emptyRule = buildProduction emptyTGM emptyTGM []

type ListOfNodesAndEdges = ([(Int,Int)],[(Int,Int,Int,Int)])

-- | It builds a TypedGraphRule with lists of deleted, created, preserved and forbidden elements
buildGraphRule :: Graph (Maybe a) (Maybe b) -> ListOfNodesAndEdges -> ListOfNodesAndEdges -> ListOfNodesAndEdges -> [ListOfNodesAndEdges] -> Production (TypedGraphMorphism a b)
buildGraphRule typegraph deleted created (preservedNodes, preservedEdges) nacs = resultingRule
  where
    -- Creates a typedgraph with the preserved elements and mounts an initial rule with preserves them
    preservedGraph = build (map fst preservedNodes) (map (\(e,s,t,_) -> (e,s,t)) preservedEdges)
    preservedTypeGraph = GM.buildGraphMorphism preservedGraph typegraph preservedNodes (map (\(e,_,_,t) -> (e,t)) preservedEdges)
    leftAndRightPreserved = FC.identity preservedTypeGraph

    -- Creates indicated elements on codomain of the initial rule
    addCreated = addElementsOnCodomain leftAndRightPreserved created
    addDeleted = addElementsOnCodomain leftAndRightPreserved deleted

    ---- Nacs part

    -- Each NAC starts from a "initial" id of L ...
    idLeft = FC.identity (codomain addDeleted)
    -- and adds all forbidden elements on codomain of this initial
    resultingNacs = map (addElementsOnCodomain idLeft) nacs

    -- The rule instantiation
    resultingRule = buildProduction addDeleted addCreated resultingNacs

    -- Function that adds nodes and edges on the codomain of an init typed graph morphism
    addElementsOnCodomain init (nodes,edges) = addEdges
      where
        addNodes = foldr (\(n,t) -> TGM.createNodeOnCodomain (NodeId n) (NodeId t)) init nodes
        addEdges = foldr (\(e,s,t,tp) -> TGM.createEdgeOnCodomain (EdgeId e) (NodeId s) (NodeId t) (EdgeId tp)) addNodes edges


-- | Checks if it is a null rule
nullGraphRule :: TypedGraphRule a b -> Bool
nullGraphRule rule = null l && null k && null r
  where
    null = G.null . untypedGraph
    l = codomain $ getLHS rule
    k = domain $ getLHS rule
    r = codomain $ getRHS rule

-- TODO: this probably shouldn't be here
instance DPO (TypedGraphMorphism a b) where

  invertProduction conf rule =
    buildProduction (getRHS rule) (getLHS rule) (concatMap (shiftNacOverProduction conf rule) (getNACs rule))

  shiftNacOverProduction conf rule nac = [calculateComatch nac rule | satisfiesGluingConditions conf rule nac]
