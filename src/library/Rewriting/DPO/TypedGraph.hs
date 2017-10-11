{-# LANGUAGE FlexibleInstances #-}

module Rewriting.DPO.TypedGraph
(-- * Types
  TypedGraphRule
, Production(..)
, NamedTypedGraphRule
, leftObject
, rightObject
, interfaceObject

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

import           Abstract.Category
import           Abstract.Rewriting.DPO             as DPO
import           Category.TypedGraph.Category       (toMorphismType, MorphismType(..))
import           Category.TypedGraph                
import           Category.TypedGraph.Adhesive       (isDeleted)
import  Data.Graphs                        (Graph)
import qualified Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph                    as GM
import           Data.TypedGraph.Morphism           as TGM
import           Data.TypedGraph.Partition               (generateGraphPartitions)
import           Data.TypedGraph.Partition.ToVerigraph   (mountTypedGraphMorphisms)
import           Data.TypedGraph.Partition.FromVerigraph (createSatisfyingNacsDisjointUnion)

type TypedGraphRule a b = Production (TypedGraphMorphism a b)
type NamedTypedGraphRule a b = NamedProduction (TypedGraphMorphism a b)

-- | Return the nodes deleted by a rule
deletedNodes :: TypedGraphRule a b -> [G.NodeId]
deletedNodes r = TGM.orphanTypedNodeIds (leftMorphism r)

-- | Return the nodes created by a rule
createdNodes :: TypedGraphRule a b -> [G.NodeId]
createdNodes r = TGM.orphanTypedNodeIds (rightMorphism r)

-- | Return the edges deleted by a rule
deletedEdges :: TypedGraphRule a b -> [G.EdgeId]
deletedEdges r = TGM.orphanTypedEdgeIds (leftMorphism r)

-- | Return the edges created by a rule
createdEdges :: TypedGraphRule a b -> [G.EdgeId]
createdEdges = TGM.orphanTypedEdgeIds . rightMorphism

preservedNodes :: TypedGraphRule a b -> [G.NodeId]
preservedNodes = nodeIds . domain . leftMorphism

preservedEdges :: TypedGraphRule a b -> [G.EdgeId]
preservedEdges = edgeIds . domain . leftMorphism

-- | Returns an empty TypedGraphRule
emptyGraphRule :: Graph (Maybe a) (Maybe b) -> TypedGraphRule a b
emptyGraphRule typegraph = emptyRule
  where
    emptyGM = GM.empty G.empty typegraph
    emptyTGM = makeInclusion emptyGM emptyGM
    emptyRule = Production emptyTGM emptyTGM []

-- | Checks if it is a null rule
nullGraphRule :: TypedGraphRule a b -> Bool
nullGraphRule rule = null l && null k && null r
  where
    null = G.null . toUntypedGraph
    l = codomain $ leftMorphism rule
    k = domain $ leftMorphism rule
    r = codomain $ rightMorphism rule

instance DPO (TypedGraphMorphism a b) where

  invertProduction conf rule =
    Production (rightMorphism rule) (leftMorphism rule) (concatMap (shiftNacOverProduction conf rule) (nacs rule))

  shiftNacOverProduction conf rule nac = [calculateComatch nac rule | satisfiesGluingConditions conf rule nac]

  createJointlyEpimorphicPairsFromNAC conf r nac =
    map (mountTypedGraphMorphisms r (codomain nac)) (generateGraphPartitions labeled)
    where
      injectiveMatch = toMorphismType (matchRestriction conf) == Monomorphism
      totalInjectiveNac = True

      labeled = createSatisfyingNacsDisjointUnion (r, injectiveMatch) (nac, totalInjectiveNac)
