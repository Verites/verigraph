{-# LANGUAGE FlexibleInstances #-}

module Rewriting.DPO.TypedGraph
(-- * Types
  TypedGraphRule
, NamedTypedGraphRule
, TypedGraphGrammar
, Production(..)
, leftObject
, interfaceObject
, rightObject

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
) where

import           Abstract.Rewriting.DPO             as DPO
import           Category.TypedGraph                
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph                    as GM
import           Data.TypedGraph.Morphism           as TGM
import           Util.Monad

type TypedGraphRule a b = Production (TGraphCat a b) (TypedGraphMorphism a b)
type NamedTypedGraphRule a b = NamedProduction (TGraphCat a b) (TypedGraphMorphism a b)
type TypedGraphGrammar a b = Grammar (TGraphCat a b) (TypedGraphMorphism a b)

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
preservedNodes = nodeIdsFromDomain . leftMorphism

preservedEdges :: TypedGraphRule a b -> [G.EdgeId]
preservedEdges = edgeIdsFromDomain . leftMorphism

-- | Returns an empty TypedGraphRule
emptyGraphRule :: Graph (Maybe a) (Maybe b) -> TypedGraphRule a b
emptyGraphRule typegraph = emptyRule
  where
    emptyGraph = empty
    emptyGM = GM.empty emptyGraph typegraph
    emptyTGM = idMap emptyGM emptyGM
    emptyRule = buildProduction emptyTGM emptyTGM []

-- | Checks if it is a null rule
nullGraphRule :: TypedGraphRule a b -> Bool
nullGraphRule rule = null (leftObject rule) && null (interfaceObject rule) && null (rightObject rule)
  where null = G.null . untypedGraph

instance DPO (TGraphCat a b) (TypedGraphMorphism a b) where
  invertProduction rule = do
    shiftedNacs <- concatMapM (shiftNacOverProduction rule) (nacs rule)
    return $ buildProduction (rightMorphism rule) (leftMorphism rule) shiftedNacs

  shiftNacOverProduction rule nac = do
    canRewrite <- satisfiesGluingConditions rule nac
    if canRewrite
      then (:[]) <$> calculateComatch nac rule
      else return []
