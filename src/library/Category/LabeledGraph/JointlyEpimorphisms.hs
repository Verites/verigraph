{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.JointlyEpimorphisms where

import           Control.Monad
import           Data.Function                          (on)
import qualified Data.List                              as List
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (isNothing, mapMaybe)
import qualified Data.Set                               as Set

import           Abstract.Category.JointlyEpimorphisms
import           Category.LabeledGraph.FinitaryCategory ()
import qualified Data.EnumMap                           as EnumMap
import           Data.LabeledGraph
import           Data.LabeledGraph.Morphism
import           Data.Partition
import           Data.Variable
import qualified Util.Map                               as Map


instance JointlyEpimorphisms LabeledMorphism where

  createAllQuotients g = do
    variablesPartition <- allPartitionsOf (freeVariableIdsOf g)
    let varMap = partitionToSurjection variablesPartition $ \identifiedIds ->
          let
            representativeId = Set.findMin identifiedIds
            names = concat . mapMaybe (fmap varNameHints . lookupVarOnG) $ Set.toList identifiedIds
          in Variable representativeId names
    let varIdMap = fmap varId varMap

    -- Two labeled nodes can only be identified if they have the same label. Thus, we first
    -- partition labeled nodes according to their label, then pick a refinement of this partition,
    -- then add unlabeled nodes to it (identifing them with any other node).
    let renamedNodes =
          [Node n (Map.lookupMaybe (varId <$> v) varMap) | Node n v <- nodes g]
    let (unlabeledNodes, labeledNodes) = List.partition (isNothing . nodeLabel) renamedNodes
    let nodeLabels = EnumMap.fromList [(nodeId n, v) | n <- labeledNodes, let Just v = nodeLabel n]

    let groupedNodesByLabel = [map nodeId group | group <- partitionBy (fmap varId . nodeLabel) labeledNodes]
    labeledNodesPartition <- allRefinementsOf (toPartition groupedNodesByLabel)
    nodesPartition <- foldM (flip addToPartition) labeledNodesPartition (map nodeId unlabeledNodes)
    let nodeMap = partitionToSurjection nodesPartition $ \identifiedIds ->
          let
            representativeId = Set.findMin identifiedIds
            label = maybeHead $ mapMaybe (`EnumMap.lookup` nodeLabels) (Set.toList identifiedIds)
          in Node representativeId label
    let nodeIdMap = fmap nodeId nodeMap

    -- Two edges can only be identified if they have the same source/target pair. Thus, we first
    -- partition edges according to theirs source/target pairs, then pick a refinement.
    let renamedEdges =
          [ Edge e (nodeIdMap Map.! src) (nodeIdMap Map.! tgt) () | Edge e src tgt () <- edges g ]
    let groupedEdgesBySrcTgt = [map edgeId group | group <- partitionBy sourceTarget renamedEdges]
    edgesPartition <- allRefinementsOf (toPartition groupedEdgesBySrcTgt)
    let edgeIdMap = partitionToSurjection edgesPartition Set.findMin

    let quotientGraph = fromNodesAndEdges (Map.elems nodeMap) (filterRepresentatives edgeId edgeIdMap renamedEdges)
    return $ LabeledMorphism g quotientGraph
      (EnumMap.fromList . Map.toList $ nodeIdMap)
      (EnumMap.fromList . Map.toList $ edgeIdMap)
      (EnumMap.fromList . Map.toList $ varIdMap)
    where
      sourceTarget (Edge _ src tgt _) = (src, tgt)
      lookupVarOnG v = EnumMap.lookup v gVarMap
      gVarMap = freeVariableMap g


type ListPartition a = [[a]]

toPartition :: Ord a => ListPartition a -> Partition a
toPartition = Set.fromList . map Set.fromList

-- | Filter a list of ids, keeping only the elements that are in the image of the given map.
filterRepresentatives :: Ord a => (t -> a) -> Map k a -> [t] -> [t]
filterRepresentatives getId idMap =
  let representativeIds = Set.fromList (Map.elems idMap)
  in filter (\e -> Set.member (getId e) representativeIds)

-- | Given a projection function, partition a list such that each group has elements with equal
-- projections.
partitionBy :: Ord b => (a -> b) -> [a] -> ListPartition a
partitionBy projection = List.groupBy ((==) `on` projection) . List.sortBy (compare `on` projection)

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x
