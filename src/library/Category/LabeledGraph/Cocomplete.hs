{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.Cocomplete () where

import           Data.EnumMap                           (EnumMap)
import qualified Data.EnumMap                           as EnumMap
import qualified Data.List                              as List
import           Data.List.NonEmpty                     (NonEmpty (..))
import qualified Data.List.NonEmpty                     as NonEmpty
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (catMaybes, listToMaybe, mapMaybe)
import           Data.Semigroup
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Abstract.Category.Cocomplete
import           Abstract.Category.FinitaryCategory
import           Category.LabeledGraph.FinitaryCategory ()
import           Data.LabeledGraph                      hiding (edgeMap, empty, nodeMap)
import qualified Data.LabeledGraph                      as Graph
import           Data.LabeledGraph.Morphism             hiding (edgeMap, nodeMap, variableMap)
import           Data.Partition
import           Data.Variable


instance Cocomplete LabeledMorphism where
  initialObject _ = Graph.empty

  calculateCoequalizer f g = calculateGenericCoequalizer (domain f) (codomain f) (mkPair f g)

  calculateNCoequalizer (f :| fs) = calculateGenericCoequalizer (domain f) (codomain f) (f : fs)

  calculateCoproduct a b =
    let
      (nodeMapA, edgeMapA, varMapA) = relableGraph a [0 ..] [0 ..] "_a"
      (nodeMapB, edgeMapB, varMapB) =
        relableGraph b [toEnum $ length (nodes a) ..] [toEnum $ length (edges a) ..] "_b"
      coproductGraph = Graph.fromNodesAndEdges
        (EnumMap.elems nodeMapA ++ EnumMap.elems nodeMapB)
        (EnumMap.elems edgeMapA ++ EnumMap.elems edgeMapB)
    in
      ( makeEmbeddingInto coproductGraph a (nodeMapA, edgeMapA, varMapA)
      , makeEmbeddingInto coproductGraph b (nodeMapB, edgeMapB, varMapB)
      )

  calculateNCoproduct graphs' =
    let
      graphs = NonEmpty.toList graphs'
      relabelings = relableAll (0, 0, 0) graphs
      coproductGraph = Graph.fromNodesAndEdges
        (concatMap (EnumMap.elems . nodeMap) relabelings)
        (concatMap (EnumMap.elems . edgeMap) relabelings)
    in
      zipWith (makeEmbeddingInto coproductGraph) graphs relabelings
    where
      relableAll :: (NodeId, EdgeId, Int) -> [LabeledGraph] -> [Relabeling]
      relableAll _ [] = []
      relableAll (freeNodeId, freeEdgeId, varSuffix) (g:gs) =
          relableGraph g [freeNodeId..] [freeEdgeId..] (Text.pack $ '_' : show varSuffix)
          : relableAll (freeNodeId', freeEdgeId', varSuffix + 1) gs
        where
          freeNodeId' = freeNodeId + toEnum (length $ nodes g)
          freeEdgeId' = freeEdgeId + toEnum (length $ edges g)


type Relabeling = (EnumMap NodeId LNode, EnumMap EdgeId LEdge, Map Variable Variable)

nodeMap :: Relabeling -> EnumMap NodeId LNode
nodeMap (m, _, _) = m

edgeMap :: Relabeling -> EnumMap EdgeId LEdge
edgeMap (_, m, _) = m

relableGraph :: LabeledGraph -> [NodeId] -> [EdgeId] -> Text -> Relabeling
relableGraph graph newNodeIds newEdgeIds varSuffix = (nodeMap, edgeMap, varMap)
  where
    varMap = Map.fromList [ (v, v <> varSuffix) | v <- freeVariablesOf graph ]
    nodeMap = EnumMap.fromList
      [ (oldId, Node newId newAttribute)
          | (Node oldId oldAttribute, newId) <- zip (nodes graph) newNodeIds
          , let newAttribute = oldAttribute >>= (`Map.lookup` varMap)
      ]
    edgeMap = EnumMap.fromList
      [ (e, Edge e' src' tgt' ())
          | (Edge e src tgt (), e') <- zip (edges graph) newEdgeIds
          , let src' = nodeId (nodeMap EnumMap.! src)
          , let tgt' = nodeId (nodeMap EnumMap.! tgt)
      ]

makeEmbeddingInto :: LabeledGraph -> LabeledGraph -> Relabeling -> LabeledMorphism
makeEmbeddingInto codomain domain (nodeMap, edgeMap, varMap) =
  LabeledMorphism domain codomain (EnumMap.map nodeId nodeMap) (EnumMap.map edgeId edgeMap) varMap


-- * Coequalizer algorithm

newtype Pair a = Pair { unPair :: (a, a) }
instance Show a => Show (Pair a) where show = show . unPair

mkPair :: a -> a -> Pair a
mkPair x y = Pair (x, y)

class Coequalizable collection where

  -- | Apply the given function to all elements of the given collection, returning the collection
  -- of non-'Nothing' results. If the resulting collection would be empty, may return 'Nothing'.
  filterMap :: Ord b => (a -> Maybe b) -> collection a -> Maybe (collection b)

  -- | Given collections of elements that should belong to the same partition, "collapses" the
  -- appropriate equivalent classes of the given partition.
  mergeElements :: (Ord a, Show a) => [collection a] -> Partition a -> Partition a


instance Coequalizable Pair where
  filterMap f (Pair (a, b)) = mkPair <$> f a <*> f b
  {-# INLINE filterMap #-}

  mergeElements = mergePairs . map unPair
  {-# INLINE mergeElements #-}

instance Coequalizable [] where
  filterMap f = Just . mapMaybe f
  {-# INLINE filterMap #-}

  mergeElements = mergeSets . map Set.fromList
  {-# INLINE mergeElements #-}

instance Coequalizable Set where
  filterMap f = Just . Set.fromList . mapMaybe f . Set.toList
  {-# INLINE filterMap #-}

  mergeElements = mergeSets
  {-# INLINE mergeElements #-}


calculateGenericCoequalizer :: Coequalizable collection
  => LabeledGraph -> LabeledGraph -> collection LabeledMorphism -> LabeledMorphism
calculateGenericCoequalizer sharedDomain sharedCodomain parallelMorphisms =
  let
    collapsedEdges = catMaybes
      [ filterMap (lookupEdgeId e) parallelMorphisms | e <- edgeIds sharedDomain ]
    edgePartition = mergeElements collapsedEdges (discretePartition $ edgeIds sharedCodomain)

    collapsedNodes = catMaybes
      [ filterMap (lookupNodeId n) parallelMorphisms | n <- nodeIds sharedDomain ]
    nodePartition = mergeElements collapsedNodes (discretePartition $ nodeIds sharedCodomain)

    collapsedVarsFromNodes = catMaybes
      [ filterMap labelOnCodomain block | block <- Set.toList nodePartition ]
    collapsedVarsFromFunctions = catMaybes
      [ filterMap (applyToVariable v) parallelMorphisms | v <- freeVariablesOf sharedDomain ]
    variablePartition = mergeElements collapsedVarsFromFunctions $
      mergeSets collapsedVarsFromNodes (discretePartition $ freeVariablesOf sharedCodomain)

    variableRenaming = partitionToSurjection variablePartition
      (Text.concat . List.intersperse "__" . Set.toList)
    nodeMapping = partitionToSurjection nodePartition $ \equivalentNodeIds ->
      let
        representativeId = Set.findMin equivalentNodeIds
        originalLabels = mapMaybe labelOnCodomain (Set.toList equivalentNodeIds)
        renamedLabel = listToMaybe originalLabels >>= (`Map.lookup` variableRenaming)
      in Node representativeId renamedLabel
    edgeMapping = partitionToSurjection edgePartition $ \equivalentEdgeIds ->
      let
        representativeId = Set.findMin equivalentEdgeIds
        Just (Edge _ originalSource originalTarget _) = lookupEdge representativeId sharedCodomain
        Just (Node mappedSource _) = Map.lookup originalSource nodeMapping
        Just (Node mappedTarget _) = Map.lookup originalTarget nodeMapping
      in Edge representativeId mappedSource mappedTarget ()

    coequalizerGraph = fromNodesAndEdges (Map.elems nodeMapping) (Map.elems edgeMapping)
  in
    LabeledMorphism sharedCodomain coequalizerGraph
      (EnumMap.fromList . Map.toList $ fmap nodeId nodeMapping)
      (EnumMap.fromList . Map.toList $ fmap edgeId edgeMapping)
      variableRenaming
  where
    labelOnCodomain nodeId = lookupNode nodeId sharedCodomain >>= nodeLabel
{-# INLINE calculateGenericCoequalizer #-}
