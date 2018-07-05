{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.Cocomplete () where

import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NonEmpty
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes, listToMaybe, mapMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           Abstract.Category
import           Abstract.Category.Limit
import           Category.LabeledGraph.Category ()
import           Data.EnumMap                   (EnumMap)
import qualified Data.EnumMap                   as EnumMap
import           Data.LabeledGraph              hiding (edgeMap, empty, nodeMap)
import qualified Data.LabeledGraph              as Graph
import           Data.LabeledGraph.Morphism     hiding (edgeMap, nodeMap, variableMap)
import           Data.Partition
import           Data.Variable


instance Cocomplete LabeledMorphism where
  initialObject _ = Graph.empty

  calculateCoequalizer f g = calculateGenericCoequalizer (domain f) (codomain f) (mkPair f g)

  calculateNCoequalizer (f :| fs) = calculateGenericCoequalizer (domain f) (codomain f) (f : fs)

  calculateCoproduct a b =
    let
      (nodeMapA, edgeMapA, varMapA) = relableGraph a [0 ..] [0 ..] [0 ..]
      (numNodesA, numEdgesA, numVarsA) = (length (nodes a), length (edges a), EnumMap.size (freeVariableMap a))
      (nodeMapB, edgeMapB, varMapB) =
        relableGraph b [toEnum numNodesA ..] [toEnum numEdgesA ..] [toEnum numVarsA ..]
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
      relableAll :: (NodeId, EdgeId, VarId) -> [LabeledGraph] -> [Relabeling]
      relableAll _ [] = []
      relableAll (freeNodeId, freeEdgeId, freeVarId) (g:gs) =
          relableGraph g [freeNodeId..] [freeEdgeId..] [freeVarId..]
          : relableAll (freeNodeId', freeEdgeId', freeVarId') gs
        where
          freeNodeId' = freeNodeId + toEnum (length $ nodes g)
          freeEdgeId' = freeEdgeId + toEnum (length $ edges g)
          freeVarId' = freeVarId + toEnum (EnumMap.size $ freeVariableMap g)


type Relabeling = (EnumMap NodeId LNode, EnumMap EdgeId LEdge, EnumMap VarId Variable)

nodeMap :: Relabeling -> EnumMap NodeId LNode
nodeMap (m, _, _) = m

edgeMap :: Relabeling -> EnumMap EdgeId LEdge
edgeMap (_, m, _) = m

relableGraph :: LabeledGraph -> [NodeId] -> [EdgeId] -> [VarId] -> Relabeling
relableGraph graph newNodeIds newEdgeIds newVarIds = (nodeMap, edgeMap, varMap)
  where
    varMap = EnumMap.fromList
      [ (oldId, Variable newId names)
          | (Variable oldId names, newId) <- zip (freeVariablesOf graph) newVarIds ]
    nodeMap = EnumMap.fromList
      [ (oldId, Node newId newLabel)
          | (Node oldId oldLabel, newId) <- zip (nodes graph) newNodeIds
          , let newLabel = (`EnumMap.lookup` varMap) . varId =<< oldLabel
      ]
    edgeMap = EnumMap.fromList
      [ (e, Edge e' src' tgt' ())
          | (Edge e src tgt (), e') <- zip (edges graph) newEdgeIds
          , let src' = nodeId (nodeMap EnumMap.! src)
          , let tgt' = nodeId (nodeMap EnumMap.! tgt)
      ]

makeEmbeddingInto :: LabeledGraph -> LabeledGraph -> Relabeling -> LabeledMorphism
makeEmbeddingInto codomain domain (nodeMap, edgeMap, varMap) =
  LabeledMorphism domain codomain (EnumMap.map nodeId nodeMap) (EnumMap.map edgeId edgeMap) (EnumMap.map varId varMap)


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
      [ filterMap (lookupVarId v) parallelMorphisms | v <- freeVariableIdsOf sharedDomain ]
    variablePartition = mergeElements collapsedVarsFromFunctions $
      mergeSets collapsedVarsFromNodes (discretePartition $ freeVariableIdsOf sharedCodomain)

    variableRenaming = partitionToSurjection variablePartition $ \equivalentVarIds ->
      let
        representativeId = Set.findMin equivalentVarIds
        names = concat . mapMaybe (fmap varNameHints . lookupVarOnCodomain) $ Set.toList equivalentVarIds
      in Variable representativeId names
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
      (EnumMap.fromList . Map.toList $ fmap varId variableRenaming)
  where
    labelOnCodomain nodeId = lookupNode nodeId sharedCodomain >>= (fmap varId . nodeLabel)
    lookupVarOnCodomain varId = EnumMap.lookup varId codomainVarMap
    codomainVarMap = freeVariableMap sharedCodomain
{-# INLINE calculateGenericCoequalizer #-}
