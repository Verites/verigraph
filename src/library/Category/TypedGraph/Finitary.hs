module Category.TypedGraph.Finitary () where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           Abstract.Category
import           Abstract.Category.Finitary
import           Category.TypedGraph.Category                    
import           Category.Graph                     ()
import           Data.Graphs                        (Node(..), Edge(..))
import qualified Data.Graphs                        as Graph
import qualified Data.Graphs.Morphism               as Untyped
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import qualified Data.Relation                      as Relation
import qualified Util.Map as Map
import qualified Data.Partition as Partition
import           Data.TypedGraph.Partition               (generateGraphPartitions)
import           Data.TypedGraph.Partition.FromVerigraph (createDisjointUnion)
import           Data.TypedGraph.Partition.ToVerigraph   (mountTypedGraphMorphisms)

instance MFinitary (TypedGraphMorphism n e) where
  inclusion = monic

  getCanonicalSubobject f = 
    let
      subgraphNodes = [ n' | (n, _) <- nodes (domain f), Just n' <- [applyNode f n] ]
      subgraphEdges = [ e' | (e, _) <- edges (domain f), Just e' <- [applyEdge f e] ]
    in assembleSubgraph subgraphNodes subgraphEdges (codomain f)

  getMorphismToCanonicalSubobject f =
    let
      canonicalSubobject = getCanonicalSubobject f
      morphismToCanonical = TypedGraphMorphism (domain f) (domain canonicalSubobject) (mapping f)
    in (morphismToCanonical, canonicalSubobject)

  findAllSubobjectsOf g = do
    let untypedG = toUntypedGraph g
    includedNodeIds <- getAllSubsets (Graph.nodeIds untypedG)
    let 
      includedEdgeIds' = map edgeId . filter srcTgtInNodes' $ Graph.edges untypedG
      srcTgtInNodes' (Edge _ srcId tgtId _) = srcId `Set.member` includedNodeIds && tgtId `Set.member` includedNodeIds
    includedEdgeIds <- getAllSubsets includedEdgeIds'

    let includedNodes = filter ((`Set.member` includedNodeIds) . nodeId) $ Graph.nodes untypedG
    let includedEdges = filter ((`Set.member` includedEdgeIds) . edgeId) $ Graph.edges untypedG
    return $ assembleSubgraph includedNodes includedEdges g

getAllSubsets :: Ord a => [a] -> [Set a]
getAllSubsets [] = [Set.empty]
getAllSubsets (x:xs) = concat
  [ [ subsetOfRest, Set.insert x subsetOfRest ]  | subsetOfRest <- getAllSubsets xs ]

assembleSubgraph :: [Node (Maybe n)] -> [Edge (Maybe e)] -> TypedGraph n e -> TypedGraphMorphism n e
assembleSubgraph subnodes subedges graph =
  let
    untypedSubgraph = Graph.fromNodesAndEdges subnodes subedges
    subtyping = Untyped.GraphMorphism untypedSubgraph (typeGraph graph)
      (Relation.filterDomain (Graph.isNodeOf untypedSubgraph) $ Untyped.nodeRelation graph)
      (Relation.filterDomain (Graph.isEdgeOf untypedSubgraph) $ Untyped.edgeRelation graph)
    inclusion = Untyped.fromGraphsAndLists untypedSubgraph (toUntypedGraph graph)
      [ (n, n) | n <- Graph.nodeIds untypedSubgraph ]
      [ (e, e) | e <- Graph.edgeIds untypedSubgraph ]
  in TypedGraphMorphism subtyping graph inclusion

instance ECofinitary (TypedGraphMorphism n e) where
  surjection = epic

  getCanonicalQuotient f =
    let
      nodeBlocks = Map.elems . Map.inverse . Map.toList . Relation.mapping . Untyped.nodeRelation $ mapping f
      edgeBlocks = Map.elems . Map.inverse . Map.toList . Relation.mapping . Untyped.edgeRelation $ mapping f
    in assembleQuotient nodeBlocks edgeBlocks (domain f)

  getMorphismFromCanonicalQuotient f =
    let
      canonicalQuotient = getCanonicalQuotient f
      untypedQuotient = toUntypedGraph (codomain canonicalQuotient)
      restrictedF = Untyped.GraphMorphism (domain $ mapping f) (codomain $ mapping f)
          (Relation.filterDomain (Graph.isNodeOf untypedQuotient) . Untyped.nodeRelation $ mapping f)
          (Relation.filterDomain (Graph.isEdgeOf untypedQuotient) . Untyped.edgeRelation $ mapping f)
      morphismFromCanonical = TypedGraphMorphism (codomain canonicalQuotient) (codomain f) restrictedF
    in (canonicalQuotient, morphismFromCanonical)

  findAllQuotientsOf g = map fst part
    where
      g' = Untyped.buildGraphMorphism Graph.empty Graph.empty [] []
      part = map (mountTypedGraphMorphisms g g') (generateGraphPartitions (createDisjointUnion (g, False) (g', False)))

assembleQuotient :: [[Graph.NodeId]] -> [[Graph.EdgeId]] -> TypedGraph n e -> TypedGraphMorphism n e
assembleQuotient nodeBlocks edgeBlocks graph =
  let
    nodeMap = Partition.partitionToSurjection (Partition.fromBlocks nodeBlocks) Set.findMin
    includedNodeIds = Set.fromList (Map.elems nodeMap)
    includedNodes = filter ((`Set.member` includedNodeIds) . nodeId) 
                  . Graph.nodes $ toUntypedGraph graph

    edgeMap = Partition.partitionToSurjection (Partition.fromBlocks edgeBlocks) Set.findMin
    includedEdgeIds = Set.fromList (Map.elems edgeMap)
    includedEdges = map correctSrcTgt 
                  . filter ((`Set.member` includedEdgeIds) . edgeId)
                  . Graph.edges $ toUntypedGraph graph
    correctSrcTgt (Edge e srcId tgtId x) = Edge e (nodeMap Map.! srcId) (nodeMap Map.! tgtId) x
  
    untypedQuotient = Graph.fromNodesAndEdges includedNodes includedEdges
    typing = Untyped.GraphMorphism untypedQuotient (typeGraph graph)
      (Relation.filterDomain (Graph.isNodeOf untypedQuotient) $ Untyped.nodeRelation graph)
      (Relation.filterDomain (Graph.isEdgeOf untypedQuotient) $ Untyped.edgeRelation graph)
    collapsing = Untyped.GraphMorphism (toUntypedGraph graph) untypedQuotient
      (Relation.fromMapAndCodomain nodeMap (Graph.nodeIds $ toUntypedGraph graph))
      (Relation.fromMapAndCodomain edgeMap (Graph.edgeIds $ toUntypedGraph graph))
  in TypedGraphMorphism graph typing collapsing


instance E'PairCofinitary (TypedGraphMorphism a b) where
  isJointSurjection (f, g) = 
      jointSurjection (Untyped.nodeRelation . mapping) nodeIds
      && jointSurjection (Untyped.edgeRelation . mapping) edgeIds
    where
      jointSurjection mapComponent graphComponent =
        Set.fromList (Relation.image (mapComponent f) ++ Relation.image (mapComponent g))
        == Set.fromList (graphComponent (codomain f))

  -- | Create all jointly surjective pairs of @m1@ and @m2@
  findJointSurjections (cls1', m1) (cls2', m2) =
    filterPairs cls1 fst . filterPairs cls2 snd . map (mountTypedGraphMorphisms m1 m2) $
      generateGraphPartitions (createDisjointUnion (m1,cls1 == Monomorphism) (m2, cls2 == Monomorphism))
    where
      cls1 = toMorphismType cls1'
      cls2 = toMorphismType cls2'
      filterPairs cls elem
        | cls == Monomorphism || cls == AnyMorphism = id
        | otherwise = filter ((`belongsToClass` toMorphismClass cls) . elem)
