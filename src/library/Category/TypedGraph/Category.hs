{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Category.TypedGraph.Category
( TGraphCat
, TGraphConfig(..)
, MatchRestriction(..)
, TGraphMorphismClass(..)
, fixedClass
, matchMorphismsClass
, runCat
, getTypeGraph
, resolveClass
, belongsTo
, assembleSubgraph
, assembleQuotient
)  where

import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import           Abstract.Category.NewClasses
import           Category.Graph                     (Graph)
import qualified Category.Graph                     as Graph
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


newtype TGraphCat n e a = TGC { unTGC :: Reader (TGraphConfig n e) a }
  deriving (Functor, Applicative, Monad)

data TGraphConfig n e = TGraphConfig
  { catTypeGraph :: Graph (Maybe n) (Maybe e)
  , matchRestriction :: MatchRestriction
  }

runCat :: TGraphCat n e a -> TGraphConfig n e -> a
runCat = runReader . unTGC

getTypeGraph :: TGraphCat n e (Graph (Maybe n) (Maybe e))
getTypeGraph = TGC $ asks catTypeGraph

getMatchRestriction :: TGraphCat n e MatchRestriction
getMatchRestriction = TGC $ asks matchRestriction

data MatchRestriction = AllMatches | MonicMatches
  deriving (Eq, Show)

data TGraphMorphismClass
  = AllMorphisms
  | Monomorphisms
  | Epimorphisms
  | Isomorphisms
  deriving (Eq, Show)

fixedClass :: TGraphMorphismClass -> MorphismClass (TGraphCat n e)
fixedClass = FixedClass

matchMorphismsClass :: MorphismClass (TGraphCat n e)
matchMorphismsClass = MatchMorphisms

instance Category (TGraphCat n e) (TypedGraphMorphism n e) where
  type Obj (TGraphCat n e) = TypedGraph n e

  domain = domainGraph
  codomain = codomainGraph

  t2 <&> t1 = compose t2 t1
  identity t = TypedGraphMorphism t t (identity $ domain t)

  data MorphismClass (TGraphCat n e)
    = FixedClass TGraphMorphismClass
    | MatchMorphisms
    deriving (Eq, Show)

  anyMorphism = FixedClass AllMorphisms
  monic = FixedClass Monomorphisms
  epic = FixedClass Epimorphisms
  iso = FixedClass Isomorphisms
  
  isSubclassOf c1 c2 = isSubclassOf' <$> resolveClass c1 <*> resolveClass c2
    where
      _ `isSubclassOf'` AllMorphisms = True
      Monomorphisms `isSubclassOf'` Monomorphisms = True
      Epimorphisms `isSubclassOf'` Epimorphisms = True
      Isomorphisms `isSubclassOf'` Isomorphisms = True
      Isomorphisms `isSubclassOf'` Monomorphisms = True
      Isomorphisms `isSubclassOf'` Epimorphisms = True
      _ `isSubclassOf'` _ = False
  
  belongsToClass f c = belongsTo f <$> resolveClass c
  

resolveClass :: MorphismClass (TGraphCat n e) -> TGraphCat n e (TGraphMorphismClass)
resolveClass (FixedClass c) = return c
resolveClass (MatchMorphisms) = do
  restriction <- getMatchRestriction
  return $ case restriction of
    AllMatches -> AllMorphisms
    MonicMatches -> Monomorphisms

belongsTo :: TypedGraphMorphism n e -> TGraphMorphismClass -> Bool
_ `belongsTo` AllMorphisms = True
f `belongsTo` Monomorphisms = Graph.runCat . isMonic $ mapping f
f `belongsTo` Epimorphisms = Graph.runCat . isEpic $ mapping f
f `belongsTo` Isomorphisms = Graph.runCat . isIsomorphism $ mapping f

instance MFinitary (TGraphCat n e) (TypedGraphMorphism n e) where
  subobject = FixedClass Monomorphisms

  getCanonicalSubobject f = return $
    let
      dom = untypedGraph (domain f)
      subgraphNodes = [ n' | n <- Graph.nodes dom, Just n' <- [applyNode f n] ]
      subgraphEdges = [ e' | e <- Graph.edges dom, Just e' <- [applyEdge f e] ]
    in assembleSubgraph subgraphNodes subgraphEdges (codomain f)

  getMorphismToCanonicalSubobject f = do
    canonicalSubobject <- getCanonicalSubobject f
    let morphismToCanonical = TypedGraphMorphism (domain f) (domain canonicalSubobject) (mapping f)
    return (morphismToCanonical, canonicalSubobject)

  findAllSubobjectsOf g = return $ do
    let untypedG = untypedGraph g
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
    inclusion = Untyped.GraphMorphism untypedSubgraph (untypedGraph graph)
      (Relation.fromList [ (n, n) | n <- Graph.nodeIds untypedSubgraph ])
      (Relation.fromList [ (e, e) | e <- Graph.edgeIds untypedSubgraph ])
  in TypedGraphMorphism subtyping graph inclusion

instance ECofinitary (TGraphCat n e) (TypedGraphMorphism n e) where
  quotient = FixedClass Epimorphisms

  getCanonicalQuotient f = return $
    let
      nodeBlocks = Map.elems . Map.inverse . Map.toList . Relation.mapping . Untyped.nodeRelation $ mapping f
      edgeBlocks = Map.elems . Map.inverse . Map.toList . Relation.mapping . Untyped.edgeRelation $ mapping f
    in assembleQuotient nodeBlocks edgeBlocks (domain f)

  getMorphismFromCanonicalQuotient f = do
    canonicalQuotient <- getCanonicalQuotient f
    let untypedQuotient = untypedGraph (codomain canonicalQuotient)
    let restrictedF = Untyped.GraphMorphism (domain $ mapping f) (codomain $ mapping f)
          (Relation.filterDomain (Graph.isNodeOf untypedQuotient) . Untyped.nodeRelation $ mapping f)
          (Relation.filterDomain (Graph.isEdgeOf untypedQuotient) . Untyped.edgeRelation $ mapping f)
    let morphismFromCanonical = TypedGraphMorphism (codomain canonicalQuotient) (codomain f) restrictedF
    return (canonicalQuotient, morphismFromCanonical)

  findAllQuotientsOf g = return $ map fst part
    where
      g' = Untyped.buildGraphMorphism Graph.empty Graph.empty [] []
      part = map (mountTypedGraphMorphisms g g') (generateGraphPartitions (createDisjointUnion (g, False) (g', False)))

assembleQuotient :: [[Graph.NodeId]] -> [[Graph.EdgeId]] -> TypedGraph n e -> TypedGraphMorphism n e
assembleQuotient nodeBlocks edgeBlocks graph =
  let
    nodeMap = Partition.partitionToSurjection (Partition.fromBlocks nodeBlocks) Set.findMin
    includedNodeIds = Set.fromList (Map.elems nodeMap)
    includedNodes = filter ((`Set.member` includedNodeIds) . nodeId) 
                  . Graph.nodes $ untypedGraph graph

    edgeMap = Partition.partitionToSurjection (Partition.fromBlocks edgeBlocks) Set.findMin
    includedEdgeIds = Set.fromList (Map.elems edgeMap)
    includedEdges = map correctSrcTgt 
                  . filter ((`Set.member` includedEdgeIds) . edgeId)
                  . Graph.edges $ untypedGraph graph
    correctSrcTgt (Edge e srcId tgtId x) = Edge e (nodeMap Map.! srcId) (nodeMap Map.! tgtId) x
  
    untypedQuotient = Graph.fromNodesAndEdges includedNodes includedEdges
    typing = Untyped.GraphMorphism untypedQuotient (typeGraph graph)
      (Relation.filterDomain (Graph.isNodeOf untypedQuotient) $ Untyped.nodeRelation graph)
      (Relation.filterDomain (Graph.isEdgeOf untypedQuotient) $ Untyped.edgeRelation graph)
    collapsing = Untyped.GraphMorphism (untypedGraph graph) untypedQuotient
      (Relation.fromMapAndCodomain nodeMap (Graph.nodeIds $ untypedGraph graph))
      (Relation.fromMapAndCodomain edgeMap (Graph.edgeIds $ untypedGraph graph))
  in TypedGraphMorphism graph typing collapsing


instance EM'Factorizable (TGraphCat n e) (TypedGraphMorphism n e) where
  monicFactor = FixedClass Monomorphisms

  factorize f = return $
    let
      untypedCodomain = untypedGraph (codomain f)
      imageNodeIds = Set.fromList . Relation.image . Untyped.nodeRelation $ mapping f
      imageNodes = filter ((`Set.member` imageNodeIds) . nodeId) (Graph.nodes untypedCodomain)
      imageEdgeIds = Set.fromList . Relation.image . Untyped.edgeRelation $ mapping f
      imageEdges = filter ((`Set.member` imageEdgeIds) . edgeId) (Graph.edges untypedCodomain)
      monicFactor = assembleSubgraph imageNodes imageEdges (codomain f)
      epicFactor = TypedGraphMorphism (domain f) (domain monicFactor) (mapping f)
    in (epicFactor, monicFactor)


instance EM'PairFactorizable (TGraphCat n e) (TypedGraphMorphism n e) where

  findJointlyEpicPairs (cls1', x1) (cls2', x2) = do
    cls1 <- resolveClass cls1'
    cls2 <- resolveClass cls2'
    return
      . ensureInClass cls1 fst
      . ensureInClass cls2 snd
      . map (mountTypedGraphMorphisms x1 x2)
      . generateGraphPartitions
      $ createDisjointUnion (x1, cls1 == Monomorphisms) (x2, cls2 == Monomorphisms)

  findJointlyEpicSquares (cls1, m1) (cls2, m2) = do
    pairs <- findJointlyEpicPairs (cls1, codomain m1) (cls2, codomain m2)
    return . map flipPair . filter isCommutingSquare $ pairs
    where 
      isCommutingSquare (e1, e2) = e1 <&> m1 == e2 <&> m2
      flipPair (x,y) = (y,x)

ensureInClass :: TGraphMorphismClass -> (a -> TypedGraphMorphism n e) -> [a] -> [a]
ensureInClass AllMorphisms _ = id
ensureInClass Monomorphisms _ = id
ensureInClass cls elem = filter ((`belongsTo` cls) . elem)
{-# INLINE ensureInClass #-}