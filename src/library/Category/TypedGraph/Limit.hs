module Category.TypedGraph.Limit () where

import           Control.Arrow                ((***))
import           Data.List                    ((\\))
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Abstract.Category
import           Abstract.Category.Limit
import           Category.TypedGraph.Category
import qualified Data.Graphs                  as G
import qualified Data.Graphs.Morphism         as GM
import           Data.Partition
import           Data.TypedGraph              as TG
import           Data.TypedGraph.Morphism     as TG

instance Complete (TypedGraphMorphism a b) where

  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  --
  -- @
  --
  -- Pullback for typed graphs.
  -- It starts getting all pairs for nodes and edges, this pairs will be
  -- the elements of X.
  -- It creates an empty X, and morphisms f' and g', and adds each pair
  -- identifing them with iSet.
  calculatePullback f g = (f'',g'')
    where
      -- This first part just defines the names for the structures
      nodeTypeInB = GM.applyNodeIdUnsafe typedGraphB
      nodeTypeInA = GM.applyNodeIdUnsafe typedGraphA
      edgeTypeInB = GM.applyEdgeIdUnsafe typedGraphB
      edgeTypeInA = GM.applyEdgeIdUnsafe typedGraphA
      typeGraph = codomain typedGraphC
      typedGraphA = domain f
      typedGraphB = domain g
      typedGraphC = codomain f

      nodesInA = map fst . nodes $ typedGraphA
      nodesInB = map fst . nodes $ typedGraphB
      edgesInA = map fst . edges $ typedGraphA
      edgesInB = map fst . edges $ typedGraphB

      -- Discover the nodes and edges of the X
      nodesWithoutId = getPairs applyNodeIdUnsafe nodeId nodesInA nodesInB G.nodes
      nodesWithId = zip nodesWithoutId ([0..]::[Int])

      egdesWithoutId = getPairs applyEdgeIdUnsafe edgeId edgesInA edgesInB G.edges
      edgesWithId = zip egdesWithoutId ([0..]::[Int])

      -- Run the product for all elements that are mapped on the same element in C
      getPairs apply getId elemA elemB list = concatMap product comb
        where
          comb =
            map
              (\n ->
                (filter (\n' -> apply f (getId n') == getId n) elemA,
                filter (\n' -> apply g (getId n') == getId n) elemB))
              (list (domain typedGraphC))

          product (x,y) = [(a,b) | a <- x, b <- y]

      -- Init X, f' and g' as empty
      initX = GM.empty G.empty typeGraph
      initF' = buildTypedGraphMorphism initX typedGraphB (GM.empty G.empty (domain typedGraphB))
      initG' = buildTypedGraphMorphism initX typedGraphA (GM.empty G.empty (domain typedGraphA))

      -- Add all elements on X and their morphisms
      (g',f') = foldr updateNodes (initG',initF') nodesWithId
      (g'',f'') = foldr updateEdges (g',f') edgesWithId

      -- Add a node is just do it on the domain of f' and g'
      updateNodes ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newNode = NodeId newId
          updateG' = createNodeOnDomain newNode (nodeTypeInA (nodeId a)) (nodeId a) g'
          updateF' = createNodeOnDomain newNode (nodeTypeInB (nodeId b)) (nodeId b) f'

      -- Add an edge on the domain of f' and g'
      updateEdges ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newEdge = EdgeId newId

          -- To add an edge, their source and target nodes must be found (they already exists).
          -- It searches the node in X that is mapped by f' and g' to the same source (resp. target) node of the related edges on A and B.
          src1 =
            filter
              (\n ->
                applyNodeIdUnsafe f' n == sourceId b &&
                applyNodeIdUnsafe g' n == sourceId a)
              (nodeIds $ domain f')
          src = if Prelude.null src1 then error "src not found" else head src1

          tgt1 =
            filter
              (\n ->
                applyNodeIdUnsafe f' n == targetId b &&
                applyNodeIdUnsafe g' n == targetId a)
              (nodeIds $ domain f')
          tgt = if Prelude.null tgt1 then error "tgt not found" else head tgt1

          updateG' = createEdgeOnDomain newEdge src tgt (edgeTypeInA (edgeId a)) (edgeId a) g'
          updateF' = createEdgeOnDomain newEdge src tgt (edgeTypeInB (edgeId b)) (edgeId b) f'

  calculateEqualizer = calculateEqualizer'
  finalObject = finalGraph . typeGraph . codomain
  morphismToFinalFrom graph =
    TypedGraphMorphism graph (finalGraph . typeGraph $ graph) (toGraphMorphism graph)
  isFinal _ graph = isIsomorphism (toGraphMorphism graph)

-- | Given a type graph, create a final typed graph
finalGraph :: G.Graph (Maybe n) (Maybe e) -> TypedGraph n e
finalGraph  = fromGraphMorphism . identity

calculateEqualizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateEqualizer' f g = makeInclusion typedX typedA
  where
    fNodes = nodeIds $ domain f
    fEdges = edgeIds $ domain f
    typedA = domain f
    equivalentNodes = filter (\n -> applyNodeIdUnsafe f n == applyNodeIdUnsafe g n) fNodes
    equivalentEdges = filter (\e -> applyEdgeIdUnsafe f e == applyEdgeIdUnsafe g e) fEdges
    typedX = foldr GM.removeNodeFromDomain
                  (foldr GM.removeEdgeFromDomain typedA (fEdges \\ equivalentEdges))
                  (fNodes \\ equivalentNodes)


instance Cocomplete (TypedGraphMorphism a b) where
  initialObject = initialGraph . typeGraph . domainGraph
  morphismFromInitialTo g = TG.makeInclusion (initialGraph $ typeGraph g) g
  isInitial _ = TG.null

  calculateCoequalizer f g = calculateGenericCoequalizer (domain f) (codomain f) (mkPair f g)

  calculateNCoequalizer (f :| fs) = calculateGenericCoequalizer (domain f) (codomain f) (f : fs)

  calculateCoproduct a b =
    let
      [(nodeMapA, edgeMapA), (nodeMapB, edgeMapB)] = relableAll (1,1) [a, b]
      coproductGraph = TG.fromNodesAndEdges (typeGraph a)
        (Map.elems nodeMapA ++ Map.elems nodeMapB)
        (Map.elems edgeMapA ++ Map.elems edgeMapB)
    in
      ( makeEmbeddingInto coproductGraph a (nodeMapA, edgeMapA)
      , makeEmbeddingInto coproductGraph b (nodeMapB, edgeMapB)
      )

  calculateNCoproduct (g :| gs) =
    let
      relabellings = relableAll (1, 1) (g:gs)
      coproductGraph = TG.fromNodesAndEdges (typeGraph g)
        (concatMap (Map.elems . nodeMap) relabellings)
        (concatMap (Map.elems . edgeMap) relabellings)
    in
      zipWith (makeEmbeddingInto coproductGraph) (g:gs) relabellings
    
relableAll :: (NodeId, EdgeId) -> [TypedGraph n e] -> [Relabelling n e]
relableAll _ [] = []
relableAll (freeNodeId, freeEdgeId) (g:gs) =
    relableGraph g [freeNodeId..] [freeEdgeId..]
    : relableAll (freeNodeId', freeEdgeId') gs
  where
    freeNodeId' = freeNodeId + toEnum (length $ nodes g)
    freeEdgeId' = freeEdgeId + toEnum (length $ edges g)

initialGraph :: G.Graph (Maybe n) (Maybe e) -> TypedGraph n e
initialGraph = fromGraphMorphism . GM.empty G.empty

type TypedNode n = (Node (Maybe n), NodeId)
type TypedEdge e = (Edge (Maybe e), EdgeId)
type Relabelling n e = (Map NodeId (TypedNode n), Map EdgeId (TypedEdge e))

nodeMap :: Relabelling n e -> Map NodeId (TypedNode n)
nodeMap (m, _) = m

edgeMap :: Relabelling n e -> Map EdgeId (TypedEdge e)
edgeMap (_, m) = m

relableGraph :: TypedGraph n e -> [NodeId] -> [EdgeId] -> Relabelling n e
relableGraph graph newNodeIds newEdgeIds = (nodeMap, edgeMap)
  where
    nodeMap = Map.fromList
      [ (oldId, (Node newId info, ntype))
          | ((Node oldId info, ntype), newId) <- zip (nodes graph) newNodeIds
      ]
    edgeMap = Map.fromList
      [ (e, (Edge e' src' tgt' info, etype))
          | ((Edge e src tgt info, etype), e') <- zip (edges graph) newEdgeIds
          , let src' = nodeId . fst $ nodeMap Map.! src
          , let tgt' = nodeId . fst $ nodeMap Map.! tgt
      ]

makeEmbeddingInto :: TypedGraph n e -> TypedGraph n e -> Relabelling n e -> TypedGraphMorphism n e
makeEmbeddingInto codomain domain (nodeMap, edgeMap) =
  fromGraphsAndLists domain codomain (toList nodeId nodeMap) (toList edgeId edgeMap)
  where toList getId = map (id *** getId . fst) . Map.toList

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
  =>
  TypedGraph n e -> TypedGraph n e -> collection (TypedGraphMorphism n e) -> TypedGraphMorphism n e
calculateGenericCoequalizer sharedDomain sharedCodomain parallelMorphisms =
  let
    collapsedNodes = catMaybes
      [ filterMap (`applyNodeId` n) parallelMorphisms | n <- nodeIds sharedDomain ]
    nodePartition = mergeElements collapsedNodes (discretePartition $ nodeIds sharedCodomain)
    nodeRepresentatives = pickRepresentatives nodePartition $ \equivalentNodeIds ->
      let
        representativeId = Set.findMin equivalentNodeIds
        Just (Node _ info, Node ntype _, _) = lookupNodeInContext representativeId sharedCodomain
      in (Node representativeId info, ntype)
    nodeMapping = Map.fromList [(n, repr) | (ns, repr) <- nodeRepresentatives, n <- ns]

    collapsedEdges = catMaybes
      [ filterMap (`applyEdgeId` e) parallelMorphisms | e <- edgeIds sharedDomain ]
    edgePartition = mergeElements collapsedEdges (discretePartition $ edgeIds sharedCodomain)
    edgeRepresentatives = pickRepresentatives edgePartition $ \equivalentEdgeIds ->
      let
        representativeId = Set.findMin equivalentEdgeIds
        Just (_, Edge _ originalSource originalTarget info, Edge etype _ _ _, _) =
          lookupEdgeInContext representativeId sharedCodomain
        Just (Node mappedSource _, _) = Map.lookup originalSource nodeMapping
        Just (Node mappedTarget _, _) = Map.lookup originalTarget nodeMapping
      in (Edge representativeId mappedSource mappedTarget info, etype)
    edgeMapping = Map.fromList [(e, repr) | (es, repr) <- edgeRepresentatives, e <- es]

    coequalizerGraph =
      fromNodesAndEdges (typeGraph sharedCodomain) (map snd nodeRepresentatives) (map snd edgeRepresentatives)
  in
    fromGraphsAndLists sharedCodomain coequalizerGraph (toList nodeId nodeMapping) (toList edgeId edgeMapping)
  where
    toList getId = map (id *** getId . fst) . Map.toList
{-# INLINE calculateGenericCoequalizer #-}
