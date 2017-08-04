{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraph.Limit () where

import           Data.List.NonEmpty                 (NonEmpty(..))
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (fromJust)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set

import           Abstract.Category.NewClasses
import           Category.Graph                     ()
import           Category.TypedGraph.Category
import           Data.Graphs                        (Node(..), Edge(..), NodeId(..), EdgeId(..))
import qualified Data.Graphs                        as Graph
import qualified Data.Graphs.Morphism               as Untyped
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Data.Partition


instance Complete (CatM n e) (TypedGraphMorphism n e) where
  getFinalObject = identity <$> getTypeGraph

  getMorphismToFinalObjectFrom g = return $
    TypedGraphMorphism g (identity $ typeGraph g) g

  calculateEqualizer f g = return $
    let
      dom = untypedGraph (domain f)
      nodes' = [ node | node@(Node n _) <- Graph.nodes dom, applyNodeId f n == applyNodeId g n ]
      edges' = [ edge | edge@(Edge e _ _ _) <- Graph.edges dom, applyEdgeId f e == applyEdgeId g e ]
    in assembleSubgraph nodes' edges' (domain f)

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
  -- identifing them with ids.
  calculatePullback f g = return (f'',g'')
    where
      -- This first part just defines the names for the structures
      nodeTypeInB = Untyped.applyNodeIdUnsafe typedGraphB
      nodeTypeInA = Untyped.applyNodeIdUnsafe typedGraphA
      edgeTypeInB = Untyped.applyEdgeIdUnsafe typedGraphB
      edgeTypeInA = Untyped.applyEdgeIdUnsafe typedGraphA
      typeGraph = codomain typedGraphC
      typedGraphA = domain f
      typedGraphB = domain g
      typedGraphC = codomain f

      nodesInA = nodesFromDomain f
      nodesInB = nodesFromDomain g
      edgesInA = edgesFromDomain f
      edgesInB = edgesFromDomain g

      -- Discover the nodes and edges of the X
      nodesWithoutId = getPairs applyNodeIdUnsafe nodeId nodesInA nodesInB Graph.nodes
      nodesWithId = zip nodesWithoutId ([0..]::[Int])

      egdesWithoutId = getPairs applyEdgeIdUnsafe edgeId edgesInA edgesInB Graph.edges
      edgesWithId = zip egdesWithoutId ([0..]::[Int])

      -- Run the product for all elements that are mapped on the same element in C
      getPairs :: Eq id => (TypedGraphMorphism n e -> id -> id) -> (elem -> id) -> [elem] -> [elem] -> (Graph.Graph (Maybe n) (Maybe e) -> [elem]) -> [(elem, elem)]
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
      initX = Untyped.empty Graph.empty typeGraph
      initF' = buildTypedGraphMorphism initX typedGraphB (Untyped.empty Graph.empty (domain typedGraphB))
      initG' = buildTypedGraphMorphism initX typedGraphA (Untyped.empty Graph.empty (domain typedGraphA))

      -- Add all elements on X and their morphisms
      (g',f') = foldr updateNodes (initG',initF') nodesWithId
      (g'',f'') = foldr updateEdges (g',f') edgesWithId

      -- Add a node is just do it on the domain of f' and g'
      updateNodes ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newNode = Graph.NodeId newId
          updateG' = createNodeOnDomain newNode (nodeTypeInA (nodeId a)) (nodeId a) g'
          updateF' = createNodeOnDomain newNode (nodeTypeInB (nodeId b)) (nodeId b) f'

      -- Add an edge on the domain of f' and g'
      updateEdges ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newEdge = Graph.EdgeId newId

          -- To add an edge, their source and target nodes must be found (they already exists).
          -- It searches the node in X that is mapped by f' and g' to the same source (resp. target) node of the related edges on A and B.
          src1 =
            filter
              (\n ->
                applyNodeIdUnsafe f' n == sourceId b &&
                applyNodeIdUnsafe g' n == sourceId a)
              (nodeIdsFromDomain f')
          src = if Prelude.null src1 then error "src not found" else head src1

          tgt1 =
            filter
              (\n ->
                applyNodeIdUnsafe f' n == targetId b &&
                applyNodeIdUnsafe g' n == targetId a)
              (nodeIdsFromDomain f')
          tgt = if Prelude.null tgt1 then error "tgt not found" else head tgt1

          updateG' = createEdgeOnDomain newEdge src tgt (edgeTypeInA (edgeId a)) (edgeId a) g'
          updateF' = createEdgeOnDomain newEdge src tgt (edgeTypeInB (edgeId b)) (edgeId b) f'

type TypedNode = (NodeId,NodeId)
type TypedEdge = (EdgeId, NodeId, NodeId, EdgeId)
type RelabelFunction = (NodeId -> NodeId, EdgeId -> EdgeId)

instance Cocomplete (CatM n e) (TypedGraphMorphism n e) where
  getInitialObject = return (identity Graph.empty)

  getMorphismFromInitialObjectTo g = return $
    TypedGraphMorphism (Untyped.empty Graph.empty Graph.empty) g
      (Untyped.empty Graph.empty (untypedGraph g))

  calculateCoequalizer f g = return (calculateCoequalizer' f g)
  calculateNCoequalizer = return . calculateNCoequalizer'
  
  calculateCoproduct f g = return (calculateCoproduct' f g)

  calculateNCoproduct [] = return []
  calculateNCoproduct [x] = return [identity x]
  calculateNCoproduct (x:xs) = return $ calculateNCoproduct' (x :| xs)

  calculateNPushout [] = return []
  calculateNPushout (f:fs) = return $
    let
      (j:js) = calculateNCoproduct' (codomain f :| map codomain fs)
      k = calculateNCoequalizer' (j <&> f :| zipWith (<&>) js fs)
    in map (k<&>) (j:js)

calculateCoequalizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateCoequalizer' f g = initCoequalizerMorphism b nodeEquivalences edgeEquivalences
  where
    b = codomainGraph f
    nodeEquivalences = createNodeEquivalences f g
    edgeEquivalences = createEdgeEquivalences f g

calculateNCoequalizer' :: NonEmpty (TypedGraphMorphism a b) -> TypedGraphMorphism a b
calculateNCoequalizer' fs' = initCoequalizerMorphism b nodeEquivalences edgeEquivalences
  where
    fs = NonEmpty.toList fs'
    b = codomainGraph $ head fs
    nodeEquivalences = createNodeNEquivalences fs
    edgeEquivalences = createEdgeNEquivalences fs

-- | Given a typed graph @B@ and the sets @Ne@ and @Ee@ of node and edge equivalences
-- it returns the the skeleton of the coequalizer morphism @h : B -> X@, consisting of
-- the typed graphs @B@ and @X@ but without the morphisms between them
initCoequalizerMorphism :: TypedGraph a b -> Set (EquivalenceClass TypedNode) -> Set (EquivalenceClass TypedEdge) -> TypedGraphMorphism a b
initCoequalizerMorphism b nodeEquivalences edgeEquivalences = addEdges
  where
    x = Untyped.empty Graph.empty (typeGraph b)
    h = buildTypedGraphMorphism b x (Untyped.empty (domain b) (domain x))
    addNodes = Set.foldr addNode h nodeEquivalences
    addEdges = Set.foldr addEdge addNodes edgeEquivalences

calculateCoproduct' :: TypedGraph a b -> TypedGraph a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
calculateCoproduct' a b = (ha',hb')
  where
    coproductObject = Prelude.foldr calculateCoproductObject emptyObject maps
    emptyObject = Untyped.empty Graph.empty (typeGraph a)
    ha = buildTypedGraphMorphism a coproductObject (Untyped.empty (domain a) (domain coproductObject))
    hb = buildTypedGraphMorphism b coproductObject (Untyped.empty (domain b) (domain coproductObject))
    ha' = addCoproductMorphisms (head maps) ha
    hb' = addCoproductMorphisms (head $ tail maps) hb
    labels = relablingFunctions [a,b] (0,0) []
    maps = zip [a,b] labels

calculateNCoproduct' :: NonEmpty (TypedGraph a b) -> [TypedGraphMorphism a b]
calculateNCoproduct' gs' = zipWith addCoproductMorphisms maps allMorphisms
  where
    gs = NonEmpty.toList gs'
    tg = typeGraph (head gs)
    emptyObject = Untyped.empty Graph.empty tg
    coproductObject = Prelude.foldr calculateCoproductObject emptyObject maps
    buildMorphism graph = buildTypedGraphMorphism graph coproductObject (Untyped.empty (domain graph) (domain coproductObject))
    allMorphisms = Prelude.map buildMorphism gs
    labels = relablingFunctions gs (0,0) []
    maps = zip gs labels

addCoproductMorphisms :: (TypedGraph a b, RelabelFunction) -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addCoproductMorphisms (original, relabel) morph = addEdges
  where
    addNodes = Prelude.foldr updateN morph graphNodes
    addEdges = Prelude.foldr updateE addNodes graphEdges
    nodeName = fst relabel
    edgeName = snd relabel
    graphNodes = typedNodes original
    graphEdges = typedEdges original
    updateN (n1,t) = updateNodeRelation n1 (nodeName n1) t
    updateE (e1,_,_,_) = updateEdgeRelation e1 (edgeName e1)

calculateCoproductObject :: (TypedGraph a b, RelabelFunction) -> TypedGraph a b -> TypedGraph a b
calculateCoproductObject (original,relabel) target = addEdges
  where
    addNodes = Prelude.foldr createNewNode target newNodes
    addEdges = Prelude.foldr createNewEdge addNodes newEdges
    originalNodes = typedNodes original
    newNodes = Prelude.map newNode originalNodes
    newNode (n,nt) = (fst relabel n, nt)
    createNewNode (n,nt) = Untyped.createNodeOnDomain n nt
    originalEdges = typedEdges original
    newEdges = Prelude.map newEdge originalEdges
    newEdge (e,s,t,et) = (snd relabel e, fst relabel s, fst relabel t, et)
    createNewEdge (e,s,t,et) = Untyped.createEdgeOnDomain e s t et

relablingFunctions :: [TypedGraph a b] -> (NodeId, EdgeId) -> [RelabelFunction] -> [RelabelFunction]
relablingFunctions [] _ functions = functions
relablingFunctions (g:gs) (nodeSeed, edgeSeed) functions =
  relablingFunctions gs (nextNode g + nodeSeed, nextEdge g + edgeSeed) (functions ++ [((+) nodeSeed, (+) edgeSeed)])
  where
    ns g = Graph.nodeIds (untypedGraph g)
    es g = Graph.edgeIds (untypedGraph g)
    nextNode g = if Prelude.null (ns g) then 1 else maximum (ns g) + 1
    nextEdge g = if Prelude.null (es g) then 1 else maximum (es g) + 1

createNodeNEquivalences :: [TypedGraphMorphism a b] -> Set (EquivalenceClass TypedNode)
createNodeNEquivalences fs = nodesOnX
  where
    representant = head fs
    equivalentNodes (n,nt) = Set.fromList $ Prelude.map (\f -> (fromJust $ applyNodeId f n,nt)) fs
    nodesFromA = typedNodes (domainGraph representant)
    nodesToGluingOnB = fmap equivalentNodes nodesFromA
    initialNodesOnX = discretePartition (typedNodes (codomainGraph representant))
    nodesOnX = mergeSets nodesToGluingOnB initialNodesOnX

createEdgeNEquivalences :: [TypedGraphMorphism a b] -> Set (EquivalenceClass TypedEdge)
createEdgeNEquivalences fs = edgesOnX
  where
    representant = head fs
    equivalentEdges (e,s,t,et) = Set.fromList $ Prelude.map (\f -> (fromJust $ applyEdgeId f e, fromJust $ applyNodeId f s, fromJust $ applyNodeId f t,et)) fs
    edgesFromA = typedEdges (domainGraph representant)
    edgesToGluingOnB = fmap equivalentEdges edgesFromA
    initialEdgesOnX = discretePartition (typedEdges (codomainGraph representant))
    edgesOnX = mergeSets edgesToGluingOnB initialEdgesOnX

createNodeEquivalences :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set (EquivalenceClass TypedNode)
createNodeEquivalences f g = nodesOnX
  where
    equivalentNodes (n,nt) = ((fromJust $ applyNodeId f n,nt), (fromJust $ applyNodeId g n,nt))
    nodesFromA = typedNodes (domainGraph f)
    nodesToGluingOnB = fmap equivalentNodes nodesFromA
    initialNodesOnX = discretePartition (typedNodes (codomainGraph f))
    nodesOnX = mergePairs nodesToGluingOnB initialNodesOnX

createEdgeEquivalences :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set (EquivalenceClass TypedEdge)
createEdgeEquivalences f g = edgesOnX
  where
    equivalentEdges (e,s,t,et) =
      ((fromJust $ applyEdgeId f e, mapByF s, mapByF t,et), (fromJust $ applyEdgeId g e,mapByG s,mapByG t,et))
    mapByF = fromJust . applyNodeId f
    mapByG = fromJust . applyNodeId g
    edgesFromA = typedEdges (domainGraph f)
    edgesToGluingOnB = fmap equivalentEdges edgesFromA
    initialEdgesOnX = discretePartition (typedEdges (codomainGraph f))
    edgesOnX = mergePairs edgesToGluingOnB initialEdgesOnX

addNode :: EquivalenceClass TypedNode -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addNode nodes h
  | Set.null nodes = h
  | otherwise = buildNodeMaps (createNodeOnCodomain n2 tp h) n2 nodes
  where
    (n2, tp) = getElem nodes

buildNodeMaps :: TypedGraphMorphism a b -> NodeId -> EquivalenceClass TypedNode -> TypedGraphMorphism a b
buildNodeMaps h nodeInX nodes
  | Set.null nodes = h
  | otherwise = buildNodeMaps h' nodeInX nodes'
    where
      (nodeInA, tp) = getElem nodes
      h' = updateNodeRelation nodeInA nodeInX tp h
      nodes' = getTail nodes

addEdge :: EquivalenceClass TypedEdge -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addEdge edges h
  | Set.null edges = h
  | otherwise = buildEdgeMaps (createEdgeOnCodomain e2 s2 t2 tp h) e2 edges
  where
    (e2, s, t, tp) = getElem edges
    s2 = fromJust $ applyNodeId h s
    t2 = fromJust $ applyNodeId h t

buildEdgeMaps :: TypedGraphMorphism a b -> EdgeId -> EquivalenceClass TypedEdge -> TypedGraphMorphism a b
buildEdgeMaps h edgeInX edges
  | Set.null edges = h
  | otherwise = buildEdgeMaps h' edgeInX edges'
    where
      (edgeInA, _, _, _) = getElem edges
      h' = updateEdgeRelation edgeInA edgeInX h
      edges' = getTail edges
