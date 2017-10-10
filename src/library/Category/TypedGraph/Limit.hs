module Category.TypedGraph.Limit () where

import           Abstract.Category
import           Abstract.Category.Limit
import           Category.TypedGraph.Category
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import qualified Data.List.NonEmpty                 as NE
import           Data.List ((\\))
import           Data.Maybe                         (fromJust)
import           Data.Partition
import           Data.Set                           (Set)
import qualified Data.Set                           as DS
import           Data.TypedGraph
import           Data.TypedGraph.Morphism 


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
  -- identifing them with ids.
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

      nodesInA = nodesFromDomain f
      nodesInB = nodesFromDomain g
      edgesInA = edgesFromDomain f
      edgesInB = edgesFromDomain g

      -- Discover the nodes and edges of the X
      nodesWithoutId = getPairs applyNodeIdUnsafe nodeId nodesInA nodesInB nodes
      nodesWithId = zip nodesWithoutId ([0..]::[Int])

      egdesWithoutId = getPairs applyEdgeIdUnsafe edgeId edgesInA edgesInB edges
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
      initX = GM.empty empty typeGraph
      initF' = buildTypedGraphMorphism initX typedGraphB (GM.empty empty (domain typedGraphB))
      initG' = buildTypedGraphMorphism initX typedGraphA (GM.empty empty (domain typedGraphA))

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

  calculateEqualizer = calculateEqualizer'

calculateEqualizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateEqualizer' f g = makeInclusion typedX typedA
  where
    fNodes = nodeIdsFromDomain f
    fEdges = edgeIdsFromDomain f
    typedA = domainGraph f
    equivalentNodes = filter (\n -> applyNodeIdUnsafe f n == applyNodeIdUnsafe g n) fNodes
    equivalentEdges = filter (\e -> applyEdgeIdUnsafe f e == applyEdgeIdUnsafe g e) fEdges
    typedX = foldr GM.removeNodeFromDomain
                  (foldr GM.removeEdgeFromDomain typedA (fEdges \\ equivalentEdges))
                  (fNodes \\ equivalentNodes)
  


type TypedNode = (NodeId,NodeId)
type TypedEdge = (EdgeId, NodeId, NodeId, EdgeId)
type RelabelFunction = (NodeId -> NodeId, EdgeId -> EdgeId)

instance Cocomplete (TypedGraphMorphism a b) where
  calculateCoequalizer = calculateCoequalizer'
  calculateNCoequalizer = calculateNCoequalizer'
  calculateCoproduct = calculateCoproduct'
  calculateNCoproduct = calculateNCoproduct'
  initialObject = initialObject'

initialObject' :: TypedGraphMorphism a b -> TypedGraph a b
initialObject' tgm = GM.empty G.empty (typeGraph (domainGraph tgm))

calculateCoequalizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateCoequalizer' f g = initCoequalizerMorphism b nodeEquivalences edgeEquivalences
  where
    b = codomainGraph f
    nodeEquivalences = createNodeEquivalences f g
    edgeEquivalences = createEdgeEquivalences f g

calculateNCoequalizer' :: NE.NonEmpty (TypedGraphMorphism a b) -> TypedGraphMorphism a b
calculateNCoequalizer' fs' = initCoequalizerMorphism b nodeEquivalences edgeEquivalences
  where
    fs = NE.toList fs'
    b = codomainGraph $ head fs
    nodeEquivalences = createNodeNEquivalences fs
    edgeEquivalences = createEdgeNEquivalences fs

-- | Given a typed graph @B@ and the sets @Ne@ and @Ee@ of node and edge equivalences
-- it returns the the skeleton of the coequalizer morphism @h : B -> X@, consisting of
-- the typed graphs @B@ and @X@ but without the morphisms between them
initCoequalizerMorphism :: TypedGraph a b -> Set (EquivalenceClass TypedNode) -> Set (EquivalenceClass TypedEdge) -> TypedGraphMorphism a b
initCoequalizerMorphism b nodeEquivalences edgeEquivalences = addEdges
  where
    x = GM.empty G.empty (typeGraph b)
    h = buildTypedGraphMorphism b x (GM.empty (domain b) (domain x))
    addNodes = DS.foldr addNode h nodeEquivalences
    addEdges = DS.foldr addEdge addNodes edgeEquivalences

calculateCoproduct' :: TypedGraph a b -> TypedGraph a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
calculateCoproduct' a b = (ha',hb')
  where
    coproductObject = Prelude.foldr calculateCoproductObject emptyObject maps
    emptyObject = GM.empty G.empty (typeGraph a)
    ha = buildTypedGraphMorphism a coproductObject (GM.empty (domain a) (domain coproductObject))
    hb = buildTypedGraphMorphism b coproductObject (GM.empty (domain b) (domain coproductObject))
    ha' = addCoproductMorphisms (head maps) ha
    hb' = addCoproductMorphisms (head $ tail maps) hb
    labels = relablingFunctions [a,b] (0,0) []
    maps = zip [a,b] labels

calculateNCoproduct' :: NE.NonEmpty (TypedGraph a b) -> [TypedGraphMorphism a b]
calculateNCoproduct' gs' = zipWith addCoproductMorphisms maps allMorphisms
  where
    gs = NE.toList gs'
    tg = typeGraph (head gs)
    emptyObject = GM.empty G.empty tg
    coproductObject = Prelude.foldr calculateCoproductObject emptyObject maps
    buildMorphism graph = buildTypedGraphMorphism graph coproductObject (GM.empty (domain graph) (domain coproductObject))
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
    createNewNode (n,nt) = GM.createNodeOnDomain n nt
    originalEdges = typedEdges original
    newEdges = Prelude.map newEdge originalEdges
    newEdge (e,s,t,et) = (snd relabel e, fst relabel s, fst relabel t, et)
    createNewEdge (e,s,t,et) = GM.createEdgeOnDomain e s t et

relablingFunctions :: [TypedGraph a b] -> (NodeId, EdgeId) -> [RelabelFunction] -> [RelabelFunction]
relablingFunctions [] _ functions = functions
relablingFunctions (g:gs) (nodeSeed, edgeSeed) functions =
  relablingFunctions gs (nextNode g + nodeSeed, nextEdge g + edgeSeed) (functions ++ [((+) nodeSeed, (+) edgeSeed)])
  where
    ns g = nodeIds (untypedGraph g)
    es g = edgeIds (untypedGraph g)
    nextNode g = if Prelude.null (ns g) then 1 else maximum (ns g) + 1
    nextEdge g = if Prelude.null (es g) then 1 else maximum (es g) + 1

createNodeNEquivalences :: [TypedGraphMorphism a b] -> Set (EquivalenceClass TypedNode)
createNodeNEquivalences fs = nodesOnX
  where
    representant = head fs
    equivalentNodes (n,nt) = DS.fromList $ Prelude.map (\f -> (fromJust $ applyNodeId f n,nt)) fs
    nodesFromA = typedNodes (domainGraph representant)
    nodesToGluingOnB = fmap equivalentNodes nodesFromA
    initialNodesOnX = discretePartition (typedNodes (codomainGraph representant))
    nodesOnX = mergeSets nodesToGluingOnB initialNodesOnX

createEdgeNEquivalences :: [TypedGraphMorphism a b] -> Set (EquivalenceClass TypedEdge)
createEdgeNEquivalences fs = edgesOnX
  where
    representant = head fs
    equivalentEdges (e,s,t,et) = DS.fromList $ Prelude.map (\f -> (fromJust $ applyEdgeId f e, fromJust $ applyNodeId f s, fromJust $ applyNodeId f t,et)) fs
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
  | DS.null nodes = h
  | otherwise = buildNodeMaps (createNodeOnCodomain n2 tp h) n2 nodes
  where
    (n2, tp) = getElem nodes

buildNodeMaps :: TypedGraphMorphism a b -> NodeId -> EquivalenceClass TypedNode -> TypedGraphMorphism a b
buildNodeMaps h nodeInX nodes
  | DS.null nodes = h
  | otherwise = buildNodeMaps h' nodeInX nodes'
    where
      (nodeInA, tp) = getElem nodes
      h' = updateNodeRelation nodeInA nodeInX tp h
      nodes' = getTail nodes

addEdge :: EquivalenceClass TypedEdge -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addEdge edges h
  | DS.null edges = h
  | otherwise = buildEdgeMaps (createEdgeOnCodomain e2 s2 t2 tp h) e2 edges
  where
    (e2, s, t, tp) = getElem edges
    s2 = fromJust $ applyNodeId h s
    t2 = fromJust $ applyNodeId h t

buildEdgeMaps :: TypedGraphMorphism a b -> EdgeId -> EquivalenceClass TypedEdge -> TypedGraphMorphism a b
buildEdgeMaps h edgeInX edges
  | DS.null edges = h
  | otherwise = buildEdgeMaps h' edgeInX edges'
    where
      (edgeInA, _, _, _) = getElem edges
      h' = updateEdgeRelation edgeInA edgeInX h
      edges' = getTail edges
