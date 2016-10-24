{-# OPTIONS_GHC -Wno-orphans #-}
module TypedGraph.Morphism.Cocomplete (

  calculateCoequalizer,createNodeEquivalences

)

where

import Abstract.Cocomplete
import Abstract.Morphism
import Data.Set
import           Graph.Graph         as G
import qualified Graph.GraphMorphism as GM
import           TypedGraph.Graph
import           TypedGraph.Morphism.Core

instance Cocomplete (TypedGraphMorphism a b) where

  calculateCoequalizer = calculateCoEq'

calculateCoEq' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateCoEq' f g = initCoequalizerMorphism b nodeEquivalences edgeEquivalences
  where
    b = getCodomain f
    nodeEquivalences = createNodeEquivalences f g
    edgeEquivalences = createEdgeEquivalences f g

createNodeEquivalences :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set (EquivalenceClass TypedNode)
createNodeEquivalences f g = nodesOnX
  where
    equivalentNodes (n,nt) = ((applyNodeUnsafe f n,nt), (applyNodeUnsafe g n,nt))
    nodesFromA = fromList $ nodesWithType (getDomain f)
    nodesToGluingOnB = Data.Set.map equivalentNodes nodesFromA
    initialNodesOnX = maximumDisjointClass (nodesWithType (getCodomain f))
    nodesOnX = construct nodesToGluingOnB initialNodesOnX

createEdgeEquivalences :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set (EquivalenceClass TypedEdge)
createEdgeEquivalences f g = edgesOnX
  where
    equivalentEdges (e,s,t,et) =
      ((applyEdgeUnsafe f e, mapByF s, mapByF t,et), (applyEdgeUnsafe g e,mapByG s,mapByG t,et))
    mapByF = applyNodeUnsafe f
    mapByG = applyNodeUnsafe g
    edgesFromA = fromList $ edgesWithType (getDomain f)
    edgesToGluingOnB = Data.Set.map equivalentEdges edgesFromA
    initialEdgesOnX = maximumDisjointClass (edgesWithType (getCodomain f))
    edgesOnX = construct edgesToGluingOnB initialEdgesOnX

initCoequalizerMorphism :: TypedGraph a b -> Set (EquivalenceClass TypedNode) -> Set (EquivalenceClass TypedEdge) -> TypedGraphMorphism a b
initCoequalizerMorphism b nodeEquivalences edgeEquivalences = addEdges
  where
    x = GM.empty G.empty (typeGraph b)
    h = buildTypedGraphMorphism b x (GM.empty (domain b) (domain x))
    addNodes = Data.Set.foldr addNode h nodeEquivalences
    addEdges = Data.Set.foldr addEdge addNodes edgeEquivalences

addNode :: EquivalenceClass TypedNode -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addNode nodes h
  | Data.Set.null nodes = h
  | otherwise = buildNodeMaps (createNodeOnCodomain n2 tp h) n2 nodes
  where
    (n2, tp) = getElem nodes

buildNodeMaps :: TypedGraphMorphism a b -> NodeId -> EquivalenceClass TypedNode -> TypedGraphMorphism a b
buildNodeMaps h nodeInX nodes
  | Data.Set.null nodes = h
  | otherwise = buildNodeMaps h' nodeInX nodes'
    where
      (nodeInA, tp) = getElem nodes
      h' = updateNodeRelation nodeInA nodeInX tp h
      nodes' = setTail nodes

addEdge :: EquivalenceClass TypedEdge -> TypedGraphMorphism a b -> TypedGraphMorphism a b
addEdge edges h
  | Data.Set.null edges = h
  | otherwise = buildEdgeMaps (createEdgeOnCodomain e2 s2 t2 tp h) e2 edges
  where
    (e2, s, t, tp) = getElem edges
    s2 = applyNodeUnsafe h s
    t2 = applyNodeUnsafe h t

buildEdgeMaps :: TypedGraphMorphism a b -> EdgeId -> EquivalenceClass TypedEdge -> TypedGraphMorphism a b
buildEdgeMaps h edgeInX edges
  | Data.Set.null edges = h
  | otherwise = buildEdgeMaps h' edgeInX edges'
    where
      (edgeInA, _, _, _) = getElem edges
      h' = updateEdgeRelation edgeInA edgeInX h
      edges' = setTail edges

maximumDisjointClass :: (Ord a) => [a] -> Set (EquivalenceClass a)
maximumDisjointClass l = fromList $ Prelude.map (fromList . (:[])) l

type EquivalenceClass a = Set a
type TypedNode = (NodeId,NodeId)
type TypedEdge = (EdgeId, NodeId, NodeId, EdgeId)

construct :: (Ord a, Show a) => Set(a,a) -> Set (EquivalenceClass a) -> Set (EquivalenceClass a)
construct toBeGlued toBeX
  | Data.Set.null toBeGlued = toBeX
  | otherwise = construct (setTail toBeGlued) (merge (getElem toBeGlued) toBeX)
  where
    merge (e1,e2) s =  mergeEquivalences (e1,e2) s `union` (s `diff` (e1,e2))
    diff s (e1,e2) = if e1 == e2 then
        s `difference` singleton (findEquivalenceClass e1 s)
      else
        s `difference` singleton (findEquivalenceClass e1 s) `difference` singleton (findEquivalenceClass e2 s)

getElem :: Set a -> a
getElem = elemAt 0

getUnitSubset :: Set a -> Set a
getUnitSubset set = singleton (getElem set)

setTail :: (Ord a) => Set a -> Set a
setTail set = set `difference` getUnitSubset set

mergeEquivalences :: (Ord a, Show a) => (a, a) -> Set(EquivalenceClass a) -> Set(EquivalenceClass a)
mergeEquivalences (e1,e2) set = singleton (findEquivalenceClass e1 set `union` findEquivalenceClass e2 set)

-- works only with non-empty sets
findEquivalenceClass :: (Eq a, Show a) => a -> Set(EquivalenceClass a) -> EquivalenceClass a
findEquivalenceClass element set
  | Data.Set.null teste = error $ show element ++ show set
  | otherwise = getElem teste
  where
    teste = Data.Set.filter (element `elem`) set
