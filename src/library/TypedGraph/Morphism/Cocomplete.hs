module TypedGraph.Morphism.Cocomplete (

  createEquivalenceNodes, construct, findEquivalenceClass, mergeEquivalences

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
calculateCoEq' f g = g
  where
    objectX = getCodomain g
    nodesOfX = nodesWithType objectX
    edgesOfX = edgesWithType objectX

createEquivalenceNodes :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set (EquivalenceClass TypedNode)
createEquivalenceNodes f g = initialNodesOnX
  where
    equivalentNodes (n,nt) = ((applyNodeUnsafe f n,nt), (applyNodeUnsafe g n,nt)) -- ((nodeAndTypeFromA),(nodeandTypeFromB))
    equivalentEdges (e,s,t,et) = ((applyEdgeUnsafe f e,s,t,et), (applyEdgeUnsafe g e,s,t,et))
    nodesFromA = fromList $ nodesWithType (getDomain f)            -- (nodeId,typeNodeId) from A
    edgesFromA = fromList $ edgesWithType (getDomain f)
    nodesToGluingOnB = Data.Set.map equivalentNodes nodesFromA
    edgesToGluingOnB = Data.Set.map equivalentEdges edgesFromA
    initialNodesOnX = maximumDisjointClass (nodesWithType (getCodomain f)) -- (nodeId, typeNodeId) from B
    initialEdgesOnX = maximumDisjointClass (edgesWithType (getCodomain f))
    nodesOnX = construct nodesToGluingOnB initialNodesOnX
    edgesOnX = construct edgesToGluingOnB initialEdgesOnX
    tG = typeGraph (getDomain g)
    newX = buildTypedGraphMorphism (getDomain g)

createCoequalizerObject :: TypedGraph a b -> Set (EquivalenceClass TypedNode) -> Set (EquivalenceClass TypedEdge) -> TypedGraphMorphism a b
createCoequalizerObject b nodeEquivalences edgeEquivalences = h
  where
    h = buildTypedGraphMorphism b x (GM.empty (domain b) (domain x))
    x = GM.empty G.empty (typeGraph b)

add :: TypedGraphMorphism a b -> EquivalenceClass TypedNode -> TypedGraphMorphism a b
add h nodes = createNodeOnCodomain n2 tp h
  where
    (n2, tp) = getElem nodes


maximumDisjointClass :: (Ord a) => [a] -> Set (EquivalenceClass a)
maximumDisjointClass l = fromList $ Prelude.map (fromList . (:[])) l

type EquivalenceClass a = Set a
type TypedNode = (NodeId,NodeId)
type TypedEdge = (EdgeId, NodeId, NodeId, EdgeId)

construct :: (Ord a) => Set(a,a) -> Set (EquivalenceClass a) -> Set (EquivalenceClass a)
construct toBeGlued toBeX
  | Data.Set.null toBeGlued = toBeX
  | otherwise = construct (setTail toBeGlued) (merge (getElem toBeGlued) toBeX)
  where
    merge (e1,e2) s =  s `diff` e1 `diff` e2 `union` mergeEquivalences (e1,e2) s
    diff s e = difference s (singleton $ findEquivalenceClass e s)

getElem :: Set a -> a
getElem = elemAt 0

getUnitSubset :: Set a -> Set a
getUnitSubset set = singleton (getElem set)

setTail :: (Ord a) => Set a -> Set a
setTail set = set `difference` getUnitSubset set

mergeEquivalences :: (Ord a) => (a, a) -> Set(EquivalenceClass a) -> Set(EquivalenceClass a)
mergeEquivalences (e1,e2) set = singleton(findEquivalenceClass e1 set `union` findEquivalenceClass e2 set)

-- works only with non-empty sets
findEquivalenceClass :: (Eq a) => a -> Set(EquivalenceClass a) -> EquivalenceClass a
findEquivalenceClass element set = getElem $ Data.Set.filter (element `elem`) set
