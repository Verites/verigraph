module Category.TypedGraph.Complete where

import Abstract.Category.FinitaryCategory
import Abstract.Category.Complete
import Base.Valid
import Category.TypedGraph
import Data.List
import qualified Data.Set as S
import Data.Graphs.Morphism as GM hiding (domainGraph, applyEdgeId, applyNodeId)
import Data.TypedGraph as TG
import Data.TypedGraph.Morphism as TGM hiding (removeNodeFromDomain, removeEdgeFromDomain)


instance Complete (TypedGraphMorphism a b) where

  calculateEqualizer = calculateEqualizer'
  calculateProduct = calculateProduct'
  calculateNEqualizer = error "not implemented yet"
  calculateNProduct = error "not implemented yet"
  finalObject = error "not implemented yet"
  calculatePullback = calculatePullback'

calculatePullback' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
calculatePullback' f g = if (isValid $ g' <&> h) then (g' <&> h,f' <&> h) else error ""
  where
    b = domain f
    c = domain g
    (g',f') = calculateProduct b c
    fg' = f <&> g'
    gf' = g <&> f'
    h = calculateEqualizer fg' gf'

calculateEqualizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateEqualizer' f g = idMap typedX typedA
  where
    fNodes = nodeIdsFromDomain f
    fEdges = edgeIdsFromDomain f
    typedA = domainGraph f
    equivalentNodes = filter (\n -> applyNodeId f n == applyNodeId g n) fNodes
    equivalentEdges = filter (\e -> applyEdgeId f e == applyEdgeId g e) fEdges
    edgesDifference = fEdges \\ equivalentEdges
    nodesDifference = fNodes \\ equivalentNodes
    typedX' = foldr removeEdgeFromDomain typedA  edgesDifference
    typedX  = foldr removeNodeFromDomain typedX' nodesDifference

calculateProduct' :: TypedGraph a b -> TypedGraph a b -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
calculateProduct' a b = (pa',pb')
  where
    (nodePairs,edgePairs) = nodesAndEdgesPairs a b
    tp = typeGraph a
    ns = zip (S.toList nodePairs) [0..]
    es = zip (S.toList edgePairs) [0..]
    object = TG.empty tp
    untypedObject = untypedGraph object
    untypedA = untypedGraph a
    untypedB = untypedGraph b
    pa = buildTypedGraphMorphism object a (GM.empty untypedObject untypedA)
    pb = buildTypedGraphMorphism object b (GM.empty untypedObject untypedB)

    insertNodesA ((x,_),n) = TGM.updateNodeRelation n x (extractNodeType a x)
    insertNodesB ((_,y),n) = TGM.updateNodeRelation n y (extractNodeType b y)
    insertEdgesA ((x,_),e) = TGM.createEdgeOnDomain e (sourceOf a x) (targetOf a x) (extractEdgeType a x) x
    insertEdgesB ((_,y),e) = TGM.createEdgeOnDomain e (sourceOf b y) (targetOf b y) (extractEdgeType b y) y
    pa' = foldr insertEdgesA (foldr insertNodesA pa ns) es
    pb' = foldr insertEdgesB (foldr insertNodesB pb ns) es

--nodesAndEdgesPairs :: TypedGraph a b -> TypedGraph a b -> (S.Set (NodeId, NodeId), S.Set (EdgeId, EdgeId))
nodesAndEdgesPairs a b = (nodePairs,edgePairs)
  where
    nas = typedNodes a
    nbs = typedNodes b
    eas = typedEdges a
    ebs = typedEdges b
    nodePairs = S.fromList [(x,y) | (x,tx) <- nas, (y,ty) <- nbs, tx == ty]
    edgePairs = S.fromList [(x,y) | (x,sx,tx,tpx) <- eas, (y,sy,ty,tpy) <- ebs, tpx == tpy,
                            (sx, sy) `elem` nodePairs, (tx, ty) `elem` nodePairs]
