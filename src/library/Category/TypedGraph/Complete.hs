module Category.TypedGraph.Complete where

import Abstract.Category.Complete
import Category.TypedGraph
import Data.List
import Data.Graphs.Morphism as GM hiding (domainGraph, applyEdgeIdUnsafe, applyNodeIdUnsafe)
import Data.TypedGraph.Morphism hiding (removeNodeFromDomain, removeEdgeFromDomain)


instance Complete (TypedGraphMorphism a b) where

  calculateEqualizer = calculateEqualizer'

calculateEqualizer' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateEqualizer' f g = idMap typedX typedA
  where
    fNodes = nodeIdsFromDomain f
    fEdges = edgeIdsFromDomain f
    typedA = domainGraph f
    equivalentNodes = filter (\n -> applyNodeIdUnsafe f n == applyNodeIdUnsafe g n) fNodes
    equivalentEdges = filter (\e -> applyEdgeIdUnsafe f e == applyEdgeIdUnsafe g e) fEdges
    typedX = foldr removeNodeFromDomain
                  (foldr removeEdgeFromDomain typedA (fEdges \\ equivalentEdges))
                  (fNodes \\ equivalentNodes)
