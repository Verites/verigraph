{-# LANGUAGE TypeFamilies #-}

module Category.Graph
(
) where


import           Abstract.Category.FinitaryCategory
import qualified Data.Relation             as R
import           Data.Graphs.Morphism
import           Data.Graphs               as G

instance FinitaryCategory (GraphMorphism a b) where
    type Obj (GraphMorphism a b) = Graph a b

    domain = domainGraph
    codomain = codomainGraph
    m2 <&> m1 = compose m2 m1

    identity g = GraphMorphism g g (R.id $ nodeIds g) (R.id $ edgeIds g)
    isMonomorphism m =
        R.isInjective (nodeRelation m) &&
        R.isInjective (edgeRelation m)
    isEpimorphism m =
        R.isSurjective (nodeRelation m) &&
        R.isSurjective (edgeRelation m)
    isIsomorphism m =
        isMonomorphism m && isEpimorphism m
