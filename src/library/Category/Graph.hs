{-# LANGUAGE TypeFamilies #-}

module Category.Graph
(
) where


import           Abstract.Category.FinitaryCategory
import qualified Data.Relation             as R
import           Morphism.Graph
import           Data.Graphs               as G

instance FinitaryCategory (GraphMorphism a b) where
    type Obj (GraphMorphism a b) = Graph a b

    domain = domainGraph
    codomain = codomainGraph
    m2 <&> m1 = GraphMorphism (domain m1)
                  (codomain m2)
                  (R.compose (nodeRelation m1) (nodeRelation m2))
                  (R.compose (edgeRelation m1) (edgeRelation m2))

    identity g = GraphMorphism g g (R.id $ nodeIds g) (R.id $ edgeIds g)
    isMonomorphism m =
        R.isInjective (nodeRelation m) &&
        R.isInjective (edgeRelation m)
    isEpimorphism m =
        R.isSurjective (nodeRelation m) &&
        R.isSurjective (edgeRelation m)
    isIsomorphism m =
        isMonomorphism m && isEpimorphism m
