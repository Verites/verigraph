{-# LANGUAGE TypeFamilies #-}

module Category.Graph
(  Graph
,  GraphMorphism
,  MorphismType(..)
,  toMorphismClass
,  toMorphismType
) where


import           Abstract.Category
import           Data.Graphs          as G
import           Data.Graphs.Morphism
import qualified Data.Relation        as R


data MorphismType =
    AnyMorphism | Monomorphism | Epimorphism | Isomorphism
    deriving Eq

toMorphismClass :: MorphismType -> MorphismClass (GraphMorphism n e)
toMorphismClass = Cls


instance Category (GraphMorphism n e) where
    type Obj (GraphMorphism n e) = Graph n e

    domain = domainGraph
    codomain = codomainGraph
    m2 <&> m1 = compose m2 m1

    identity g = GraphMorphism g g (R.id $ nodeIds g) (R.id $ edgeIds g)

    newtype MorphismClass (GraphMorphism n e) =
        Cls { toMorphismType :: MorphismType }
        deriving Eq

    anyMorphism = Cls AnyMorphism
    monic = Cls Monomorphism
    epic = Cls Epimorphism
    iso = Cls Isomorphism

    _ `belongsToClass` Cls AnyMorphism = True

    m `belongsToClass` Cls Monomorphism =
        R.isInjective (nodeRelation m) &&
        R.isInjective (edgeRelation m)

    m `belongsToClass` Cls Epimorphism =
        R.isSurjective (nodeRelation m) &&
        R.isSurjective (edgeRelation m)

    m `belongsToClass` Cls Isomorphism =
        m `belongsToClass` Cls Monomorphism && m `belongsToClass` Cls Epimorphism

    _ `isSubclassOf` Cls AnyMorphism = True
    Cls Monomorphism `isSubclassOf` Cls Monomorphism = True
    Cls Epimorphism `isSubclassOf` Cls Epimorphism = True
    Cls Isomorphism `isSubclassOf` Cls Monomorphism = True
    Cls Isomorphism `isSubclassOf` Cls Epimorphism = True
    Cls Isomorphism `isSubclassOf` Cls Isomorphism = True
    _ `isSubclassOf` _ = False
