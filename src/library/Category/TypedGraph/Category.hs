{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraph.Category
    ( TypedGraph
    , TypedGraphMorphism
    , MorphismType(..)
    , toMorphismClass
    , toMorphismType
    ) where

import           Abstract.Category
import           Category.Graph                     (MorphismType(..))
import qualified Category.Graph                     as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

instance Category (TypedGraphMorphism n e) where
    type Obj (TypedGraphMorphism n e) = TypedGraph n e

    domain = domainGraph
    codomain = codomainGraph
    t2 <&> t1 = compose t2 t1
    identity t = TypedGraphMorphism t t (identity $ domain t)

    newtype MorphismClass (TypedGraphMorphism n e) =
        Cls { toMorphismType :: MorphismType } 
        deriving Eq

    anyMorphism = Cls AnyMorphism
    monic = Cls Monomorphism
    epic = Cls Epimorphism
    iso = Cls Isomorphism

    f `belongsToClass` (Cls c) = mapping f `belongsToClass` Graph.toMorphismClass c
    Cls c1 `isSubclassOf` Cls c2 = Graph.toMorphismClass c1 `isSubclassOf` Graph.toMorphismClass c2

toMorphismClass :: MorphismType -> MorphismClass (TypedGraphMorphism n e)
toMorphismClass = Cls