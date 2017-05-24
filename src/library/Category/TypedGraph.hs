{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraph
(  TypedGraph
,  TypedGraphMorphism
)  where

import           Abstract.Category.FinitaryCategory
import           Category.Graph                     ()
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

instance FinitaryCategory (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = TypedGraph a b

    domain = domainGraph
    codomain = codomainGraph
    t2 <&> t1 = compose t2 t1
    identity t = TypedGraphMorphism t t (identity $ domain t)
    isMonomorphism = isMonomorphism . mapping
    isEpimorphism = isEpimorphism . mapping
    isIsomorphism = isIsomorphism . mapping
