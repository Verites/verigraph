{-# LANGUAGE TypeFamilies #-}

module TypedMorphism (
      mapping
    , typedMorphism
    , TypedMorphism
) where

import Graph (Graph)
import GraphMorphism
import Morphism
import Valid

data TypedMorphism a b = TypedMorphism {
                              getDomain   :: GraphMorphism a b
                            , getCodomain :: GraphMorphism a b
                            , getMapping  :: GraphMorphism a b
                         }

typedMorphism = TypedMorphism
mapping = getMapping

instance (Eq a, Eq b) => Eq (TypedMorphism a b) where
    (TypedMorphism dom1 cod1 m1) == (TypedMorphism dom2 cod2 m2) =
        dom1 == dom2 &&
        cod1 == cod2 &&
        m1 == m2

instance (Eq a, Eq b) => Morphism (TypedMorphism a b) where
    type Obj (TypedMorphism a b) = GraphMorphism a b

    domain = getDomain
    codomain = getCodomain
    compose t1 t2 =
        TypedMorphism (domain t1)
                      (codomain t2)
                      $ compose (getMapping t1)
                                (getMapping t2)
    id t = TypedMorphism t t (Morphism.id $ domain t)
    monomorphism = monomorphism . mapping
    epimorphism = epimorphism . mapping
    isomorphism = isomorphism . mapping


instance (Eq a, Eq b) => Valid (TypedMorphism a b) where
    valid (TypedMorphism dom cod m) =
        valid dom &&
        valid cod &&
        dom == compose m cod
        
