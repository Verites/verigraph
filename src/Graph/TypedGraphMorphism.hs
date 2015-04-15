{-# LANGUAGE TypeFamilies #-}

module Graph.TypedGraphMorphism (
      mapping
    , typedMorphism
    , TypedGraphMorphism
) where

import Graph.Graph (Graph)
import Graph.GraphMorphism
import Abstract.Morphism as M
import Abstract.Valid

data TypedGraphMorphism a b = TypedGraphMorphism {
                              getDomain   :: GraphMorphism a b
                            , getCodomain :: GraphMorphism a b
                            , getMapping  :: GraphMorphism a b
                         } deriving (Show, Read)

typedMorphism = TypedGraphMorphism
mapping = getMapping

instance Eq (TypedGraphMorphism a b) where
    (TypedGraphMorphism dom1 cod1 m1) == (TypedGraphMorphism dom2 cod2 m2) =
        dom1 == dom2 &&
        cod1 == cod2 &&
        m1 == m2

instance Morphism (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = GraphMorphism a b

    domain = getDomain
    codomain = getCodomain
    compose t1 t2 =
        TypedGraphMorphism (domain t1)
                      (codomain t2)
                      $ compose (getMapping t1)
                                (getMapping t2)
    id t = TypedGraphMorphism t t (M.id $ domain t)
    monomorphism = monomorphism . mapping
    epimorphism = epimorphism . mapping
    isomorphism = isomorphism . mapping


instance Valid (TypedGraphMorphism a b) where
    valid (TypedGraphMorphism dom cod m) =
        valid dom &&
        valid cod &&
        dom == compose m cod
        
