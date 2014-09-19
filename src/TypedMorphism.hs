{-# LANGUAGE TypeFamilies #-}

module TypedMorphism (TypedMorphism) where

import Graph (Graph)
import TypedMorphismClass
import qualified MorphismClass as M
import Morphism
import Valid

data TypedMorphism a b = TypedMorphism {
                              getDomain   :: Morphism a b
                            , getCodomain :: Morphism a b
                            , getMapping  :: Morphism a b
                         }

instance TypedMorphismClass (TypedMorphism a b) where
    type M (TypedMorphism a b) = Morphism a b

    domain t   = getDomain t
    codomain t = getCodomain t
    mapping t  = getMapping t

    typedMorphism = TypedMorphism

instance (Eq a, Eq b) => Valid (TypedMorphism a b) where
    valid t = let dom = domain t
                  cod = codomain t
              in valid dom &&
                 valid cod &&
                 M.image dom == (M.image $ (M.compose (mapping t) cod))
        
