{-# LANGUAGE TypeFamilies #-}

module TypedMorphism (TypedMorphism) where

import TypedMorphismClass
import MorphismClass
import Morphism

data TypedMorphism a b = TypedMorphism {
                              getDomain   :: Morphism a b
                            , getCodomain :: Morphism a b
                            , getMapping  :: Morphism a b
                         }

instance TypedMorphismClass (TypedMorphism a b) where
    type M (TypedMorphism a b) = Morphism a b

    domain m   = getDomain m
    codomain m = getCodomain m
    mapping m  = getMapping m

    typedMorphism = TypedMorphism
