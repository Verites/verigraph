{-# LANGUAGE TypeFamilies #-}

module TypedMorphism
    ( TypedMorphism
    , TypedGraph
    ) where

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

    typedDomain m   = getDomain m
    typedCodomain m = getCodomain m
    typedMapping m  = getMapping m
