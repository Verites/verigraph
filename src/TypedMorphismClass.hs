{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module TypedMorphismClass where

import MorphismClass

class MorphismClass (M t) => TypedMorphismClass t where
    type M t :: *

    domain   :: t -> (M t)
    codomain :: t -> (M t)
    mapping  :: t -> (M t)

    typedMorphism :: M t -> M t -> M t -> t
