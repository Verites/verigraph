{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module TypedMorphismClass where

import MorphismClass

class MorphismClass (M t) => TypedMorphismClass t where
    type M t :: *

    typedDomain   :: t -> (M t)
    typedCodomain :: t -> (M t)
    typedMapping  :: t -> (M t)
