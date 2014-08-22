{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module TypedMorphismClass where

class MorphismClass (M t) => TypedMorphism t 
    type M t :: *

    typedDomain   :: t -> (M t)
    typedCodomain :: t -> (M t)
    typedMapping  :: t -> (M t)
