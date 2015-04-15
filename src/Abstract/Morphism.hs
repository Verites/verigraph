{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Abstract.Morphism where

class (Eq m) => Morphism m where
    type Obj m :: *
    compose  :: m -> m -> m 
    domain   :: m -> Obj m
    codomain :: m -> Obj m
    id       :: Obj m -> m
    monomorphism :: m -> Bool
    epimorphism :: m -> Bool
    isomorphism :: m -> Bool



