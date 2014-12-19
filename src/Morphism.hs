module Morphism where

import qualified Relation as R

class Morphism m where
    data obj :: *
    compose  :: m -> m -> m 
    (==)     :: m -> m -> Bool
    domain   :: m -> [obj]
    codomain :: m -> [obj]
    id       :: obj -> m
    monomorphism :: m -> Bool
    epimorphism :: m -> Bool
    isomorphism :: m -> Bool



