module Morphism where

import Relation

class Morphism m where
  data obj :: *
  compose  :: m -> m -> m 
  (==)     :: m -> m -> Bool
  domain   :: m -> m -> obj 
  codomain :: m -> a -> obj



