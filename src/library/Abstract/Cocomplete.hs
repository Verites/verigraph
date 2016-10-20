module Abstract.Cocomplete (

  Cocomplete(..)

)

where

import           Abstract.Morphism

class (Morphism m) => Cocomplete m where
  -- | f : A -> B, g : A -> B, h : B -> X
  calculateCoequalizer :: m -> m -> m

  --calculateCoproduct :: m -> m -> (m,m)
