module Abstract.AdhesiveHLR
  ( Morphism(..)
  , AdhesiveHLR(..)
  ) where

import Abstract.Morphism

class Morphism m => AdhesiveHLR m where
  -- | Pushout : D <-k- K,   K -r-> R ,  ====>  G' <-m'- R  (comatch)
  -- TODO: review docs
  po :: m -> m -> (m, m)

  -- | Pushout complement:  G <-m- L,  L <-l- K,  ====>   (D <-k- K, G <-l'- D)
  -- this code assumes that l is injective
  --
  -- TODO: review docs
  poc :: m -> m -> (m, m)
