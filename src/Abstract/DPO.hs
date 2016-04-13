{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Abstract.DPO
  ( Production
  , production

  , left
  , right
  , nacs

  , inverseWithoutNacs
  ) where

import Abstract.AdhesiveHLR
import Abstract.Valid

data Production m = Production
  { left :: m
  , right :: m
  , nacs :: [m]
  }
  deriving (Show, Read)

production :: (Morphism m, Valid m, Eq (Obj m)) => m -> m -> [m] -> Production m
production = Production

inverseWithoutNacs :: Production m -> Production m
inverseWithoutNacs p = Production (right p) (left p) []

instance (Morphism m, Valid m, Eq (Obj m)) => Valid (Production m) where
  valid (Production l r nacs) =
    valid l && valid r && all valid nacs &&
    domain l == domain r && all (==codomain l) (map domain nacs)
