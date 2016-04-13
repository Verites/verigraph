{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Abstract.DPO
  (
  -- * Productions
    Production
  , production

  , left
  , right
  , nacs

  -- ** Applying
  , dpo
  , comatch

  -- ** Manipulating
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

production :: (Morphism m, Eq (Obj m)) => m -> m -> [m] -> Production m
production = Production

instance (Morphism m, Valid m, Eq (Obj m)) => Valid (Production m) where
  valid (Production l r nacs) =
    valid l && valid r && all valid nacs &&
    domain l == domain r && all (==codomain l) (map domain nacs)

dpo :: AdhesiveHLR m => m -> Production m -> (m, m, m, m)
dpo m (Production l r _) =
  let (m', l') = poc m l
      (m'', r') = po m' r
  in (m',m'',l',r')

comatch :: AdhesiveHLR m => m -> Production m -> m
comatch m prod = let (_,m',_,_) = dpo m prod in m'

inverseWithoutNacs :: Production m -> Production m
inverseWithoutNacs p = Production (right p) (left p) []
