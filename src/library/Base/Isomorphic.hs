{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Base.Isomorphic where

import           Data.List.NonEmpty                 (NonEmpty)
import qualified Data.List.NonEmpty                 as NonEmpty

import Util.Monad

-- | The 'Iso' class defines isomorphism ('~=') checks.
class Iso a where
  -- | Check if the given values are isomorphic to each other.
  (~=) :: a -> a -> Bool

infix 4 ~=

instance Iso a => Iso [a] where
  [] ~= [] = True
  [] ~= _ = False
  _ ~= [] = False
  (x:xs) ~= ys = case break (~=x) ys of
    (_, []) -> False
    (ys1, _:ys2) -> xs ~= (ys1++ys2)

instance Iso a => Iso (Maybe a) where
  Nothing ~= Nothing = True
  Just x ~= Just y = x ~= y
  _ ~= _ = False

instance Iso a => Iso (NonEmpty a) where
  xs ~= ys = NonEmpty.toList xs ~= NonEmpty.toList ys

instance (Iso a, Iso b) => Iso (a, b) where
  (xa, xb) ~= (ya, yb) = xa ~= ya && xb ~= yb

instance (Iso a, Iso b, Iso c) => Iso (a, b, c) where
  (xa, xb, xc) ~= (ya, yb, yc) = xa ~= ya && xb ~= yb && xc ~= yc

instance (Iso a, Iso b, Iso c, Iso d) => Iso (a, b, c, d) where
  (xa, xb, xc, xd) ~= (ya, yb, yc, yd) =
    xa ~= ya && xb ~= yb && xc ~= yc && xd ~= yd

instance (Iso a, Iso b, Iso c, Iso d, Iso e) => Iso (a, b, c, d, e) where
  (xa, xb, xc, xd, xe) ~= (ya, yb, yc, yd, ye) =
    xa ~= ya && xb ~= yb && xc ~= yc && xd ~= yd && xe ~= ye

-- | The 'IsoM' class defines isomorphism checks that run inside monads.
class Monad m => IsoM m a where
  -- | Check if the given values are isomorphic to each other.
  isIso :: a -> a -> m Bool

instance IsoM m a => IsoM m [a] where
  isIso [] [] = return True
  isIso [] _  = return False
  isIso _  [] = return False
  isIso (x:xs) ys = do
    (ys1, ys2) <- breakM (isIso x) ys
    case ys2 of
      [] -> return False
      _ : ys2' -> isIso xs (ys1 ++ ys2')

instance IsoM m a => IsoM m (Maybe a) where
  isIso Nothing Nothing = return True
  isIso (Just x) (Just y) = isIso x y
  isIso _ _ = return False

instance IsoM m a => IsoM m (NonEmpty a) where
  isIso xs ys = isIso (NonEmpty.toList xs) (NonEmpty.toList ys)

instance (IsoM m a, IsoM m b) => IsoM m (a, b) where
  isIso (xa, xb) (ya, yb) = isIso xa ya `andM` isIso xb yb

instance (IsoM m a, IsoM m b, IsoM m c) => IsoM m (a, b, c) where
  isIso (xa, xb, xc) (ya, yb, yc) = isIso xa ya `andM` isIso xb yb `andM` isIso xc yc

instance (IsoM m a, IsoM m b, IsoM m c, IsoM m d) => IsoM m (a, b, c, d) where
  isIso (xa, xb, xc, xd) (ya, yb, yc, yd) =
    isIso xa ya `andM` isIso xb yb `andM` isIso xc yc `andM` isIso xd yd

instance (IsoM m a, IsoM m b, IsoM m c, IsoM m d, IsoM m e) => IsoM m (a, b, c, d, e) where
  isIso (xa, xb, xc, xd, xe) (ya, yb, yc, yd, ye) =
    isIso xa ya `andM` isIso xb yb `andM` isIso xc yc `andM` isIso xd yd `andM` isIso xe ye
