module Base.Isomorphic where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.List as List


-- | The 'Iso' class defines isomorphism ('~=') checks.
class Iso a where
  -- | Check if the given values are isomorphic to each other.
  (~=) :: a -> a -> Bool

infix 4 ~=


instance Iso a => Iso [a] where
  (~=) = List.correspondsOneToOne (~=)

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
