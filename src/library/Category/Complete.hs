module Category.Complete (

  Complete(..)

)

where

import           Category.Morphism
import           Data.List.NonEmpty

-- | Type class for morphisms whose category is Complete.
--
-- Mainly provides categorical operations that Complete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (Morphism m) => Complete m where

  -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the equalizer morphism
  -- @/h : X -> A/@
  calculateEqualizer :: m -> m -> m

  -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the equalizer
  -- morphism @/h : X -> A/@
  calculateNEqualizer :: NonEmpty m -> m

  -- | Given two objects @A@ and @B@ it returns the product @(AxB, f: AxB -> A, g: AxB -> B)@
  calculateProduct :: Obj m -> Obj m -> (m,m)

  -- | Given a non-empty list of objects @Bi@ it returns the product @fi : PROD(Bi) -> Bi@
  calculateNProduct :: NonEmpty (Obj m) -> [m]

  finalObject :: m -> Obj m

  calculatePullback :: m -> m -> (m, m)
  calculatePullback f g = (f',g')
    where
      a = domain f
      b = domain g
      (a',b') = calculateProduct a b
      a'f = compose a' f
      b'g = compose b' g
      h = calculateEqualizer a'f b'g
      f' = compose h b'
      g' = compose h a'
