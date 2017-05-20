module Abstract.Category.Complete (

  Complete(..)

)

where

import           Abstract.Category.FinitaryCategory
import           Data.List.NonEmpty

-- | Type class for morphisms whose category is Complete.
--
-- Mainly provides categorical operations that Complete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (FinitaryCategory morph) => Complete morph where

  -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the equalizer morphism
  -- @/h : X -> A/@
  calculateEqualizer :: morph -> morph -> morph

  -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the equalizer
  -- morphism @/h : X -> A/@
  calculateNEqualizer :: NonEmpty morph -> morph

  -- | Given two objects @A@ and @B@ it returns the product @(AxB, f: AxB -> A, g: AxB -> B)@
  calculateProduct :: Obj morph -> Obj morph -> (morph,morph)

  -- | Given a non-empty list of objects @Bi@ it returns the product @fi : PROD(Bi) -> Bi@
  calculateNProduct :: NonEmpty (Obj morph) -> [morph]

  finalObject :: morph -> Obj morph

  calculatePullback :: morph -> morph -> (morph,morph)
  calculatePullback f g = (f',g')
    where
      a = domain f
      b = domain g
      (a',b') = calculateProduct a b
      a'f = f <&> a'
      b'g = g <&> b'
      h = calculateEqualizer a'f b'g
      f' = b' <&> h
      g' = a' <&> h
