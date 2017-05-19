{-|

A Cocomplete category is one which has all small colimits. This module provides a signature for
such colimits and a default implementation of pushouts based on coequalizers and coproducts.
-}
module Category.Cocomplete (

  Cocomplete(..)

)

where

import           Category.Morphism
import           Data.List.NonEmpty

-- | Type class for morphisms whose category is Cocomplete.
--
-- Mainly provides categorical operations that Cocomplete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (Morphism m) => Cocomplete m where

  -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the coequalizer morphism
  -- @/h : B -> X/@
  calculateCoequalizer :: m -> m -> m

  -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the coequalizer Morphism
  -- @/h : B -> X/@
  calculateNCoequalizer :: NonEmpty m -> m

  -- | Given two objects @A@ and @B@ it returns the coproduct @(A+B, f: A -> A+B, g: B -> A+B)@
  calculateCoproduct :: Obj m -> Obj m -> (m,m)

  -- | Given a non-empty list of objects @Bi@ it returns the coproduct @fi : Bi -> SUM(Bi)@
  calculateNCoproduct :: NonEmpty (Obj m) -> [m]

  initialObject :: m -> Obj m

  calculatePushout :: m -> m -> (m, m)
  calculatePushout f g = (f', g')
    where
      b = codomain f
      c = codomain g
      (b',c') = calculateCoproduct b c
      gc' = compose g c'
      fb' = compose f b'
      h = calculateCoequalizer fb' gc'
      g' = compose b' h
      f' = compose c' h
