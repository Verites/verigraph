{-|

A Cocomplete category is one which has all small colimits. This module provides a signature for
such colimits and a default implementation of pushouts based on coequalizers and coproducts.
-}
module Category.Cocomplete (

  Cocomplete(..)

)

where

import           Category.FinitaryCategory
import           Data.List.NonEmpty

-- | Type class for morphisms whose category is Cocomplete.
--
-- Mainly provides categorical operations that Cocomplete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (FinitaryCategory morph) => Cocomplete morph where

  -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the coequalizer morphism
  -- @/h : B -> X/@
  calculateCoequalizer :: morph -> morph -> morph

  -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the coequalizer Morphism
  -- @/h : B -> X/@
  calculateNCoequalizer :: NonEmpty morph -> morph

  -- | Given two objects @A@ and @B@ it returns the coproduct @(A+B, f: A -> A+B, g: B -> A+B)@
  calculateCoproduct :: Obj morph -> Obj morph -> (morph,morph)

  -- | Given a non-empty list of objects @Bi@ it returns the coproduct @fi : Bi -> SUM(Bi)@
  calculateNCoproduct :: NonEmpty (Obj morph) -> [morph]

  initialObject :: morph -> Obj morph

  calculatePushout :: morph -> morph -> (morph,morph)
  calculatePushout f g = (f', g')
    where
      b = codomain f
      c = codomain g
      (b',c') = calculateCoproduct b c
      gc' = c' <&> g
      fb' = b' <&> f
      h = calculateCoequalizer fb' gc'
      g' = h <&> b'
      f' = h <&> c'
