module Abstract.Cocomplete (

  Cocomplete(..)

)

where

import           Abstract.Morphism
import           Data.List.NonEmpty

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
      h = calculateNCoequalizer $ fromList [fb',gc']
      g' = compose b' h
      f' = compose c' h
