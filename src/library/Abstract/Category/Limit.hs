{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Abstract.Category.Limit (Complete(..), Cocomplete(..)) where

import           Data.Proxy

import           Abstract.Category
import           Data.List.NonEmpty (NonEmpty (..))

-- | Type class for morphisms whose category is Complete.
--
-- Mainly provides categorical operations that Complete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (Category morph) => Complete morph where

  -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the equalizer morphism
  -- @/h : X -> A/@
  calculateEqualizer :: morph -> morph -> morph

  -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the equalizer
  -- morphism @/h : X -> A/@
  calculateNEqualizer :: NonEmpty morph -> morph
  calculateNEqualizer (f :| []) = identity (domain f)
  calculateNEqualizer (f :| [g]) = calculateEqualizer f g
  calculateNEqualizer (f :| g : gs) =
    let
      q = calculateNEqualizer (g :| gs)
      p = calculateEqualizer (f <&> q) (g <&> q)
    in (q <&> p)

  -- | Given two objects @A@ and @B@ it returns the product @(AxB, f: AxB -> A, g: AxB -> B)@
  calculateProduct :: Obj morph -> Obj morph -> (morph,morph)
  calculateProduct x y =
    calculatePullback (morphismToFinalFrom y) (morphismToFinalFrom x)

  -- | Given a non-empty list of objects @Bi@ it returns the product @fi : PROD(Bi) -> Bi@
  calculateNProduct :: NonEmpty (Obj morph) -> [morph]
  calculateNProduct (x :| []) = [identity x]
  calculateNProduct (x :| [y]) = [px, py]
    where (px, py) = calculateProduct x y
  calculateNProduct (x :| y : ys) =
    let
      qs = calculateNProduct (y :| ys)
      (px, pys) = calculateProduct x (domain $ head qs)
    in px : map (<&> pys) qs

  finalObject :: morph -> Obj morph

  morphismToFinalFrom :: Obj morph -> morph

  isFinal :: Proxy morph -> Obj morph -> Bool
  isFinal _ = isIsomorphism . morphismToFinalFrom @morph

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


-- | Type class for morphisms whose category is Cocomplete.
--
-- Mainly provides categorical operations that Cocomplete categories
-- are guaranteed to have.
--
-- Note that for performance reasons, verigraph assumes that the parameters
-- are valid for all functions in this module.
class (Category morph) => Cocomplete morph where

    -- | Given two morphisms @/f : A -> B/@ and @/g : A -> B/@ retuns the coequalizer morphism
    -- @/h : B -> X/@
    calculateCoequalizer :: morph -> morph -> morph

    -- | Given a non-empty list of morphisms of the form @/f : A -> B/@ returns the coequalizer Morphism
    -- @/h : B -> X/@
    calculateNCoequalizer :: NonEmpty morph -> morph
    calculateNCoequalizer (f :| []) = identity (codomain f)
    calculateNCoequalizer (f :| [g]) = calculateCoequalizer f g
    calculateNCoequalizer (f :| g : gs) =
      let
        k = calculateNCoequalizer (g :| gs)
        j = calculateCoequalizer (k <&> f) (k <&> g)
      in j <&> k

    -- | Given two objects @A@ and @B@ it returns the coproduct @(A+B, f: A -> A+B, g: B -> A+B)@
    calculateCoproduct :: Obj morph -> Obj morph -> (morph,morph)

    -- | Given a non-empty list of objects @Bi@ it returns the coproduct @fi : Bi -> SUM(Bi)@
    calculateNCoproduct :: NonEmpty (Obj morph) -> [morph]
    calculateNCoproduct (x :| []) = [identity x]
    calculateNCoproduct (x :| [y]) = [jx, jy]
      where (jx, jy) = calculateCoproduct x y
    calculateNCoproduct (x :| y : ys) =
      let
        ks = calculateNCoproduct (y :| ys)
        (jx, jys) = calculateCoproduct x (codomain $ head ks)
      in jx : map (jys <&>) ks

    initialObject :: morph -> Obj morph

    morphismFromInitialTo :: Obj morph -> morph

    isInitial :: Proxy morph -> Obj morph -> Bool
    isInitial _ = isIsomorphism . morphismFromInitialTo @morph

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
