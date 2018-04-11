{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Abstract.Category.Adhesive
  (
    -- * \(\mathcal{M}\)-adhesive categories
    MAdhesive(..)
  , subobjectIntersection
  , subobjectUnion
    -- * Related Constructions
  , MInitialPushout(..)
  , FinalPullbackComplement(..)
  ) where

import           Abstract.Category
import           Abstract.Category.Limit
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary

{- Type class for \(\mathcal{M}\)-adhesive categories.

 In these categories, there exist pullbacks and pushouts along
 \(\mathcal{M}\)-morphisms. Furtheremore, pushout complements along
 \(\mathcal{M}\)-morphisms are unique, when they exist.
-}
class MFinitary morph => MAdhesive morph where
  -- | Calculate the pushout of an \(\mathcal{M}\)-morphism and another morphism.
  --
  -- Given the morphisms \(m : Z \to X\) and \(f : Z \to Y\), respectively and
  -- with \(m \in \mathcal{M}\), returns the pair of morphisms \(f' : Y \to S\)
  -- and \(m': X \to S\) such that the following square is a pushout.
  --
  -- @
  --       f
  --    Z──────▶Y
  --    │       │
  --  m │       │ m'
  --    ▼       ▼
  --    X──────▶S
  --       f'
  -- @
  --
  -- The behaviour of this function is undefined if the first morphism isn't
  -- in \(\mathcal{M}\), or if the morphisms have different domains.
  calculatePushoutAlongM :: morph -> morph -> (morph, morph)
  default calculatePushoutAlongM :: Cocomplete morph => morph -> morph -> (morph, morph)
  calculatePushoutAlongM = calculatePushout

  -- | Calculate the pullback an \(\mathcal{M}\)-morphism and another morphism.
  --
  -- Given the morphisms \(f : A \to B\) and \(g : A \to C\), respectively and
  -- with \(m \in \mathcal{M}\), returns the pair of morphisms \(f' : P \to B\)
  -- and \(g': P \to A\) such that the following square is a pullback.
  --
  -- @
  --        g'
  --     P──────▶B
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     A──────▶C
  --        g
  -- @
  --
  -- The behaviour of this function is undefined if the first morphism isn't
  -- in \(\mathcal{M}\), or if the morphisms have different domains.
  calculatePullbackAlongM :: morph -> morph -> (morph, morph)
  default calculatePullbackAlongM :: Complete morph => morph -> morph -> (morph, morph)
  calculatePullbackAlongM = calculatePullback

  -- | Check if the morphisms \(m : X \to Y\) and \(f : Y \to Z\), respectively
  -- and with \(m \in \mathcal{M}\), have a pushout complement
  -- (see 'calculatePushoutComplementAlongM').
  --
  -- The behaviour of this function is undefined when \(m \notin \mathcal{M}\).
  hasPushoutComplementAlongM :: morph -> morph -> Bool

  -- | Calculate the pushout complement for a sequence of a morphisms,
  -- __assuming the first morphism is in \(\mathcal{M}\) and the complement exists__.
  -- In order to test if the pushout complement exists, use 'hasPushoutComplementAlongMono'.
  --
  -- Given the morphisms \(m : X \to Y\) and \(f : Y \to Z\), respectively and
  -- with \(m \in \mathcal{M}\), returns the pair of morphisms \(f' : X \to W\)
  -- and \(m' : W \to Z\) such that the following square is a pushout. Since
  -- the category is adhesive, such a pair is unique.
  --
  -- @
  --        m
  --     X──────▶Y
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     W──────▶Z
  --        m'
  -- @
  --
  -- The behaviour of this function is undefined if \(m \notin \mathcal{M}\),
  -- or if the pushout complement doesn't exist.
  calculatePushoutComplementAlongM :: morph -> morph -> (morph, morph)

-- | Given two \(\mathcal{M}\)-subobjects of the same object X, calculate their
-- intersection (by taking a pullback)
subobjectIntersection :: MAdhesive morph => morph -> morph -> morph
subobjectIntersection a b =
  let (_, b') = calculatePullbackAlongM a b
  in a <&> b'

-- | Given two \(\mathcal{M}\)-subobjects of the same object X, calculate their
-- union as the pushout over their intersection
subobjectUnion :: (FindMorphism morph, MAdhesive morph) => morph -> morph -> morph
subobjectUnion a b =
  let
    (a'', b'') = calculatePullbackAlongM a b
    (a', b') = calculatePushoutAlongM a'' b''
    candidates = findSpanCommuters monic a' a
  in case filter (\h -> h <&> b' == b) candidates of
    [] -> error "subobjectUnion: no union found"
    [h] -> h
    _ -> error "subobjectUnion: multiple unions foind"

class MFinitary morph => MInitialPushout morph where
  -- | Calculate the \(\mathcal{M}\)-initial pushout of the given morphism.
  --
  -- Given the morphism \(f : A \to A'\), returns the morphisms \(b : B \to A\),
  -- \(f' : B \to C\) and \(c: C \to A'\), respectively and with
  -- \(b,c \in \mathcal{M}\), such that the following square is an
  -- \(\mathcal{M}\)-initial pushout of \(f\).
  --
  -- @
  --        f'
  --    B──────▶C
  --    │       │
  --  b │       │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  calculateMInitialPushout :: morph -> (morph, morph, morph)


-- | Type class for morphisms whose category has the final pullback complement operation
class (Category morph) => FinalPullbackComplement morph where

    -- | Checks if the given sequential morphisms have a final pullback
    -- complement, assuming they satisfy the given restriction.
    --
    -- Given the morphisms /g : B -> C/ and /f : A -> B/, respectively, tests if
    -- there exists a pair of morphisms /f' : A -> X/ and /g' : X -> B/ such that the
    -- following square is the final pullback.
    --
    -- @
    --        f
    --     A──────▶B
    --     │       │
    --  g' │       │ g
    --     ▼       ▼
    --     X──────▶C
    --        f'
    -- @
    hasFinalPullbackComplement :: (MorphismClass morph, morph) -> (MorphismClass morph, morph) -> Bool

    -- | Calculate the final pullback complement for two sequential morphisms, __assumes it exists__.
    --
    -- In order to test if the final pullback complement exists, use 'hasFinalPullbackComplement'.
    --
    -- Given the morphisms /g : B -> C/ and /f : A -> B/, respectively, returns
    -- the pair of morphisms /g' : X -> B/ and /f' : A -> X/ such that the
    -- following square is a pullback.
    --
    -- @
    --        f
    --     A──────▶B
    --     │       │
    --  g' │       │ g
    --     ▼       ▼
    --     X──────▶C
    --        f'
    -- @
    calculateFinalPullbackComplement :: morph -> morph -> (morph,morph)
