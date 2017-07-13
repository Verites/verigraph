{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Abstract.Category.AdhesiveHLR
  ( FinitaryCategory(..)
  , AtomicConstraint (..)
  , buildNamedAtomicConstraint
  , satisfiesAtomicConstraint
  , satisfiesAllAtomicConstraints
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  , AdhesiveHLR(..)
  ) where

import           Abstract.Category.AdhesiveHLR.Constraint
import           Abstract.Category.Cocomplete
import           Abstract.Category.FinitaryCategory

-- | Type class for morphisms whose category Adhesive and suitable for
-- High-Level Replacement Systems.
--
-- Mainly provides categorical operations that AdhesiveHLR categories
-- are guaranteed to have.
class (Cocomplete morph) => AdhesiveHLR morph where
  -- | Calculate the initial pushout of the given morphism.
  --
  -- Given the morphism /f : A -> A'/, returns
  -- the morphisms /b : B -> A/, /f' : B -> C/ and /c: C -> A'/ such that
  -- the following square is the initial pushout of f.
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
  calculateInitialPushout :: morph -> (morph,morph,morph)

  -- | Calculate the pushout between the two given morphisms.
  --
  -- Given the morphisms /f : A -> B/ and /g : A -> C/, respectively, returns
  -- the pair of morphisms /f' : C -> D/ and /g': B -> D/ such that the
  -- following square is a pushout.
  --
  -- @
  --       g
  --    A──────▶C
  --    │       │
  --  f │       │ f'
  --    ▼       ▼
  --    B──────▶D
  --       g'
  -- @
  calculatePushout :: morph -> morph -> (morph,morph)
  calculatePushout = Abstract.Category.Cocomplete.calculatePushout

  -- | Checks if the given sequential morphisms have a pushout complement, assuming they satsify
  -- the given restriction.
  --
  -- Given the morphisms /g : B -> C/ and /f : A -> B/, respectively, tests if
  -- there exists a pair of morphisms /f' : A -> X/ and /g' : X -> B/ such that the
  -- following square is a pushout. Since the category is Adhesive, such a pair is unique.
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
  --
  -- If the types of the morphisms are known, they should be given. The implementation
  -- of this operation may then use them for more efficient calculation.
  hasPushoutComplement :: (MorphismType, morph) -> (MorphismType, morph) -> Bool


  -- | Calculate the pushout complement for two sequential morphisms, __assumes it exists__.
  --
  -- In order to test if the pushout complement exists, use 'hasPushoutComplement'.
  --
  -- Given the morphisms /g : B -> C/ and /f : A -> B/, respectively, returns
  -- the pair of morphisms /f' : A -> X/ and /g' : X -> B/ such that the
  -- following square is a pushout. Since the category is Adhesive, such a pair is unique.
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
  calculatePushoutComplement :: morph -> morph -> (morph,morph)

  -- | Calculate the pullback between the two given morphisms.
  --
  -- Given two morphisms /f : A -> C/ and /g : B -> C/, respectively, returns
  -- the pair of morphisms /f' : X -> B/ and /g': X -> A/ such that the
  -- following square is a pullback.
  --
  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  -- @
  calculatePullback :: morph -> morph -> (morph,morph)
