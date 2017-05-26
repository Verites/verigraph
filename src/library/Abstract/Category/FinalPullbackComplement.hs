module Abstract.Category.FinalPullbackComplement
  ( FinalPullbackComplement(..)
  ) where

import           Abstract.Category.FinitaryCategory

-- | Type class for morphisms whose category has the final pullback complement operation
class (FinitaryCategory morph) => FinalPullbackComplement morph where

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
  hasFinalPullbackComplement :: (MorphismType, morph) -> (MorphismType, morph) -> Bool

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
