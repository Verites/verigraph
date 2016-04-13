module Abstract.AdhesiveHLR
  ( Morphism(..)
  , AdhesiveHLR(..)
  ) where

import Abstract.Morphism

-- | Type class for morphisms whose category Adhesive and suitable for
-- High-Level Replacement Systems.
--
-- Mainly provides categorical operations that AdhesiveHLR categories
-- are guaranteed to have.
class Morphism m => AdhesiveHLR m where
  -- | Calculate the pushout between the two given morphisms.
  --
  -- Given the morphisms /f : C -> A/ and /g : C -> B/, respectively, returns
  -- the pair of morphisms /f' : B -> X/ and /g': A -> X/ such that the
  -- following square is a pushout.
  --
  -- @
  --       f
  --    C──────▶A
  --    │       │
  --  g │       │ g'
  --    ▼       ▼
  --    B──────▶X
  --       f'
  -- @
  po :: m -> m -> (m, m)

  -- | Calculate the pushout complement for two sequential morphisms.
  --
  -- Given the morphisms /g : B -> C/ and /f : A -> B/, respectively, returns
  -- the pair of morphisms /f' : A -> X/ and /g' : X -> B/ such that the
  -- following square is a pushout.
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
  -- TODO: what if it doesn't exist??
  poc :: m -> m -> (m, m)
