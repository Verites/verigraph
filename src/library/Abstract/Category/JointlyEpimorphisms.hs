module Abstract.Category.JointlyEpimorphisms
( JointlyEpimorphisms(..)
, createJointlyEpimorphicPairsFromCodomains
) where

import           Abstract.Category.AdhesiveHLR

class FinitaryCategory morph => JointlyEpimorphisms morph where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects.
  --
  -- If the first argument is true, only pairs of monomorphisms are created.
  -- Otherwise, pairs of arbitrary morphisms are created.
  createJointlyEpimorphicPairs :: Bool -> Obj morph -> Obj morph -> [(morph,morph)]

  -- | Create all quotients of the given object, i.e. all (up to isomorphism) epimorphisms having
  -- the given object as domain.
  createAllQuotients :: Obj morph -> [morph]

  -- | Create a special case of jointly epimorphic pairs, where the second morphism is a Nac.
  -- The pairs generated are dependent of the NAC config.
  --
  -- FIXME: nacs don't belong in this module
  createJointlyEpimorphicPairsFromNAC :: MorphismsConfig -> Obj morph -> morph -> [(morph,morph)]

  -- Given the morphisms /f : X -> A/ and /g : X -> B/ with the same domain,
  -- obtain all jointly epimorphic pairs /(f', g')/ such that the following
  -- diagram commutes.
  -- @
  --        g
  --     X──────▶B
  --     │       │
  --   f │       │ f'
  --     ▼       ▼
  --     A──────▶Y
  --        g'
  -- @
  --
  -- If the first argument is true, only pairs of monomorphisms are created.
  -- Otherwise, pairs of arbitrary morphisms are created.
  calculateCommutativeSquares :: Bool -> morph -> morph -> [(morph,morph)]
  calculateCommutativeSquares inj m1 m2 = filt
    where
      allPairs = createJointlyEpimorphicPairs inj (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> x <&> m1 == y <&> m2) allPairs

  -- Similar to calculateCommutativeSquares but indicating which morphism is injective.
  --
  -- Given the morphisms /f : X -> A/ and /g : X -> B/ with the same domain,
  -- obtain all jointly epimorphic pairs /(f', g')/ such that the following
  -- diagram commutes.
  -- @
  --        g
  --     X──────▶B
  --     │       │
  --   f │       │ f'
  --     ▼       ▼
  --     A──────▶Y
  --        g'
  -- @
  --
  -- If the first Bool argument is True then g' is injective.
  -- If the second Bool argument is True then f' is injective.
  calculateCommutativeSquaresAlongMonomorphism :: (morph,Bool) -> (morph,Bool) -> [(morph,morph)]

-- | Create all jointly epimorphic pairs of morphisms from the codomains of the given morphisms.
-- The flag indicates only monomorphic morphisms.
createJointlyEpimorphicPairsFromCodomains :: (JointlyEpimorphisms morph) => MatchRestriction -> morph -> morph -> [(morph,morph)]
createJointlyEpimorphicPairsFromCodomains inj m1 m2 =
  createJointlyEpimorphicPairs (inj == MonoMatches) (codomain m1) (codomain m2)
