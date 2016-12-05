module Abstract.EpiPairs where

--import           Abstract.Cocomplete
import           Abstract.Morphism

class Morphism m => EpiPairs m where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects.
  --
  -- If the first argument is true, only pairs of monomorphisms are created. Otherwise,
  -- pairs of arbitrary morphisms are created.
  createJointlyEpimorphicPairs :: Bool -> Obj m -> Obj m -> [(m, m)]

  -- TODO: document
  createAllSubobjects :: Bool -> Obj m -> [m]

  -- | Create a special case of jointly epimorphic pairs, where the second morphism is a Nac
  -- The first flag indicates Nac satisfability with a monomorphic morphism
  -- The second flag indicates that the other morphism is monomorphic
  --
  -- FIXME: nacs don't belong in this module
  --createJointlyEpimorphicPairsFromNAC :: DPOConfig -> Obj m -> m -> [(m, m)]

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
  -- Bool indicates injective
  calculateCommutativeSquares :: Bool -> m -> m -> [(m, m)]
  calculateCommutativeSquares inj m1 m2 = filt
    where
      allPairs = createJointlyEpimorphicPairs inj (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs

  -- Similar to calculateCommutativeSquares but indicating which morphism is injective
  calculateCommutativeSquaresAlongMonomorphism :: (m,Bool) -> (m,Bool) -> [(m, m)]
