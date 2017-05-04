{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Abstract.AdhesiveHLR
  ( Morphism(..)
  , AtomicConstraint (..)
  , buildNamedAtomicConstraint
  , satisfiesAtomicConstraint
  , satisfiesAllAtomicConstraints
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  , EpiPairs(..)
  , AdhesiveHLR(..)

  , MatchRestriction(..)
  , matchRestrictionToMorphismType
  , NacSatisfaction(..)
  , MorphismsConfig(..)
  ) where

import           Abstract.Cocomplete
import           Abstract.Constraint
import           Abstract.Morphism

-- | Type class for morphisms whose category Adhesive and suitable for
-- High-Level Replacement Systems.
--
-- Mainly provides categorical operations that AdhesiveHLR categories
-- are guaranteed to have.
class (Cocomplete m) => AdhesiveHLR m where
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
  calculateInitialPushout :: m -> (m, m, m)

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
  calculatePushout :: m -> m -> (m, m)
  calculatePushout = Abstract.Cocomplete.calculatePushout

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
  hasPushoutComplement :: (MorphismType, m) -> (MorphismType, m) -> Bool


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
  calculatePushoutComplement :: m -> m -> (m, m)

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
  calculatePullback :: m -> m -> (m, m)

class Morphism m => EpiPairs m where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects.
  --
  -- If the first argument is true, only pairs of monomorphisms are created.
  -- Otherwise, pairs of arbitrary morphisms are created.
  createJointlyEpimorphicPairs :: Bool -> Obj m -> Obj m -> [(m, m)]

  -- | Create all subobjects from the given object.
  --
  -- If the first argument is true, only identity morphism is created.
  -- Otherwise, arbitrary (epimorphic) morphisms are created.
  createAllSubobjects :: Bool -> Obj m -> [m]

  -- | Create a special case of jointly epimorphic pairs, where the second morphism is a Nac.
  -- The pairs generated are dependent of the NAC config.
  --
  -- FIXME: nacs don't belong in this module
  createJointlyEpimorphicPairsFromNAC :: MorphismsConfig -> Obj m -> m -> [(m, m)]

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
  calculateCommutativeSquares :: Bool -> m -> m -> [(m, m)]
  calculateCommutativeSquares inj m1 m2 = filt
    where
      allPairs = createJointlyEpimorphicPairs inj (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs

  -- Similar to calculateCommutativeSquares but indicating which morphism is injective
  calculateCommutativeSquaresAlongMonomorphism :: (m,Bool) -> (m,Bool) -> [(m, m)]


-- | Flag indicating what restrictions are required or assumed of matches.
data MatchRestriction = MonoMatches | AnyMatches deriving (Eq, Show)

-- | Converts a match restriction to the corresponding MorphismType
matchRestrictionToMorphismType :: MatchRestriction -> MorphismType
matchRestrictionToMorphismType MonoMatches = Monomorphism
matchRestrictionToMorphismType AnyMatches  = GenericMorphism

-- | Flag indicating the semantics of NAC satisfaction.
data NacSatisfaction = MonomorphicNAC | PartiallyMonomorphicNAC deriving (Eq, Show)

data MorphismsConfig = MorphismsConfig
  { matchRestriction :: MatchRestriction
  , nacSatisfaction  :: NacSatisfaction
  }
