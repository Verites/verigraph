{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Abstract.AdhesiveHLR
  ( Morphism(..)
  , Constraint (..)
  , AtomicConstraint(..)
  , EpiPairs(..)
  , AdhesiveHLR(..)

  , MatchRestriction(..)
  , matchRestrictionToMorphismType
  , NacSatisfaction(..)
  , DPOConfig(..)
  ) where

import           Abstract.Morphism
import           Abstract.Valid

-- | Type class for morphisms whose category Adhesive and suitable for
-- High-Level Replacement Systems.
--
-- Mainly provides categorical operations that AdhesiveHLR categories
-- are guaranteed to have.
class (Morphism m) => AdhesiveHLR m where
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

  -- | Calculate the pullback between the two given morphisms
  --
  -- Given two monomorphic morphisms /f : A -> C/ and /g : B -> C/, respectively, returns
  -- the pair of monomorphic morphisms /f' : X -> B/ and /g': X -> A/ such that the
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
  --
  -- @
  monomorphicPullback :: m -> m -> (m, m)

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
  createJointlyEpimorphicPairsFromNAC :: DPOConfig -> Obj m -> m -> [(m, m)]

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


-- | Flag indicating what restrictions are required or assumed of matches.
data MatchRestriction = MonoMatches | AnyMatches deriving (Eq, Show)

-- | Converts a match restriction to the corresponding MorphismType
matchRestrictionToMorphismType :: MatchRestriction -> MorphismType
matchRestrictionToMorphismType MonoMatches = Monomorphism
matchRestrictionToMorphismType AnyMatches  = GenericMorphism

-- | Flag indicating the semantics of NAC satisfaction.
data NacSatisfaction = MonomorphicNAC | PartiallyMonomorphicNAC deriving (Eq, Show)


data DPOConfig = DPOConfig
  { matchRestriction :: MatchRestriction
  , nacSatisfaction  :: NacSatisfaction
  }

data (Morphism m, FindMorphism m, Valid m, Eq (Obj m)) => Constraint m = Constraint {
        name     :: String,
        morphism :: m,
        positive :: Bool
      }

instance Show (Constraint m) where
  show = error "Not implemented"

class Morphism m => AtomicConstraint m where

  buildNamedAtomicConstraint :: String -> m -> Bool -> Constraint m
  satisfiesAtomicConstraint :: Obj m -> Constraint m -> Bool
  satisfiesAllAtomicConstraints :: Obj m -> [Constraint m] -> Bool

instance (Morphism m, FindMorphism m, Valid m, Eq (Obj m)) => AtomicConstraint m where
  buildNamedAtomicConstraint = buildNamedConstraint
  satisfiesAtomicConstraint = satisfiesConstraint
  satisfiesAllAtomicConstraints = satisfiesAllConstraints

buildNamedConstraint :: (Valid m, FindMorphism m, Eq (Obj m)) => String -> m -> Bool -> Constraint m
buildNamedConstraint = Constraint

premise :: (Valid m, FindMorphism m, Eq (Obj m)) => Constraint m -> Obj m
premise = domain . morphism

conclusion :: (Valid m, FindMorphism m, Eq (Obj m)) => Constraint m -> Obj m
conclusion = codomain . morphism

-- | Given a TypedGraph @G@ and a Constraint @a : P -> C@, check whether @G@ satisfies the Constraint @a@
satisfiesConstraint :: (Valid m, Eq (Obj m), Morphism m, FindMorphism m) => Obj m -> Constraint m -> Bool
satisfiesConstraint graph constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findConstraintMorphisms (premise constraint) graph
    qs = findConstraintMorphisms (conclusion constraint) graph
    a = morphism constraint
    positiveSatisfaction = all (\p ->       any (\q -> compose a q == p) qs) ps
    negativeSatisfaction = all (\p -> not $ any (\q -> compose a q == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then positiveSatisfaction else negativeSatisfaction

-- | Given a TypedGraph @G@ and a list of Constraints @a : P -> C@, check whether @G@ satisfies the all the Constraints
satisfiesAllConstraints :: (Valid m, Eq (Obj m), FindMorphism m) => Obj m -> [Constraint m] -> Bool
satisfiesAllConstraints graph = all (satisfiesConstraint graph)

findConstraintMorphisms :: (FindMorphism m) => Obj m -> Obj m -> [m]
findConstraintMorphisms = findMonomorphisms
