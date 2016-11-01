{-# LANGUAGE FlexibleContexts  #-}
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
  , DPOConfig(..)
  ) where

import           Abstract.Cocomplete
import           Abstract.Morphism
import           Abstract.Valid

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
  --
  -- @
  calculatePullback :: m -> m -> (m, m)

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

data AtomicConstraint m = AtomicConstraint {
        name     :: String,
        morphism :: m,
        positive :: Bool
      } deriving (Show)

instance Valid m => Valid (AtomicConstraint m) where
  validate = validate . morphism

buildNamedAtomicConstraint :: String -> m -> Bool -> AtomicConstraint m
buildNamedAtomicConstraint = AtomicConstraint

premise :: (Morphism m) => AtomicConstraint m -> Obj m
premise = domain . morphism

conclusion :: (Morphism m) => AtomicConstraint m -> Obj m
conclusion = codomain . morphism

-- | Given an object @G@ and a AtomicConstraint @a : P -> C@, check whether @G@ satisfies the AtomicConstraint @a@
satisfiesAtomicConstraint :: (FindMorphism m) => Obj m -> AtomicConstraint m -> Bool
satisfiesAtomicConstraint graph constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findMonomorphisms (premise constraint) graph
    qs = findMonomorphisms (conclusion constraint) graph
    a = morphism constraint
    positiveSatisfaction = all (\p ->       any (\q -> compose a q == p) qs) ps
    negativeSatisfaction = all (\p -> not $ any (\q -> compose a q == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then positiveSatisfaction else negativeSatisfaction

-- | Given an object @G@ and a list of AtomicConstraints @a : P -> C@, check whether @G@ satisfies the all them
satisfiesAllAtomicConstraints :: (FindMorphism m) => Obj m -> [AtomicConstraint m] -> Bool
satisfiesAllAtomicConstraints graph = all (satisfiesAtomicConstraint graph)

data Constraint m =
    Atomic { atomic :: AtomicConstraint m }
  | And { lc :: Constraint m,
          rc :: Constraint m}
  | Or{ lc :: Constraint m,
        rc :: Constraint m}
  | Not { nc :: Constraint m }

instance Valid m => Valid (Constraint m) where
    validate cons = case cons of
      Atomic a -> validate a
      Not b    -> validate (nc b)
      And a b  -> mconcat [validate (lc a), validate (rc b)]
      Or a b   -> mconcat [validate (lc a), validate (rc b)]

-- | Given an object @G@ and a Constraint @c@ (a Boolean formula over atomic constraints), check whether @G@ satisfies @c@
satisfiesConstraint :: (FindMorphism m) => Obj m -> Constraint m -> Bool
satisfiesConstraint graph constraint =
  case constraint of
    Atomic atomic -> satisfiesAtomicConstraint graph atomic
    Not nc -> not $ satisfiesConstraint graph nc
    And lc rc -> satisfiesConstraint graph lc && satisfiesConstraint graph rc
    Or lc rc -> satisfiesConstraint graph lc || satisfiesConstraint graph rc

-- | Given an object @G@ and a list of Constraints (Boolean formulas over atomic constraints), check whether @G@ satisfies the all them
satisfiesAllConstraints :: (FindMorphism m) => Obj m -> [Constraint m] -> Bool
satisfiesAllConstraints graph = all (satisfiesConstraint graph)
