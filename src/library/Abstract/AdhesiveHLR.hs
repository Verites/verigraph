module Abstract.AdhesiveHLR
  ( Morphism(..)
  , EpiPairs(..)
  , AdhesiveHLR(..)

  , MatchRestriction(..)
  , matchRestrictionToProp
  , NacSatisfaction(..)
  , DPOConfig(..)
  ) where

import Abstract.Morphism

-- | Type class for morphisms whose category Adhesive and suitable for
-- High-Level Replacement Systems.
--
-- Mainly provides categorical operations that AdhesiveHLR categories
-- are guaranteed to have.
class (Morphism m) => AdhesiveHLR m where
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
  -- If restrictions are known of one of the morphisms, they should be given. The implementation
  -- of this operation may then assume such restrictions for more efficient calculation.
  hasPushoutComplement :: (MorphismRestriction, m) -> (MorphismRestriction, m) -> Bool


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
  pushoutComplement :: m -> m -> (m, m)

  -- | Calculate the pullback between the two given morphisms
  --
  -- Given the morphisms /f : A -> C/ and /g : B -> C/, respectively, returns
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
  -- TODO: rename to /monoPullback/ to use category-theoretical names?
  -- @
  injectivePullback :: m -> m -> (m, m)

class Morphism m => EpiPairs m where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects.
  --
  -- If the first argument is true, only pairs of monomorphisms are created. Otherwise,
  -- pairs of arbitrary morphisms are created.
  createPairs :: Bool -> Obj m -> Obj m -> [(m, m)]

  -- TODO: document
  partitions :: Bool -> Obj m -> [m]

  -- | Create a special case of jointly epimorphic pairs, where the second morphism is a Nac
  -- The first flag indicates Nac satisfability with a monomorphic morphism
  -- The second flag indicates that the other morphism is monomorphic
  --
  -- FIXME: nacs don't belong in this module
  createPairsNac :: DPOConfig -> Obj m -> m -> [(m, m)]

  -- | Given two morphisms from the same domain, create all jointly epimorphic
  -- pairs of morphisms from their codomains, such that the square formed by
  -- all these morphisms commutes.
  --
  -- Given /f : X -> A/ and /g : X -> B/, obtain all jointly epimorphic pairs
  -- /(f', g')/ such that the following diagram commutes.
  --
  -- @
  --       g
  --    X----->B
  --    |      |
  --  f |      | f'
  --    v      v
  --    A----->Y
  --       g'
  -- @
  --
  -- Bool indicates injective
  commutingPairs :: Bool -> m -> m -> [(m, m)]
  commutingPairs inj m1 m2 = filt
    where
      allPairs = createPairs inj (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs

  -- Similar to commutingPairs but indicating which morphism is injective
  commutingPairsAlt :: (m,Bool) -> (m,Bool) -> [(m, m)]


-- | Flag indicating what restrictions are required or assumed of matches.
data MatchRestriction = MonoMatches | AnyMatches deriving (Eq, Show)

-- | Converts a match restriction to the corresponding restriction for morphism search
matchRestrictionToProp :: MatchRestriction -> MorphismRestriction
matchRestrictionToProp MonoMatches = MonoMorphisms
matchRestrictionToProp AnyMatches = AnyMorphisms

-- | Flag indicating the semantics of NAC satisfaction.
data NacSatisfaction = MonoNacSatisfaction | PartMonoNacSatisfaction deriving (Eq, Show)


data DPOConfig = DPOConfig
  { matchRestriction :: MatchRestriction
  , nacSatisfaction :: NacSatisfaction
  }
