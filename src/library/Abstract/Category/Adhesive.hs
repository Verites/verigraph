{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Abstract.Category.Adhesive
  (
    -- * \(\mathcal{M}\)-adhesive categories
    MAdhesive(..)
  , subobjectIntersection
  , subobjectUnion
    -- ** Generalized subobjects
    -- $generalized-subobjects
  , pairSubobjectIntersection
  , pairSubobjectUnion
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
    _ -> error "subobjectUnion: multiple unions found"

-- $generalized-subobjects
-- We can generalize the notion of subobject, defining subobjects for a
-- pair of objects instead of a single one. The theory behind this is
-- further developed in a Master's thesis by Guilherme Azzi (2018).
--
-- Given a pair of objects \((L_1, L_2)\), a \(\mathcal{M}\)-pair-subobject
-- is a pair of \(\mathcal{M}\)-morphisms \(L_1 \rightarrow G \leftarrow L_2\),
-- and \(\mathbf{Sub}(L_1,L_2)\) is a partially ordered set of
-- \(\mathcal{M}\)-pair-subobjects.
--
-- Unlike regular \(\mathcal{M}\)-subobjects, the poset of
-- \(\mathcal{M}\)-pair-subobjects is not a lattice. But it has
-- all intersections, and unions work as a partial function.

-- | Given two \(\mathcal{M}\)-pair-subobjects, calculate their intersection
-- (by taking a pullback followed by an equalizer).
pairSubobjectIntersection :: (Complete morph, MAdhesive morph) => Span morph -> Span morph -> Span morph
pairSubobjectIntersection (x1, x2) (y1, y2) =
  let 
    (iy', ix') = calculatePullbackAlongM x1 y1
    e = calculateEqualizer (x2 <&> ix') (y2 <&> iy')
    ix = ix' <&> e
    (i1, i2) = (x1 <&> ix, x2 <&> ix)
  in (i1, i2)

-- | Given two \(\mathcal{M}\)-pair-subobjects, calculate their union, if it exists
-- (by taking a pushout over their intersection).
pairSubobjectUnion :: (Complete morph, FindMorphism morph, MAdhesive morph) => Span morph -> Span morph -> Maybe (Span morph)
pairSubobjectUnion (x1, x2) (y1, y2) =
  let
    (iy', ix') = calculatePullbackAlongM x1 y1
    e = calculateEqualizer (x2 <&> ix') (y2 <&> iy')
    (ix, iy) = (ix' <&> e, iy' <&> e)
    (yu, xu) = calculatePushoutAlongM ix iy
    (u1Candidates, u2Candidates) = (findSpanCommuters monic xu x1, findSpanCommuters monic xu x2)
    isCandidateAcceptable y u = isInclusion u && u <&> yu == y
  in case (filter (isCandidateAcceptable y1) u1Candidates, filter (isCandidateAcceptable y2) u2Candidates) of
    ([u1], [u2]) -> Just (u1, u2)
    ([], _) -> Nothing
    (_, []) -> Nothing
    _ -> error "pairSubobjectUnion: multiple unions found"

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
