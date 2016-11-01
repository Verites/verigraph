module SndOrder.Morphism.CommutingSquares (
    commutingMorphism
  , commutingMorphismSameDomain
  , commutingMorphismSameCodomain
  ) where

import           Abstract.Morphism
import           TypedGraph.Morphism

-- | Given the morphisms /a1 : X -> B1/, /b1 : Y -> B1/,
-- /a2 : X -> B2/ and /b2 : Y -> B2/, respectively,
-- creates the monomorphic morphism /x : X -> Y/,
-- where the following diagram commutes:
--
-- @
--         X
--         |
--   a1    |x   a2
--         ▼
--  B1◀────Y────▶B2
--      b1   b2
-- @
--
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                  -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphism a1 b1 a2 b2 = buildTypedGraphMorphism (domain a1) (domain b1) select
  where
    mats = findMonomorphisms (domain a1) (domain b1)
    filt = filter (\m -> compose m b1 == a1 && compose m b2 == a2) mats
    select = case filt of
                [] -> error "(commutingMorphism) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(commutingMorphism) Error when commuting monomorphic morphisms (non unique commuting morphism)"

-- | Given the morphisms /k1 : X -> Y/, /s1 : X -> Z/,
-- /k2 : W -> Y/ and /s2 : W -> Z/, respectively,
-- creates the monomorphic morphism /x : Y -> Z/,
-- where the following diagram commutes:
--
-- @
--        k1
--     X ───▶Y
--      \\   / ▲
--     s1\\ /x  \\k2
--        ▼     \\
--        Z◀──── W
--           s2
-- @
--
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphismSameDomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameDomain k1 s1 k2 s2 = buildTypedGraphMorphism (codomain k1) (codomain s1) select
  where
    mats = findMonomorphisms (codomain k1) (codomain s1)
    filt = filter (\m -> compose k1 m == s1 && compose k2 m == s2) mats
    select = case filt of
                [] -> error "(commutingMorphismSameDomain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(commutingMorphismSameDomain) Error when commuting monomorphic morphisms (non unique commuting morphism)"

-- | Given the morphisms /k1 : Y -> X/, /s1 : Z -> X/,
-- /k2 : W -> Y/ and /s2 : W -> Z/, respectively,
-- creates the monomorphic morphism /a : X -> Y/,
-- where the following diagram commutes:
--
-- @
--        k1
--     X ◀─── Y
--     ▲     / ▲
--    s1\\   /x  \\k2
--       \\ ▼     \\
--        Z◀──── W
--           s2
-- @
--
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphismSameCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                              -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameCodomain k1 s1 k2 s2 = buildTypedGraphMorphism (domain k1) (domain s1) select
  where
    mats = findMonomorphisms (domain k1) (domain s1)
    filt = filter (\m -> compose m s1 == k1 && compose k2 m == s2) mats
    select = case filt of
                [] -> error "(commutingMorphismSameCodomain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(commutingMorphismSameCodomain) Error when commuting monomorphic morphisms (non unique commuting morphism)"
