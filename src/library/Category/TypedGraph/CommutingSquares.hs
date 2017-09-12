module Category.TypedGraph.CommutingSquares (
    commutingMorphism
  , commutingMorphismSameDomain
  , commutingMorphismSameCodomain
  ) where

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Category.TypedGraph                ()
import           Data.TypedGraph.Morphism

---- All functions in this file search the unique morphism in some diagram.
-- Since it is for build productions, we consider only monomorphic morphisms.

-- All functions has three possible outputs:
-- 1. An unique and monomorphic morphism that commutes with the diagram.
-- 2. An error: if the diagram do not has the commuting morphism.
-- 3. An error: if two or more commuting morphisms are found, which is probably an implementation error.

output :: String -> [TypedGraphMorphism a b] -> TypedGraphMorphism a b
output fname morphisms =
  case morphisms of
    [] -> error $ "("++fname++") Error when commuting monomorphic morphisms (must be generating an invalid rule)"
    [x] -> x
    (_:_:_) -> error $ "("++fname++") Error when commuting monomorphic morphisms (non unique commuting morphism)"

-- | Given the morphisms /a1 : X -> A/, /b1 : Y -> B/,
-- /a2 : X -> B/ and /b2 : Y -> B/, respectively,
-- creates the monomorphic morphism /x : X -> Y/,
-- where the following diagram commutes:
--
-- @
--        a1
--     A ◀─── X
--     ▲     /  \\
--    b1\\   /x  \\a2
--       \\ ▼    ▼
--        Y ────▶B
--           b2
-- @
commutingMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                  -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphism a1 b1 a2 b2 = buildTypedGraphMorphism (domain a1) (domain b1) select
  where
    mats = findMonomorphisms (domain a1) (domain b1)
    filt = filter (\m -> b1 <&> m == a1 && b2 <&> m == a2) mats
    select = mapping $ output "commutingMorphism" filt

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
commutingMorphismSameDomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameDomain k1 s1 k2 s2 = buildTypedGraphMorphism (codomain k1) (codomain s1) select
  where
    mats = findMonomorphisms (codomain k1) (codomain s1)
    filt = filter (\m -> m <&> k1 == s1 && m <&> k2 == s2) mats
    select = mapping $ output "commutingMorphismSameDomain" filt

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
commutingMorphismSameCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                              -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameCodomain k1 s1 k2 s2 = buildTypedGraphMorphism (domain k1) (domain s1) select
  where
    mats = findMonomorphisms (domain k1) (domain s1)
    filt = filter (\m -> s1 <&> m == k1 && m <&> k2 == s2) mats
    select = mapping $ output "commutingMorphismSameCodomain" filt
