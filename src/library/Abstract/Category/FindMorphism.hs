{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Abstract.Category.FindMorphism 
  ( FindMorphism(..)
  , findAllMorphisms
  , findMonomorphisms
  , findEpimorphisms
  , findIsomorphisms
  ) where

import Abstract.Category
import Base.Isomorphic

{- Type class for categories with finite and computable Hom-sets. -}
class Category morph => FindMorphism morph where
  -- | Find all morphisms between the given objects that belong to the given class.
  findMorphisms :: MorphismClass morph -> Obj morph -> Obj morph -> [morph]

  -- | Given a span \(B \overset{f}{\leftarrow} A \overset{g}{\to} C\), find
  -- all morphisms \(h : B \to C\), of the given class, such that \(g = h \circ f\).
  --
  -- @
  --      f
  --   A ────▶ B
  --    ╲      │
  --   g ╲     │ h
  --      ╲    ▼
  --       ╲─▶ C
  -- @
  findSpanCommuters :: MorphismClass morph -> morph -> morph -> [morph]

  -- | Given a cospan \(B \overset{f}{\to} A \overset{g}{\leftarrow} C\), find
  -- all morphisms \(h : B \to C\), of the given class, such that \(f = g \circ h\).
  --
  -- @
  --       f
  --    B ────▶ A
  --    │       ▲
  --  h │      ╱
  --    ▼     ╱ g
  --    C ───╱
  -- @
  findCospanCommuters :: MorphismClass morph -> morph -> morph -> [morph]

  -- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
  -- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
  -- not be empty.
  --
  -- __WARNING:__ since such a morphism may not exist, this may return an invalid morphism.
  induceSpanMorphism :: [morph] -> [morph] -> morph

-- | Find all morphisms between the given objects.
findAllMorphisms :: forall morph. FindMorphism morph => Obj morph -> Obj morph -> [morph]
findAllMorphisms = findMorphisms (anyMorphism @morph)

-- | Find all monomorphisms between the given objects.
findMonomorphisms :: forall morph. FindMorphism morph => Obj morph -> Obj morph -> [morph]
findMonomorphisms = findMorphisms (monic @morph)

-- | Find all epimorphisms between the given objects.
findEpimorphisms :: forall morph. FindMorphism morph => Obj morph -> Obj morph -> [morph]
findEpimorphisms = findMorphisms (epic @morph)

-- | Find all isomorphisms between the given objects.
findIsomorphisms :: forall morph. FindMorphism morph => Obj morph -> Obj morph -> [morph]
findIsomorphisms = findMorphisms (iso @morph)


instance {-# OVERLAPPABLE #-} (FindMorphism morph, Iso (Obj morph)) => Iso morph where
  f ~= g = not . null $
    [ (domainIso, codomainIso)
      | codomainIso <- findMorphisms iso (codomain f) (codomain g)
      , domainIso <- findCospanCommuters iso (codomainIso <&> f) g ]
