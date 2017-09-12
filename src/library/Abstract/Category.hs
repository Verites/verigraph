{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Abstract.Category
  (
    Category(..)
  , isMonic
  , isEpic
  , isIsomorphism
  , Span
  , Cospan
  ) where


{- | Type class for representing categories in Verigraph.

Each category is defined by the type @morph@ of its morphisms. Furthermore,
there is an associated type of objects @Obj morph@.
-}
class (Eq morph) => Category morph where
  -- | Data type that represents objects of the category
  type Obj morph :: *

  -- | Morphism composition operator.
  --
  -- Given morphisms \(f:B \to C\), and \(g:A\to B\), returns \(f \circ g : A\to C\),
  -- i.e. the order of arguments is the same as in function composition.
  (<&>)    :: morph -> morph -> morph

  -- | Given an object \(A\), return its identity morphism \(id_A : A\to A\).
  identity :: Obj morph -> morph
  -- | Given a morphism \(f:A \to B\), return the object \(A\).
  domain   :: morph -> Obj morph
  -- | Given a morphism \(f:A \to B\), return the object \(B\).
  codomain :: morph -> Obj morph

  -- | Data type defining the different classes of morphism for this category.
  data MorphismClass morph :: *
  -- | Class containing all morphisms of the category.
  anyMorphism :: MorphismClass morph
  -- | Class containing all monomorphisms of the category.
  monic :: MorphismClass morph
  -- | Class containing all epimorphisms of the category.
  epic :: MorphismClass morph
  -- | Class containing all isomorphisms of the category.
  iso :: MorphismClass morph

  -- | Check if a given morphism belongs to the given class of morphisms.
  belongsToClass :: morph -> MorphismClass morph -> Bool

  -- | Check if a given class of morphism is contained in another.
  isSubclassOf :: MorphismClass morph -> MorphismClass morph -> Bool

-- | Check if a given morphism is monic.
isMonic :: forall morph. Category morph => morph -> Bool
isMonic = (`belongsToClass` monic @morph)
{-# INLINE isMonic #-}

-- | Check if a given morphism is epic.
isEpic :: forall morph. Category morph => morph -> Bool
isEpic = (`belongsToClass` epic @morph)
{-# INLINE isEpic #-}

-- | Check if a given morphism is an isomorphism.
isIsomorphism :: forall morph. Category morph => morph -> Bool
isIsomorphism = (`belongsToClass` iso @morph)
{-# INLINE isIsomorphism #-}

-- | A span is a pair of morphisms with same domain (e.g. \(A \leftarrow C \to B \)).
type Span morph = (morph, morph)

-- | A cospan is a pair of morphisms with same codomain (e.g. \(A \to C \leftarrow B \)).
type Cospan morph = (morph, morph)
