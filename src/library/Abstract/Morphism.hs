{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Abstract.Morphism where

class (Eq m) => Morphism m where
    type Obj m :: *
    -- | Apply the first argument first (compose f g = g . f)
    compose  :: m -> m -> m
    domain   :: m -> Obj m
    codomain :: m -> Obj m
    id       :: Obj m -> m
    isMonomorphism :: m -> Bool
    isEpimorphism :: m -> Bool
    isIsomorphism :: m -> Bool

-- | Enum for the types of morphisms that can be used / found
data MorphismType
  = GenericMorphism
  | Monomorphism
  | Epimorphism
  | Isomorphism
  deriving (Show, Enum)


class Morphism m => FindMorphism m where
  -- | Given a type __/t/__ of @MorphismType@ and two objects __/A/__ and __/B/__, it finds all the matches
  -- m : A -> B in which m is of the type t
  findMorphisms :: MorphismType -> Obj m -> Obj m -> [m]

  -- | Given two objects A and B, finds all monomorphisms from A to B
  findMonomorphisms :: Obj m -> Obj m -> [m]
  findMonomorphisms = findMorphisms Monomorphism

  -- | Given two objects A and B, finds all epimorphisms from A to B
  findEpimorphisms :: Obj m -> Obj m -> [m]
  findEpimorphisms = findMorphisms Epimorphism

  -- | Given two objects A and B, finds all isomorphisms from A to B
  findIsomorphisms :: Obj m -> Obj m -> [m]
  findIsomorphisms = findMorphisms Isomorphism

  -- | Given two objects A and B, finds all morphisms from A to B
  findAllMorphisms :: Obj m -> Obj m -> [m]
  findAllMorphisms = findMorphisms GenericMorphism

  -- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
  -- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
  -- not be empty.
  induceSpanMorphism :: [m] -> [m] -> m

  -- TODO: properly explain partial injectivity
  -- TODO: replace by data constructor @PartMono :: m -> MorphismType@?
  -- | Given a NAC /n : L -> N / and a match /m : L -> G/, finds the morphisms
  -- from /N/ to /G/ that are injective out of the image of /n/
  partialInjectiveMatches :: m -> m -> [m]
