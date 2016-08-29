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
    monomorphism :: m -> Bool
    epimorphism :: m -> Bool
    isomorphism :: m -> Bool

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

  findMonomorphisms :: Obj m -> Obj m -> [m]
  findMonomorphisms = findMorphisms Monomorphism

  -- | Finds matches __/q/__ .
  --
  --   Partially injective. (Injective out of __/m/__)
  --
  -- TODO: replace by data constructor @PartMono :: m -> MorphismType@?
  --
  -- TODO: what is the second argument??
  --
  -- TODO: properly explain partial injectivity
  partInjMatches :: m -> m -> [m]
