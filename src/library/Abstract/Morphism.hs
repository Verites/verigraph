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

-- | Restriction to morphisms that may be considered when searching for them.
data MorphismRestriction
  = AnyMorphisms -- ^ Allows all morphisms
  | MonoMorphisms -- ^ Allows only monomorphisms
  | EpiMorphisms -- ^ Allows only epimorphisms
  | IsoMorphisms -- ^ Allows only isomorphisms
  deriving (Show)


class Morphism m => FindMorphism m where
  -- | Finds matches __/m/__
  --
  --   Injective, surjective, isomorphic or all possible matches
  findMorphisms :: MorphismRestriction -> Obj m -> Obj m -> [m]

  -- | Finds matches __/q/__ .
  --
  --   Partially injective. (Injective out of __/m/__)
  --
  -- TODO: replace by data constructor @PartMono :: m -> MorphismRestriction@?
  --
  -- TODO: what is the second argument??
  --
  -- TODO: properly explain partial injectivity
  partInjMatches :: m -> m -> [m]
