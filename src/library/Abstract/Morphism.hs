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
--
-- TODO: rename following Haskell naming conventions (not all caps)
data PROP
  = ALL   -- ^ Finds all possible matches
  | MONO  -- ^ Finds only monomorphic matches
  | EPI   -- ^ Finds only epimorphic matches
  | ISO   -- ^ Finds only isomorphic matches
  deriving (Show)

-- | Receives a bool indicating injective or arbitrary match,
-- converts it to data PROP
injectiveBoolToProp :: Bool -> PROP
injectiveBoolToProp True = MONO
injectiveBoolToProp False = ALL

class Morphism m => FindMorphism m where
  -- | Finds matches __/m/__
  --
  --   Injective, surjective, isomorphic or all possible matches
  --
  -- TODO: rename to allMorphismsBetween?
  findMorphisms :: PROP -> Obj m -> Obj m -> [m]

  -- | Finds matches __/q/__ .
  --
  --   Partially injective. (Injective out of __/m/__)
  --
  -- TODO: replace by data constructor @PartMono :: m -> PROP@?
  --
  -- TODO: what is the second argument??
  --
  -- TODO: properly explain partial injectivity
  partInjMatches :: m -> m -> [m]
