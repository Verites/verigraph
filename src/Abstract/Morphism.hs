{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Abstract.Morphism where

class (Eq m) => Morphism m where
    type Obj m :: *
    -- | TODO: what is the order of composition???
    compose  :: m -> m -> m
    domain   :: m -> Obj m
    codomain :: m -> Obj m
    id       :: Obj m -> m
    monomorphism :: m -> Bool
    epimorphism :: m -> Bool
    isomorphism :: m -> Bool

-- | Data type definition to choose specifics propertys of a morphism
--
--     [@ALL@]  Finds all possible matches
--     [@MONO@] Finds only monomorphics matches
--     [@EPI@]  Finds only epimorphics matches
--     [@ISO@]  Finds only isomorphics matches
--
-- TODO: rename following Haskell naming conventions (not all caps)
data PROP = ALL | MONO | EPI | ISO

class Morphism m => FindMorphism m where
  -- | Finds matches __/m/__
  --
  --   Injective, surjective, isomorphic or all possible matches
  --
  -- TODO: rename to allMorphismsBetween?
  matches :: PROP -> Obj m -> Obj m -> [m]

  -- | Finds matches __/q/__ .
  --
  --   Partially injective. (Injective out of __/m/__)
  --
  -- TODO: replace by data constructor @PartMono :: m -> PROP@?
  -- TODO: what is the second argument??
  partInjMatches :: m -> m -> [m]
