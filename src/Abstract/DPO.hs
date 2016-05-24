{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides definitions for the Double-Pushout approach to
-- High-Level Rewriting Systems.
module Abstract.DPO
  (
    Production
  , production

  , left
  , right
  , nacs

  -- ** Application

  -- *** Conditions
  -- | In order to apply a production with a particular match, some application
  -- conditions must be satisfied: the gluing condition and the negative
  -- application conditions (NACs). This section provides functions that test
  -- if such conditions are met.

  , DPO(..)
  , satsNacs
  , satsGluingAndNacs
  , satsGluingNacsBoth


  -- *** Transform
  -- | Given a production and a match for its left side, it may be possible
  -- to apply the production and obtain a transformation of the matched graph.
  -- This section provides functions that calculate such transformations.
  , dpo
  , comatch


  -- ** Manipulating
  , inverseWithoutNacs
  , downwardShift
  ) where

import Abstract.Morphism
import Abstract.AdhesiveHLR
import Abstract.Valid

-- | A Double-Pushout production.
--
-- Consists of two morphisms /'left' : K -> L/ and /'right' : K -> R/,
-- as well as a set of 'nacs' /L -> Ni/.
data Production m = Production
  { left :: m   -- ^ The morphism /K -> L/ of a production
  , right :: m  -- ^ The morphism /K -> R/ of a production
  , nacs :: [m] -- ^ The set of nacs /L -> Ni/ of a production
  }
  deriving (Show, Read)

-- | Construct a production from the morphism /l : K -> L/,
-- the morphism /r : K -> R/, and the nacs /L -> Ni/, respectively.
--
-- Note: this doesn't check that the production is valid.
production :: (DPO m, Eq (Obj m)) => m -> m -> [m] -> Production m
production = Production

instance (Morphism m, Valid m, Eq (Obj m)) => Valid (Production m) where
  valid (Production l r nacs) =
    monomorphism l && monomorphism r && --check if is needed
    valid l && valid r && all valid nacs &&
    domain l == domain r && all (==codomain l) (map domain nacs)

-- | Given a match and a production, calculate the double-pushout diagram
-- for the corresponding transformation.
--
-- Given match /m : L -> G/ and the production /L ←l- K -r→ R/ such that
-- @'satsGluingAndNacs' _ _ p m == True@, returns /k/, /n/, /f/ and /g/ (respectively)
-- such that the following two squares are pushouts.
--
-- @
--       l        r
--    L◀──────K──────▶R
--    │       │       │
--  m │       │ k     │ n
--    ▼       ▼       ▼
--    G◀──────D──────▶H
--         f     g
-- @
--
-- Note: this doesn't test whether the match is for the actual production,
-- nor if the match satisfies all application conditions.
dpo :: AdhesiveHLR m => m -> Production m -> (m, m, m, m)
dpo m (Production l r _) =
  let (k, f) = poc m l
      (n, g) = po k r
  in (k, n, f, g)

-- | Given a match and a production, calculate the comatch for the
-- corresponding transformation.
--
-- Given match /m : L -> G/ and the production @p = /L ←l- K -r→ R/@ such that
-- @'satsGluingAndNacs' _ _ p m == True@, returns /n/ such that the following two
-- squares are pushouts.
--
-- @
--       l        r
--    L◀──────K──────▶R
--    │       │       │
--  m │       │       │ n
--    ▼       ▼       ▼
--    G◀──────D──────▶H
-- @
--
-- Note: this doesn't test whether the match is for the actual production,
-- nor if the match satisfies all application conditions.
comatch :: AdhesiveHLR m => m -> Production m -> m
comatch m prod = let (_,m',_,_) = dpo m prod in m'

-- | Discards the NACs of a production and inverts it.
inverseWithoutNacs :: Production m -> Production m
inverseWithoutNacs p = Production (right p) (left p) []

-- | Class for morphisms whose category is Adhesive-HLR, and which can be
-- used for double-pushout transformations.
class (AdhesiveHLR m, FindMorphism m) => DPO m where
  -- | True if the given match satisfies the gluing condition for the given production.
  -- This function does not need all production, just the left morphism.
  --
  -- Bool only indicates if the match is injective,
  -- in the case of unknown use False
  satsGluing :: Bool -> m -> m -> Bool
  
  -- | Checks if the match if free oh have a dangling edge that unable the production
  freeDanglingEdges :: m -> m -> Bool
  
  -- | Inverts a production, adjusting the NACs accordingly
  inverse :: DPO m => Bool -> Production m -> Production m
  
  -- | Given a production /L ←l- K -r→ R/ and a NAC morphism /n : L -> N/, obtain
  -- a set of NACs /n'i : R -> N'i/ that is equivalent to the original NAC.
  --
  -- TODO: review name
  --
  -- TODO: what's the first parameter?
  shiftLeftNac :: DPO m => Bool -> Production m -> m -> [m]
  
  -- | Check if the second morphism is monomorphic outside the image of the
  -- first morphism.
  partiallyMonomorphic :: m -> m -> Bool
--{-# WARNING partiallyMonomorphic "Only necessary until 'partInjMatches' is corrected" #-}

-- | True if the given match satisfies all NACs of the given production.
--
-- If the first and second arguments are false, only considers partially injective morphisms.
-- Otherwise, considers injective morphisms /Ni -> G/.
satsNacs :: DPO m => Bool -> Bool -> Production m -> m -> Bool
satsNacs nacInj inj rule m = all (==True) (map (satsFun m) (nacs rule))
  where
    satsFun = if not nacInj && not inj then satsOneNacPartInj else satsOneNacInj

-- | Check gluing conditions and the NACs satisfaction for a pair of matches
-- @inj@ only indicates if the match is injective, this function does not checks it
--
-- TODO: deprecate? why do we need this __here__?
satsGluingNacsBoth :: DPO m => Bool -> Bool -> (Production m, m) -> (Production m, m) -> Bool
satsGluingNacsBoth nacInj inj (l,m1) (r,m2) =
  satsGluingAndNacs nacInj inj l m1 && satsGluingAndNacs nacInj inj r m2

-- | True if the given match satisfies the gluing condition and NACs of the
-- given production.
--
-- If the first argument is true, only considers injective morphisms /Ni -> G/.
-- Otherwise, considers partially injective morphisms.
--
-- TODO: what is the second argument?
satsGluingAndNacs :: DPO m => Bool -> Bool -> Production m -> m -> Bool
satsGluingAndNacs nacInj inj rule m = gluingCond && nacsCondition
    where
        gluingCond    = satsGluing inj m (left rule)
        nacsCondition = satsNacs nacInj inj rule m

satsOneNacInj :: FindMorphism m => m -> m -> Bool
satsOneNacInj m nac = null checkCompose
   where
      checkCompose = filter (\x -> compose nac x == m) nacMatches
      nacMatches = matches MONO typeNac typeG
      typeNac = codomain nac
      typeG   = codomain m

satsOneNacPartInj :: DPO m => m -> m -> Bool
satsOneNacPartInj m nac = null checkCompose
   where
      checkCompose = filter (\x -> compose nac x == m) matches
      matches = partInjMatches nac m

-- | Given a morphism /m : L -> L'/ and a NAC /n : L -> N/, obtains
-- an equivalent set of NACs /n'i : L' -> N'i/ that is equivalent to the
-- original NAC.
downwardShift :: EpiPairs m => Bool -> m -> m -> [m]
downwardShift inj m n = newNacs
  where
    pairs = commutingPairsAlt (n,True) (m,inj) --Bool indicates injective
    newNacs = map snd pairs
