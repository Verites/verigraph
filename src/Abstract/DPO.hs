{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Abstract.DPO
  (
  -- * Productions
    Production
  , production

  , left
  , right
  , nacs

  -- ** Application
  , dpo
  , comatch

  -- *** Application Conditions
  , DPO(..)
  , satsNacs
  , satsGluingAndNacs
  , satsGluingNacsBoth

  -- ** Manipulating
  , inverseWithoutNacs
  ) where

import Abstract.Morphism
import Abstract.AdhesiveHLR
import Abstract.Valid

data Production m = Production
  { left :: m
  , right :: m
  , nacs :: [m]
  }
  deriving (Show, Read)

production :: (DPO m, Eq (Obj m)) => m -> m -> [m] -> Production m
production = Production

instance (Morphism m, Valid m, Eq (Obj m)) => Valid (Production m) where
  valid (Production l r nacs) =
    valid l && valid r && all valid nacs &&
    domain l == domain r && all (==codomain l) (map domain nacs)

dpo :: AdhesiveHLR m => m -> Production m -> (m, m, m, m)
dpo m (Production l r _) =
  let (m', l') = poc m l
      (m'', r') = po m' r
  in (m',m'',l',r')

comatch :: AdhesiveHLR m => m -> Production m -> m
comatch m prod = let (_,m',_,_) = dpo m prod in m'

inverseWithoutNacs :: Production m -> Production m
inverseWithoutNacs p = Production (right p) (left p) []

class (AdhesiveHLR m, FindMorphism m) => DPO m where
  -- | Check just the gluing conditions for a match
  -- @inj@ only indicates the match, this function does not checks if the match is injective
  satsGluing :: Bool -> m -> Production m -> Bool

  -- | Check if the second morphism is monomorphic outside the image of the
  -- first morphism.
  partiallyMonomorphic :: m -> m -> Bool
{-# WARNING partiallyMonomorphic "Only necessary until 'partInjMatches' is corrected" #-}

satsNacs :: DPO m => Bool -> Production m -> m -> Bool
satsNacs nacInj rule m = all (==True) (map (satsFun m) (nacs rule))
  where
    satsFun = if nacInj then satsOneNacInj else satsOneNacPartInj

-- | Check gluing conditions and the NACs satisfability for a pair of matches
-- @inj@ only indicates the match, this function does not checks if the pair is injective
--
-- TODO: deprecate?
satsGluingNacsBoth :: DPO m => Bool -> Bool -> (Production m, m) -> (Production m, m) -> Bool
satsGluingNacsBoth nacInj inj (l,m1) (r,m2) =
  satsGluingAndNacs nacInj inj l m1 && satsGluingAndNacs nacInj inj r m2

-- | Check gluing conditions and the NACs satisfability for a match
-- @inj@ only indicates the match, this function does not checks if the match is injective
satsGluingAndNacs :: DPO m => Bool -> Bool -> Production m -> m -> Bool
satsGluingAndNacs nacInj inj rule m = gluingCond && nacsCondition
    where
        gluingCond    = satsGluing inj m rule
        nacsCondition = satsNacs nacInj rule m

satsOneNacInj :: FindMorphism m => m -> m -> Bool
satsOneNacInj m nac = all (==False) checkCompose
   where
      checkCompose = map (\x -> compose nac x == m) nacMatches
      nacMatches = matches MONO typeNac typeG
      typeNac = codomain nac
      typeG   = codomain m

satsOneNacPartInj :: DPO m => m -> m -> Bool
satsOneNacPartInj m nac = all (==False) check
   where
      check = map (partiallyMonomorphic nac) checkCompose
      checkCompose = filter (\x -> compose nac x == m) matches
      matches = partInjMatches nac m --generating some non partial injective matches
