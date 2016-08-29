{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides definitions for the Double-Pushout approach to
-- High-Level Rewriting Systems.
module Abstract.DPO
  (
    Production
  , constructProduction

  , getLHS
  , getRHS
  , getNACs

  , DPOConfig(..)
  , MatchRestriction(..)
  , matchRestrictionToMorphismType
  , NacSatisfaction(..)

  -- ** Application

  -- *** Conditions
  -- | In order to apply a production with a particular match, some application
  -- conditions must be satisfied: the gluing condition and the negative
  -- application conditions (NACs). This section provides functions that test
  -- if such conditions are met.

  , DPO(..)
  , satisfiesGluingConditions
  , satisfiesNACs
  , satisfiesRewritingConditions
  , satisfyRewritingConditions

  -- *** Transformation
  -- | Given a production and a match for its left side, it may be possible
  -- to apply the production and obtain a transformation of the matched graph.
  -- This section provides functions that calculate such transformations.
  , findAllMatches
  , findApplicableMatches
  , calculateDPO
  , calculateComatch
  , rewrite

  -- ** Manipulation
  , invertProductionWithoutNacs
  , nacDownwardShift
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.Morphism
import           Abstract.Valid

-- | A Double-Pushout production.
--
-- Consists of two morphisms /'left' : K -> L/ and /'right' : K -> R/,
-- as well as a set of 'nacs' /L -> Ni/.
data Production m = Production
  { left  :: m   -- ^ The morphism /K -> L/ of a production
  , right :: m  -- ^ The morphism /K -> R/ of a production
  , nacs  :: [m] -- ^ The set of nacs /L -> Ni/ of a production
  }
  deriving (Eq, Show, Read)

-- | Returns the morphism /K -> L/ of the given production
getLHS :: Production m -> m
getLHS = left

-- | Returns the morphism /K -> R/ of the given production
getRHS :: Production m -> m
getRHS = right

-- | Returns the set of nacs /L -> Ni/ of the given production
getNACs :: Production m -> [m]
getNACs = nacs

-- | Construct a production from the morphism /l : K -> L/,
-- the morphism /r : K -> R/, and the nacs /L -> Ni/, respectively.
--
-- Note: this doesn't check that the production is valid.
constructProduction :: (DPO m, Eq (Obj m)) => m -> m -> [m] -> Production m
constructProduction = Production

-- | Obtain all matches from the production into the given object, even if they
-- aren't applicable.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findAllMatches :: (DPO m) => DPOConfig -> Production m -> Obj m -> [m]
findAllMatches config production =
  findMorphisms
    (matchRestrictionToMorphismType $ matchRestriction config)
    (codomain $ left production)


-- | Obtain the matches from the production into the given object that satisfiy the NACs
-- and gluing conditions.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findApplicableMatches :: (DPO m) => DPOConfig -> Production m -> Obj m -> [m]
findApplicableMatches config production obj =
  filter (satisfiesRewritingConditions config production) (findAllMatches config production obj)


instance (Morphism m, Valid m, Eq (Obj m)) => Valid (Production m) where
  valid (Production l r nacs) =
    monomorphism l && monomorphism r &&
    valid l && valid r && all valid nacs &&
    domain l == domain r && all (==codomain l) (map domain nacs)

-- | Given a match and a production, calculates the double-pushout diagram
-- for the corresponding transformation.
--
-- Given match /m : L -> G/ and the production /L ←l- K -r→ R/ such that
-- @'satisfiesRewritingConditions' _ _ p m == True@, returns /k/, /n/, /f/ and /g/ (respectively)
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
calculateDPO :: AdhesiveHLR m => m -> Production m -> (m, m, m, m)
calculateDPO m (Production l r _) =
  let (k, f) = calculatePushoutComplement m l
      (n, g) = calculatePushout k r
  in (k, n, f, g)

-- | Given a match and a production, calculate the calculateComatch for the
-- corresponding transformation.
--
-- Given match /m : L -> G/ and the production @p = /L ←l- K -r→ R/@ such that
-- @'satisfiesRewritingConditions' _ _ p m == True@, returns /n/ such that the following two
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
calculateComatch :: AdhesiveHLR m => m -> Production m -> m
calculateComatch m prod = let (_,m',_,_) = calculateDPO m prod in m'

-- | Given a match and a production, obtain the rewritten object.
--
-- @rewrite match production@ is equivalent to @'codomain' ('calculateComatch' match production)@
rewrite :: AdhesiveHLR m => m -> Production m -> Obj m
rewrite m prod =
  codomain (calculateComatch m prod)

-- | Discards the NACs of a production and inverts it.
invertProductionWithoutNacs :: Production m -> Production m
invertProductionWithoutNacs p = Production (right p) (left p) []

-- | Class for morphisms whose category is Adhesive-HLR, and which can be
-- used for double-pushout transformations.
class (AdhesiveHLR m, FindMorphism m) => DPO m where
  -- | Inverts a production, adjusting the NACs accordingly.
  -- Needs information of nac injective satisfaction (in second order)
  -- and matches injective.
  invertProduction :: DPO m => DPOConfig -> Production m -> Production m

  -- | Given a production /L ←l- K -r→ R/ and a NAC morphism /n : L -> N/, obtain
  -- a set of NACs /n'i : R -> N'i/ that is equivalent to the original NAC.
  shiftNacOverProduction :: DPO m => DPOConfig -> Production m -> m -> [m]

  -- TODO : Verify why this function continues to be used
  -- | Check if the second morphism is monomorphic outside the image of the
  -- first morphism.
  partiallyMonomorphic :: m -> m -> Bool
--{-# WARNING partiallyMonomorphic "Only necessary until 'partialInjectiveMatches' is corrected" #-}

-- | Verifies if the gluing conditions for a production /p/ are satisfied by a match /m/
satisfiesGluingConditions :: DPO m => DPOConfig -> Production m -> m -> Bool
satisfiesGluingConditions config production match =
  hasPushoutComplement (matchIsMono, match) (GenericMorphism, left production)
  where
    matchIsMono =
      matchRestrictionToMorphismType (matchRestriction config)

-- | True if the given match satisfies all NACs of the given production.
satisfiesNACs :: DPO m => DPOConfig -> Production m -> m -> Bool
satisfiesNACs config production match =
  all (satisfiesSingleNac config match) (nacs production)

-- TODO: deprecate? why do we need this __here__?
-- | Check gluing conditions and the NACs satisfaction for a pair of matches
-- @inj@ only indicates if the match is injective, this function does not checks it
satisfyRewritingConditions :: DPO m => DPOConfig -> (Production m, m) -> (Production m, m) -> Bool
satisfyRewritingConditions config (l,m1) (r,m2) =
  satisfiesRewritingConditions config l m1 && satisfiesRewritingConditions config r m2


-- | True if the given match satisfies the gluing condition and NACs of the
-- given production.
satisfiesRewritingConditions :: DPO m => DPOConfig -> Production m -> m -> Bool
satisfiesRewritingConditions config production match =
  satisfiesGluingConditions config production match && satisfiesNACs config production match


satisfiesSingleNac :: DPO m => DPOConfig -> m -> m -> Bool
satisfiesSingleNac config match nac =
  let nacMatches =
        case nacSatisfaction config of
          MonomorphicNAC ->
            findMonomorphisms (codomain nac) (codomain match)
          PartiallyMonomorphicNAC ->
            partialInjectiveMatches nac match
      commutes nacMatch =
        compose nac nacMatch == match
  in
    not $ any commutes nacMatches

-- TODO: Is this really a DPO feature?
-- | Given a morphism /m : L -> L'/ and a NAC /n : L -> N/, obtains
-- an equivalent set of NACs /n'i : L' -> N'i/ that is equivalent to the
-- original NAC.
nacDownwardShift :: EpiPairs m => DPOConfig -> m -> m -> [m]
nacDownwardShift config m n = newNacs
  where
    pairs = calculateCommutativeSquaresAlongMonomorphism (n,True) (m, matchRestriction config == MonoMatches)
    newNacs = map snd pairs
