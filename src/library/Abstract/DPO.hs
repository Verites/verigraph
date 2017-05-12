{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides definitions for the Double-Pushout approach to
-- High-Level Rewriting Systems.
module Abstract.DPO
  ( Production
  , buildProduction
  , getLHS
  , getRHS
  , getNACs

  , grammar
  , Grammar
  , ObjectFlow (..)
  , RuleSequence
  , NamedProduction
  , getProductionName
  , getProduction
  , start
  , constraints
  , rules
  , findProduction
  , reachableGraphs
  , addReachableGraphs

  , Derivation(..)
  , generateDerivation

  , Process(..)
  , GenerateProcess(..)

  , MorphismsConfig(..)
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
import           Abstract.DPO.Core
import           Abstract.DPO.Derivation
import           Abstract.DPO.Process

-- TODO: deprecate? why do we need this __here__?
-- | Check gluing conditions and the NACs satisfaction for a pair of matches
-- @inj@ only indicates if the match is injective, this function does not checks it
satisfyRewritingConditions :: DPO m => MorphismsConfig -> (Production m, m) -> (Production m, m) -> Bool
satisfyRewritingConditions conf (l,m1) (r,m2) =
  satisfiesRewritingConditions conf l m1 && satisfiesRewritingConditions conf r m2

-- TODO: Is this really a DPO feature?
-- | Given a morphism /m : L -> L'/ and a NAC /n : L -> N/, obtains
-- an equivalent set of NACs /n'i : L' -> N'i/ that is equivalent to the
-- original NAC.
nacDownwardShift :: EpiPairs m => MorphismsConfig -> m -> m -> [m]
nacDownwardShift conf m n = newNacs
  where
    pairs = calculateCommutativeSquaresAlongMonomorphism (n,True) (m, matchRestriction conf == MonoMatches)
    newNacs = map snd pairs
