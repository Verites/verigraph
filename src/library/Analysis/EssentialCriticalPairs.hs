{-|
Description : Essential Critical Pairs generator.

This module provides functions that generate all essential critical
pairs between two productions. The essential critical pairs are
classified with 'CriticalPairType', but each pair can be only a
'DeleteUse'.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Analysis.EssentialCriticalPairs
 ( namedEssentialCriticalPairs,
   findAllEssentialDeleteUse
   ) where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import Util.Monad

type NamedRule cat morph = (String, Production cat morph)
type NamedCriticalPairs morph = (String,String, [CriticalPair morph])

-- | Returns the Essential Critical Pairs with rule names
namedEssentialCriticalPairs :: (EM'PairFactorizable cat morph, DPO cat morph, Complete cat morph, InitialPushout cat morph) =>
  [NamedRule cat morph] -> cat [NamedCriticalPairs morph]
namedEssentialCriticalPairs namedRules =
  mapM (uncurry findEssentialCriticalPairs') [ (a,b) | a <- namedRules, b <- namedRules ]
    where findEssentialCriticalPairs' (n1,r1) (n2,r2) = (n1, n2,) <$> findEssentialCriticalPairs r1 r2

-- | Finds all Essential Critical Pairs between two given Productions
findEssentialCriticalPairs :: (EM'PairFactorizable cat morph, DPO cat morph, Complete cat morph, InitialPushout cat morph) =>
  Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findEssentialCriticalPairs = findAllEssentialDeleteUse

-- | Get all essential delete-use and organize them in a list of 'CriticalPair'.
findAllEssentialDeleteUse :: (EM'PairFactorizable cat morph, DPO cat morph, Complete cat morph, InitialPushout cat morph) =>
  Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findAllEssentialDeleteUse p1 p2 = do
  conflicts <- filterM isEssentialDeleteUse =<< findPotentialEssentialCPs p1 p2
  return [ CriticalPair (m1,m2) Nothing Nothing DeleteUse | (_,_,m1,m2) <- conflicts ]

-- | Generates all "epi" pairs for essential delete-use,
-- returns part of the initial pushout to avoid recalculations.
findPotentialEssentialCPs :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph, InitialPushout cat morph) => Production cat morph -> Production cat morph -> cat [(morph,morph, morph,morph)]
findPotentialEssentialCPs p1 p2 = do
  (_, l1', c) <- calculateInitialPushout (leftMorphism p1)
  pairs <- findJointlyEpicPairs (matchMorphism @cat, codomain l1') (matchMorphism @cat, leftObject p2)
  shiftedPairs <- forM pairs $ \(e1, e2) -> do
    (d1', m1) <- calculatePushoutAlongRN c e1
    let m2 = d1' <&> e2
    return (l1', c, m1, m2)
  filterM isValidPairOfMatches shiftedPairs
  where
    isValidPairOfMatches (_, _, m1, m2) = satisfiesRewritingConditions p1 m1 `andM` satisfiesRewritingConditions p2 m2

-- | A pair of monomorphic matches (with precalcultated initial pushout (l1',c) elements)
-- is an essential delete use when the
-- pullback of the composition of the c (from the initial pushout) with m1
-- and m2 is a pushout (guaranteed by the construction of 'findPotentialEssentialCPs')
-- and does not exist morphism from S1 to B that commutes.
isEssentialDeleteUse :: forall cat morph. (DPO cat morph, Complete cat morph) => (morph,morph, morph,morph) -> cat Bool
isEssentialDeleteUse (l1', c, m1, m2) = do
  (_, o1) <- calculatePullback (m1 <&> c) m2
  null <$> findCospanCommuters (matchMorphism @cat) o1 l1'
