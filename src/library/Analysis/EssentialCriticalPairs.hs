{-|
Description : Essential Critical Pairs generator.

This module provides functions that generate all essential critical
pairs between two productions. The essential critical pairs are
classified with 'CriticalPairType', but each pair can be only a
'DeleteUse'.
-}
module Analysis.EssentialCriticalPairs
  ( findEssentialCriticalPairs
  , findAllEssentialDeleteUse
  ) where

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs


-- | Finds all Essential Critical Pairs between two given Productions
findEssentialCriticalPairs :: (E'PairCofinitary morph, DPO morph, MInitialPushout morph, Complete morph, Cocomplete morph) =>
  MorphismsConfig morph -> Production morph -> Production morph -> [CriticalPair morph]
findEssentialCriticalPairs = findAllEssentialDeleteUse

-- | Get all essential delete-use and organize them in a list of 'CriticalPair'.
findAllEssentialDeleteUse :: (E'PairCofinitary morph, DPO morph, MInitialPushout morph, Complete morph, Cocomplete morph) =>
  MorphismsConfig morph -> Production morph -> Production morph -> [CriticalPair morph]
findAllEssentialDeleteUse conf p1 p2 =
  map (\(_,_,m1,m2) -> CriticalPair (m1,m2) Nothing Nothing DeleteUse) essentialCPs
  where
    essentialCPs =
      filter
        (isEssentialDeleteUse conf)
        (findPotentialEssentialCPs conf p1 p2)

-- | Generates all "epi" pairs for essential delete-use,
-- returns part of the initial pushout to avoid recalculations.
findPotentialEssentialCPs :: (DPO morph, E'PairCofinitary morph, MInitialPushout morph, Cocomplete morph) => MorphismsConfig morph -> Production morph -> Production morph -> [(morph,morph, morph,morph)]
findPotentialEssentialCPs conf p1 p2 = satisfyingPairs
  where
    (_,l1',c) = calculateMInitialPushout (leftMorphism p1)
    pairs = findJointSurjections (matchRestriction conf, codomain l1') (matchRestriction conf, leftObject p2)
    shiftedPairs =
      map
        (\(e1,e2) ->
          let (m1,d1') = calculatePushout e1 c
              m2 = d1' <&> e2
           in (l1', c, m1, m2)
        )
      pairs
    satisfyingPairs = filter (\(_,_,m1,m2) -> satisfiesRewritingConditions conf p1 m1 && satisfiesRewritingConditions conf p2 m2) shiftedPairs

-- | A pair of monomorphic matches (with precalcultated initial pushout (l1',c) elements)
-- is an essential delete use when the
-- pullback of the composition of the c (from the initial pushout) with m1
-- and m2 is a pushout (guaranteed by the construction of 'findPotentialEssentialCPs')
-- and does not exist morphism from S1 to B that commutes.
isEssentialDeleteUse :: (DPO morph, Complete morph) => MorphismsConfig morph -> (morph,morph, morph,morph) -> Bool
isEssentialDeleteUse conf (l1',c,m1,m2) = null commuting
  where
    (_,o1) = calculatePullback (m1 <&> c) m2
    alls1 = findMorphismsFromDomains conf o1 l1'
    commuting = filter (\s1 -> l1' <&> s1 == o1) alls1

findMorphismsFromDomains :: FindMorphism morph => MorphismsConfig morph -> morph -> morph -> [morph]
findMorphismsFromDomains conf  a b =
  findMorphisms (matchRestriction conf) (domain a) (domain b)
