{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Analysis.ConcurrentRules
  ( CRDependencies (..)
  , allConcurrentRules
  , maxConcurrentRules
  ) where

import           Data.Maybe                 (mapMaybe)

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.Finitary
import           Abstract.Constraint
import           Abstract.Rewriting.DPO
import           Analysis.CriticalSequence  (findTriggeredCriticalSequences,
                                             getCriticalSequenceComatches)
import           Base.Cardinality
import           Base.Valid

data CRDependencies = AllOverlapings | OnlyDependency

-- | Generates the Concurrent Rules for a given list of Productions following the order of the elements in the list.
allConcurrentRules :: (DPO morph, E'PairCofinitary morph, Eq (Obj morph), Valid morph) => CRDependencies -> MorphismsConfig morph
                    -> [Constraint morph] -> [Production morph] -> [Production morph]
allConcurrentRules _ _ _ [] = []
allConcurrentRules _ _ _ [x] = [x]
allConcurrentRules dep conf constraints (x:xs) = concatMap (crs x) (allCRs xs)
  where
    crs = concurrentRules dep conf constraints
    allCRs = allConcurrentRules dep conf constraints

-- | Generates the Concurrent Rules with the least disjoint E'PairCofinitary (E'PairCofinitary with the least cardinality) for a given list of Productions
-- (following the order of the elements in the list).
maxConcurrentRules :: (DPO morph, E'PairCofinitary morph, Eq (Obj morph), Valid morph, Cardinality (Obj morph))
                  => CRDependencies -> MorphismsConfig morph -> [Constraint morph] -> [Production morph] -> [Production morph]
maxConcurrentRules _ _ _ [] = []
maxConcurrentRules _ _ _ [r] = [r]
maxConcurrentRules dep conf constraints (r:rs) = concat $ concatRule r (maxConcurrentRules dep conf constraints rs)
  where
    concatRule rule subMaxRule = case subMaxRule of
      [] -> []
      xs -> map (maxConcurrentRuleForLastPairs dep conf constraints rule) xs

concurrentRules :: (DPO morph, E'PairCofinitary morph) => CRDependencies -> MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [Production morph]
concurrentRules dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
  in mapMaybe (concurrentRuleForPair conf constraints c n) epiPairs

maxConcurrentRuleForLastPairs :: (DPO morph, E'PairCofinitary morph, Cardinality (Obj morph)) => CRDependencies -> MorphismsConfig morph -> [Constraint morph] ->
  Production morph -> Production morph -> [Production morph]
maxConcurrentRuleForLastPairs dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
      maxPair = last (epiPairsForConcurrentRule dep conf constraints c n)
      sizeOfMaxPair = cardinality $ codomain (fst maxPair)
      maxPairs = filter (\(e,_) -> cardinality (codomain e) == sizeOfMaxPair) epiPairs
      maxRule = if null epiPairs
        then []
        else mapMaybe (concurrentRuleForPair conf constraints c n) maxPairs
  in maxRule

epiPairsForConcurrentRule :: (DPO morph, E'PairCofinitary morph)
  => CRDependencies -> MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [(morph,morph)]
-- it only considers triggered dependencies because is the most intuitive and natural behaviour expected until now.
epiPairsForConcurrentRule OnlyDependency conf constraints c n =
  let dependencies = map getCriticalSequenceComatches (findTriggeredCriticalSequences conf constraints c n)
      validDependency (lp, _) = satisfiesAllConstraints (codomain lp) constraints
  in filter validDependency dependencies

epiPairsForConcurrentRule AllOverlapings conf constraints c n =
  let allPairs = findJointSurjections (matchRestriction conf, rightObject c) (matchRestriction conf, leftObject n)
      isValidPair (lp, rp) = satisfiesAllConstraints (codomain lp) constraints &&
        satisfiesGluingConditions conf (invertProductionWithoutNacs c) lp && satisfiesRewritingConditions conf n rp
  in filter isValidPair allPairs

concurrentRuleForPair :: forall morph. (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> (morph,morph) -> Maybe (Production morph)
concurrentRuleForPair conf constraints c n pair = if invalidSides then Nothing else Just (Production l r (dmc ++ lp))
  where
    pocC = calculatePushoutComplementAlongM (rightMorphism c) (fst pair)
    pocN = calculatePushoutComplementAlongM (leftMorphism n) (snd pair)
    poC = calculatePushoutAlongM (leftMorphism c) (fst pocC)
    poN = calculatePushoutAlongM (rightMorphism n) (fst pocN)
    pb = calculatePullbackAlongM (snd pocC) (snd pocN)
    l = fst poC <&> fst pb
    r = fst poN <&> snd pb
    dmc = filter validNac $ concatMap (nacDownwardShift conf (snd poC)) (nacs c)
    inverseP = Production (snd pocC) (fst poC) []
    den = filter validNac $ concatMap (nacDownwardShift conf (snd pair)) (nacs n)
    lp = filter validNac $ concatMap (shiftNacOverProduction conf inverseP) den
    -- Filters that are not in the default algorithm, useful when dealing with injective morphisms only
    validNac nac = not (matchRestriction conf `isSubclassOf` monic @morph) || satisfiesAllConstraints (codomain nac) constraints
    invalidSides = matchRestriction conf `isSubclassOf` monic @morph &&
      (not (satisfiesAllConstraints (codomain l) constraints) || not (satisfiesAllConstraints (codomain r) constraints))
