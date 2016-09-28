{-# LANGUAGE FlexibleContexts #-}
module Analysis.ConcurrentRules
( CRDependencies (..),
  allConcurrentRules,
  maxConcurrentRule
) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Valid
import           Analysis.CriticalSequence (findTriggeringCriticalSequences,
                                            getCriticalSequenceComatches)

data CRDependencies = AllOverlapings | OnlyDependency

-- | Generates the Concurrent Rules for a given list of GraphRules following the order of the elements in the list.
allConcurrentRules :: (DPO m, EpiPairs m, Eq (Obj m), Valid m) => CRDependencies -> DPOConfig -> [Constraint m] -> [Production m] -> [Production m]
allConcurrentRules _ _ _ [] = []
allConcurrentRules _ _ _ [x] = [x]
allConcurrentRules dep conf constraints (x:xs) = concatMap (crs x) (allCRs xs)
  where
    crs = concurrentRules dep conf constraints
    allCRs = allConcurrentRules dep conf constraints

-- | Generates the Concurrent Rule with the least disjoint EpiPair for a given list of GraphRules
-- (following the order of the elements in the list).
maxConcurrentRule :: (DPO m, EpiPairs m, Eq (Obj m), Valid m) => CRDependencies -> DPOConfig ->
  [Constraint m] -> [Production m] -> Maybe (Production m)
maxConcurrentRule _ _ _ [] = Nothing
maxConcurrentRule _ _ _ [r] = Just r
maxConcurrentRule dep conf constraints (r:rs) = concatRule r (maxConcurrentRule dep conf constraints rs)
  where
    concatRule rule subMaxRule = case subMaxRule of
      Nothing -> Nothing
      Just x -> maxConcurrentRuleForLastPair dep conf constraints rule x

--auxConcurrentRule :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig ->
--  [Constraint m] -> Production m -> Maybe (Production m) -> Maybe (Production m)
--auxConcurrentRule dep conf constraints rule subMaxRule = case subMaxRule of
--  Nothing -> Nothing
--  Just x -> maxConcurrentRuleForLastPair dep conf constraints rule x

concurrentRules :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> [Constraint m] -> Production m -> Production m -> [Production m]
concurrentRules dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
  in map (concurrentRuleForPair conf constraints c n) epiPairs

maxConcurrentRuleForLastPair :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> [Constraint m] ->
  Production m -> Production m -> Maybe (Production m)
maxConcurrentRuleForLastPair dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
      maxPair = last (epiPairsForConcurrentRule dep conf constraints c n)
      maxRule = if null epiPairs
        then  Nothing
        else Just (concurrentRuleForPair conf constraints c n maxPair)
  in maxRule

epiPairsForConcurrentRule :: (DPO m, EpiPairs m)
  => CRDependencies -> DPOConfig -> [Constraint m] -> Production m -> Production m -> [(m, m)]
-- it only considers triggered dependencies because is the most intuitive and natural behaviour expected until now.
epiPairsForConcurrentRule OnlyDependency conf constraints c n =
  let dependencies = map getCriticalSequenceComatches (findTriggeringCriticalSequences conf c n)
      validDependency (lp, _) = satisfiesAllAtomicConstraints (codomain lp) constraints
  in filter validDependency dependencies

epiPairsForConcurrentRule AllOverlapings conf constraints c n =
  let matchInj = matchRestriction conf == MonoMatches
      allPairs = createJointlyEpimorphicPairs matchInj (codomain (getRHS c)) (codomain (getLHS n))
      isValidPair (lp, rp) = satisfiesAllAtomicConstraints (codomain lp) constraints &&
        satisfiesGluingConditions conf (invertProductionWithoutNacs c) lp && satisfiesRewritingConditions conf n rp
  in filter isValidPair allPairs

concurrentRuleForPair :: (DPO m, EpiPairs m, Eq(Obj m)) => DPOConfig -> [Constraint m] -> Production m -> Production m -> (m, m) -> Production m
concurrentRuleForPair conf constraints c n pair = buildProduction l r (dmc ++ lp)
  where
    pocC = calculatePushoutComplement (fst pair) (getRHS c)
    pocN = calculatePushoutComplement (snd pair) (getLHS n)
    poC = calculatePushout (fst pocC) (getLHS c)
    poN = calculatePushout (fst pocN) (getRHS n)
    pb = monomorphicPullback (snd pocC) (snd pocN)
    l = compose (fst pb) (snd poC)
    r = compose (snd pb) (snd poN)
    dmc = filter validNac $ concatMap (nacDownwardShift conf (fst poC)) (getNACs c)
    inverseP = buildProduction (snd pocC) (snd poC) []
    den = filter validNac $ concatMap (nacDownwardShift conf (snd pair)) (getNACs n)
    lp = filter validNac $ concatMap (shiftNacOverProduction conf inverseP) den
    validNac nac = matchRestriction conf /= MonoMatches || satisfiesAllAtomicConstraints (codomain nac) constraints
