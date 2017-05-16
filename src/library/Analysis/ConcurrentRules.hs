{-# LANGUAGE FlexibleContexts #-}
module Analysis.ConcurrentRules
( CRDependencies (..),
  allConcurrentRules,
  maxConcurrentRules
) where

import           Abstract.AdhesiveHLR
import           Abstract.Cardinality
import qualified Abstract.Cocomplete       as C
import           Abstract.DPO
import           Abstract.Valid
import           Analysis.CriticalSequence (findTriggeredCriticalSequences,
                                            getCriticalSequenceComatches)
import           Data.Maybe                (mapMaybe)

data CRDependencies = AllOverlapings | OnlyDependency

-- | Generates the Concurrent Rules for a given list of Productions following the order of the elements in the list.
allConcurrentRules :: (DPO m, EpiPairs m, Eq (Obj m), Valid m) => CRDependencies -> MorphismsConfig -> [Constraint m] -> [Production m] -> [Production m]
allConcurrentRules _ _ _ [] = []
allConcurrentRules _ _ _ [x] = [x]
allConcurrentRules dep conf constraints (x:xs) = concatMap (crs x) (allCRs xs)
  where
    crs = concurrentRules dep conf constraints
    allCRs = allConcurrentRules dep conf constraints

-- | Generates the Concurrent Rules with the least disjoint EpiPairs (EpiPairs with the least cardinality) for a given list of Productions
-- (following the order of the elements in the list).
maxConcurrentRules :: (DPO m, EpiPairs m, Eq (Obj m), Valid m, Cardinality (Obj m)) => CRDependencies -> MorphismsConfig ->
  [Constraint m] -> [Production m] -> [Production m]
maxConcurrentRules _ _ _ [] = []
maxConcurrentRules _ _ _ [r] = [r]
maxConcurrentRules dep conf constraints (r:rs) = concat $ concatRule r (maxConcurrentRules dep conf constraints rs)
  where
    concatRule rule subMaxRule = case subMaxRule of
      [] -> []
      xs -> map (maxConcurrentRuleForLastPairs dep conf constraints rule) xs

concurrentRules :: (DPO m, EpiPairs m) => CRDependencies -> MorphismsConfig -> [Constraint m] -> Production m -> Production m -> [Production m]
concurrentRules dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
  in mapMaybe (concurrentRuleForPair conf constraints c n) epiPairs

maxConcurrentRuleForLastPairs :: (DPO m, EpiPairs m, Cardinality (Obj m)) => CRDependencies -> MorphismsConfig -> [Constraint m] ->
  Production m -> Production m -> [Production m]
maxConcurrentRuleForLastPairs dep conf constraints c n =
  let epiPairs = epiPairsForConcurrentRule dep conf constraints c n
      maxPair = last (epiPairsForConcurrentRule dep conf constraints c n)
      sizeOfMaxPair = cardinality $ codomain (fst maxPair)
      maxPairs = filter (\(e,_) -> cardinality (codomain e) == sizeOfMaxPair) epiPairs
      maxRule = if null epiPairs
        then []
        else mapMaybe (concurrentRuleForPair conf constraints c n) maxPairs
  in maxRule

epiPairsForConcurrentRule :: (DPO m, EpiPairs m)
  => CRDependencies -> MorphismsConfig -> [Constraint m] -> Production m -> Production m -> [(m, m)]
-- it only considers triggered dependencies because is the most intuitive and natural behaviour expected until now.
epiPairsForConcurrentRule OnlyDependency conf constraints c n =
  let dependencies = map getCriticalSequenceComatches (findTriggeredCriticalSequences conf c n)
      validDependency (lp, _) = satisfiesAllConstraints (codomain lp) constraints
  in filter validDependency dependencies

epiPairsForConcurrentRule AllOverlapings conf constraints c n =
  let matchInj = matchRestriction conf == MonoMatches
      allPairs = createJointlyEpimorphicPairs matchInj (codomain (getRHS c)) (codomain (getLHS n))
      isValidPair (lp, rp) = satisfiesAllConstraints (codomain lp) constraints &&
        satisfiesGluingConditions conf (invertProductionWithoutNacs c) lp && satisfiesRewritingConditions conf n rp
  in filter isValidPair allPairs

concurrentRuleForPair :: (DPO m, EpiPairs m) => MorphismsConfig -> [Constraint m] -> Production m -> Production m -> (m, m) -> Maybe (Production m)
concurrentRuleForPair conf constraints c n pair = if invalidSides then Nothing else Just (buildProduction l r (dmc ++ lp))
  where
    pocC = calculatePushoutComplement (fst pair) (getRHS c)
    pocN = calculatePushoutComplement (snd pair) (getLHS n)
    poC = C.calculatePushout (fst pocC) (getLHS c)
    poN = C.calculatePushout (fst pocN) (getRHS n)
    pb = calculatePullback (snd pocC) (snd pocN)
    l = compose (fst pb) (snd poC)
    r = compose (snd pb) (snd poN)
    dmc = filter validNac $ concatMap (nacDownwardShift conf (fst poC)) (getNACs c)
    inverseP = buildProduction (snd pocC) (snd poC) []
    den = filter validNac $ concatMap (nacDownwardShift conf (snd pair)) (getNACs n)
    lp = filter validNac $ concatMap (shiftNacOverProduction conf inverseP) den
    -- Filters that are not in the default algorithm, useful when dealing with injective morphisms only
    validNac nac = matchRestriction conf /= MonoMatches || satisfiesAllConstraints (codomain nac) constraints
    invalidSides = matchRestriction conf == MonoMatches &&
      (not (satisfiesAllConstraints (codomain l) constraints) || not (satisfiesAllConstraints (codomain r) constraints))
