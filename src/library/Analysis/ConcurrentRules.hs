{-# LANGUAGE FlexibleContexts #-}
module Analysis.ConcurrentRules
( CRDependencies (..),
  allConcurrentRules,
  maxConcurrentRule
) where

import           Abstract.Morphism
import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Analysis.CriticalSequence (triggeredCriticalSequences,getComatch)

data CRDependencies = AllOverlapings | OnlyDependency

-- | Generates the Concurrent Rules for a given list of GraphRules following the order of the elements in the list. If the first argument evaluates to True, it will calculate only rules generated by Injective EpiPairs
allConcurrentRules :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> [Production m] -> [Production m]
allConcurrentRules _ _ [] = []
allConcurrentRules _ _ [x] = [x]
allConcurrentRules dep config (x:xs) = concatMap (crs x) (allCRs xs)
  where
    crs = concurrentRules dep config
    allCRs = allConcurrentRules dep config

-- | Generates the Concurrent Rule with the least disjoint EpiPair for a given list of GraphRules (following the order of the elements in the list). If the first argument evaluates to True, it will generate only the least disjoint injective EpiPair
maxConcurrentRule :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> [Production m] -> Production m
maxConcurrentRule dep config rules = last $ maxConcurrentRules dep config rules

maxConcurrentRules :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> [Production m] -> [Production m]
maxConcurrentRules _ _ [] = []
maxConcurrentRules _ _ [x] = [x]
maxConcurrentRules dep config (x:xs) = map (singleCR x) (maxCRs xs)
  where
    singleCR = maxConcurrentRuleForLastPair dep config
    maxCRs = maxConcurrentRules dep config

concurrentRules :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> Production m -> Production m -> [Production m]
concurrentRules dep config c n =
  let epiPairs = epiPairsForConcurrentRule dep config c n
  in map (concurrentRuleForPair config c n) epiPairs

maxConcurrentRuleForLastPair :: (DPO m, EpiPairs m, Eq (Obj m)) => CRDependencies -> DPOConfig -> Production m -> Production m -> Production m
maxConcurrentRuleForLastPair dep config c n =
  let epiPair = last (epiPairsForConcurrentRule dep config c n)
  in concurrentRuleForPair config c n epiPair

epiPairsForConcurrentRule :: (DPO m, EpiPairs m)
  => CRDependencies -> DPOConfig -> Production m -> Production m -> [(m, m)]
-- it only considers triggered dependencies because is the most intuitive and natural behaviour expected until now.
epiPairsForConcurrentRule OnlyDependency config c n =
  let dependencies = triggeredCriticalSequences config c n
  in map getComatch dependencies

epiPairsForConcurrentRule AllOverlapings config c n =
  let matchInj = matchRestriction config == MonoMatches
      allPairs = createPairs matchInj (codomain (right c)) (codomain (left n))
      isValidPair (lp, rp) = satisfiesGluingConditions config (invertProductionWithoutNacs c) lp && satisfiesRewritingConditions config n rp
  in filter isValidPair allPairs

concurrentRuleForPair :: (DPO m, EpiPairs m, Eq (Obj m)) => DPOConfig -> Production m -> Production m -> (m, m) -> Production m
concurrentRuleForPair config c n pair = constructProduction l r (dmc ++ lp)
  where
    pocC = pushoutComplement (fst pair) (right c)
    pocN = pushoutComplement (snd pair) (left n)
    poC = pushout (fst pocC) (left c)
    poN = pushout (fst pocN) (right n)
    pb = injectivePullback (snd pocC) (snd pocN)
    l = compose (fst pb) (snd poC)
    r = compose (snd pb) (snd poN)
    dmc = concatMap (nacDownwardShift config (fst poC)) (nacs c)
    inverseP = constructProduction (snd pocC) (snd poC) []
    den = concatMap (nacDownwardShift config (snd pair)) (nacs n)
    lp = concatMap (shiftNacOverProduction config inverseP) den
