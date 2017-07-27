{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Analysis.ConcurrentRules
( CRDependencies (..),
  allConcurrentRules,
  maxConcurrentRules
) where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Constraint
import           Abstract.Rewriting.DPO
import           Analysis.CriticalSequence             (findTriggeredCriticalSequences,
                                                        getCriticalSequenceComatches)
import           Base.Cardinality
import Util.Monad

data CRDependencies = AllOverlapings | OnlyDependency

-- | Generates the Concurrent Rules for a given list of Productions following the order of the elements in the list.
allConcurrentRules :: (DPO cat morph, EM'PairFactorizable cat morph, Eq (Obj cat)) => CRDependencies
                    -> [Constraint cat morph] -> [Production cat morph] -> cat [Production cat morph]
allConcurrentRules _ _ [] = return []
allConcurrentRules _ _ [r] = return [r]
allConcurrentRules dep constraints (r:rs) = do
  concurrentRulesOfRest <- allConcurrentRules dep constraints rs
  concatMapM (concurrentRules dep constraints r) concurrentRulesOfRest

-- | Generates the Concurrent Rules with the least disjoint JointlyEpimorphisms (JointlyEpimorphisms with the least cardinality) for a given list of Productions
-- (following the order of the elements in the list).
maxConcurrentRules :: (DPO cat morph, EM'PairFactorizable cat morph, Eq (Obj cat), Cardinality (Obj cat))
                  => CRDependencies -> [Constraint cat morph] -> [Production cat morph] -> cat [Production cat morph]
maxConcurrentRules _ _ [] = return []
maxConcurrentRules _ _ [r] = return [r]
maxConcurrentRules dep constraints (r:rs) = do
  maxConcurrentRulesOfRest <- maxConcurrentRules dep constraints rs
  concatMapM (maxConcurrentRuleForLastPairs dep constraints r) maxConcurrentRulesOfRest

concurrentRules :: (DPO cat morph, EM'PairFactorizable cat morph) => CRDependencies -> [Constraint cat morph] -> Production cat morph -> Production cat morph -> cat [Production cat morph]
concurrentRules dep constraints c n = do
  epiPairs <- epiPairsForConcurrentRule dep constraints c n
  mapMaybeM (concurrentRuleForPair constraints c n) epiPairs

maxConcurrentRuleForLastPairs :: (DPO cat morph, EM'PairFactorizable cat morph, Cardinality (Obj cat)) => CRDependencies -> [Constraint cat morph] ->
  Production cat morph -> Production cat morph -> cat [Production cat morph]
maxConcurrentRuleForLastPairs dep constraints c n = do
  epiPairs <- epiPairsForConcurrentRule dep constraints c n
  if null epiPairs
    then return []
    else do
      let maxPair = last epiPairs
          sizeOfMaxPair = sizeOfPair maxPair
          maxPairs = filter (\pair -> sizeOfPair pair == sizeOfMaxPair) epiPairs
      mapMaybeM (concurrentRuleForPair constraints c n) maxPairs
  where sizeOfPair = cardinality . codomain . fst

epiPairsForConcurrentRule :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph)
  => CRDependencies -> [Constraint cat morph] -> Production cat morph -> Production cat morph -> cat [(morph,morph)]
-- it only considers triggered dependencies because is the most intuitive and natural behaviour expected until now.
epiPairsForConcurrentRule OnlyDependency constraints c n = do
  dependencies <- map getCriticalSequenceComatches <$> findTriggeredCriticalSequences c n
  filterM validDependency dependencies
  where validDependency (lp, _) = satisfiesAllConstraints (codomain lp) constraints

epiPairsForConcurrentRule AllOverlapings constraints c n = do
  matchPairs <- findJointlyEpicPairs (matchMorphism @cat, rightObject c) (matchMorphism @cat, leftObject n)
  filterM isValidPair matchPairs
  where isValidPair (lp, rp) =
          satisfiesAllConstraints (codomain lp) constraints 
          `andM` satisfiesGluingConditions (invertProductionWithoutNacs c) lp
          `andM` satisfiesRewritingConditions n rp

concurrentRuleForPair :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph) => [Constraint cat morph] -> Production cat morph -> Production cat morph -> (morph,morph) -> cat (Maybe (Production cat morph))
concurrentRuleForPair constraints c n pair = do
  pocC <- calculatePushoutComplementOfRN (rightMorphism c) (fst pair)
  pocN <- calculatePushoutComplementOfRN (leftMorphism n) (snd pair)

  poC <- calculatePushoutAlongRN (leftMorphism c) (fst pocC)
  poN <- calculatePushoutAlongRN (rightMorphism n) (snd pocN)
  pb <- calculatePullbackAlongR (snd pocC) (snd pocN)

  let l = snd poC <&> fst pb
  let r = snd poN <&> snd pb
  
  dmc <- filterM validNac =<< concatMapM (nacDownwardShift (fst poC)) (nacs c)

  let inverseP = buildProduction (snd pocC) (snd poC) []
  den <- filterM validNac =<< concatMapM (nacDownwardShift (snd pair)) (nacs n)
  lp <- filterM validNac =<< concatMapM (shiftNacOverProduction inverseP) den

  invalidSides <- matchMorphism @cat `isSubclassOf` monic @cat `andM`
      (not <$> satisfiesAllConstraints (codomain l) constraints `andM` satisfiesAllConstraints (codomain r) constraints)
  return $ if invalidSides then Nothing else Just (buildProduction l r (dmc ++ lp))
  where
    -- Filters that are not in the default algorithm, useful when dealing with injective morphisms only
    validNac :: morph -> cat Bool
    validNac nac = (not <$> matchMorphism @cat `isSubclassOf` monic @cat) `orM` satisfiesAllConstraints (codomain nac) constraints
