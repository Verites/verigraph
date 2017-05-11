{-
   This module was modified in order to run specific tests,
   instead the parallel independence, it is running the
   critical pairs analysis without NACs.
-}

module Analysis.ParallelIndependent where

import           Control.DeepSeq

import           Abstract.Category.AdhesiveHLR
import           Abstract.Category.FinitaryCategory
import           Abstract.Category.JointlyEpimorphisms
import           Abstract.Rewriting.DPO

-- | Algorithm used to determine independence between two rules
data Algorithm        = AbovePullback | AboveMorphism | AbovePullbackIso | BelowPullback | BelowMorphism
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

findEpiPairs :: (DPO morph, JointlyEpimorphisms morph) => MorphismsConfig -> Production morph -> Production morph -> [(morph, morph)]
findEpiPairs conf p1 p2 = satisfyingPairs
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs

-- | Checks if two transformations are independent (just delete-use),
-- works with delete-use or pullback checking.
isIndependent :: (JointlyEpimorphisms morph, DPO morph, NFData morph) =>
  IndependenceType -> Algorithm -> MorphismsConfig -> Production morph -> Production morph -> Int
isIndependent ind algorithm conf p1' p2 = conflict algorithm
  where
    p1 = case ind of
           Parallel     -> p1'
           Sequentially -> invertProductionWithoutNacs p1'

    epiPairs = findEpiPairs conf p1 p2

    conflict AbovePullback    = length $ filter (abovePullback p1 p2) epiPairs
    conflict BelowPullback    = length $ filter (belowPullback p1 p2) epiPairs

    conflict AboveMorphism    = length $ filter (aboveMorphism conf  p1 p2) epiPairs
    conflict BelowMorphism    = length $ filter (belowMorphism conf p1 p2) epiPairs

    conflict AbovePullbackIso = length $ filter (abovePullbackAndIso p1 p2) epiPairs

-- | Checks independence between transformations via 3 pullbacks and 2 iso checks
abovePullback :: (AdhesiveHLR morph, FindMorphism morph, NFData morph) =>
  Production morph -> Production morph -> (morph,morph) -> Bool
abovePullback p1 p2 (m1,m2) = not $!! leftCondition && rightCondition
  where
    (pb2,pb1) = force $!! (calculatePullback $!! m1) $!! m2

    (a,_) = force $!! (calculatePullback $!! (getLHS $!! p1)) $!! pb1
    (b,_) = force $!! (calculatePullback $!! (getLHS $!! p2)) $!! pb2

    leftCondition = force (isIsomorphism $!! a)
    rightCondition = force (isIsomorphism $!! b)

-- | Checks independence between transformations via 2 pullbacks and 2 findCospanCommuter
aboveMorphism :: (AdhesiveHLR morph, FindMorphism morph, NFData morph) =>
  MorphismsConfig -> Production morph -> Production morph -> (morph,morph) -> Bool
aboveMorphism dpoConf p1 p2 (m1,m2) = force $!! leftCondition || rightCondition
  where
    conf  = force $!! matchRestrictionToMorphismType $!! (matchRestriction $!! dpoConf)

    (m1',m2') = force $!! (calculatePullback $!! m1) $!! m2

    leftMorphisms  = force $!! ((findCospanCommuter $!! conf) $!! m2') $!! (getLHS $!! p1)
    rightMorphisms = force $!! ((findCospanCommuter $!! conf) $!! m1') $!! (getLHS $!! p2)

    leftCondition = force (Prelude.null $!! leftMorphisms)
    rightCondition = force (Prelude.null $!! rightMorphisms)

-- | Checks independence between transformations via 2 pushout complements, 2 pullbacks and 2 iso checking
belowPullback :: (AdhesiveHLR morph, FindMorphism morph, NFData morph) =>
  Production morph -> Production morph -> (morph,morph) -> Bool
belowPullback p1 p2 (m1,m2) = not $!! leftCondition && rightCondition
  where
    (_,d1) = force $!! (calculatePushoutComplement $!! m1) $!! (getLHS $!! p1)
    (_,d2) = force $!! (calculatePushoutComplement $!! m2) $!! (getLHS $!! p2)

    (a,_) = force $!! (calculatePullback $!! d1) $!! m2
    (b,_) = force $!! (calculatePullback $!! d2) $!! m1

    leftCondition  = force (isIsomorphism $!! a)
    rightCondition = force (isIsomorphism $!! b)

-- | Checks independence between transformations via 2 pushout complements and two 2 findCospanCommuter
belowMorphism :: (AdhesiveHLR morph, FindMorphism morph, DPO morph, NFData morph) =>
  MorphismsConfig -> Production morph -> Production morph -> (morph,morph) -> Bool
belowMorphism dpoConf p1 p2 (m1,m2) = force $ leftCondition || rightCondition
  where
    conf  = matchRestrictionToMorphismType $!! (matchRestriction $!! dpoConf)

    (_,leftRewrite)  = force $!! (calculatePushoutComplement $!! m1) $!! (getLHS $!! p1)
    (_,rightRewrite) = force $!! (calculatePushoutComplement $!! m2) $!! (getLHS $!! p2)

    leftMorphisms  = force $!! ((findCospanCommuter $!! conf) $!! m1) $!! rightRewrite
    rightMorphisms = force $!! ((findCospanCommuter $!! conf) $!! m2) $!! leftRewrite

    leftCondition  = force (Prelude.null $!! leftMorphisms)
    rightCondition = force (Prelude.null $!! rightMorphisms)

-- | Checks independence between transformations via 2 pullbacks and a Iso find.
abovePullbackAndIso :: (AdhesiveHLR morph, FindMorphism morph, NFData morph) =>
  Production morph -> Production morph -> (morph,morph) -> Bool
abovePullbackAndIso p1 p2 (m1,m2) = Prelude.null $!! independenceCondition
  where
    (_,pb1) = force $!! (calculatePullback $!! m1) $!!m2

    a1 = force $!! force m1 <&> force (getLHS $!! p1)
    a2 = force $!! force m2 <&> force (getLHS $!! p2)

    (_,pb2) = force $!! (calculatePullback $!! a1) $!! a2

    k1k2ToG =  force $!! force a1 <&> force pb2
    l1l2ToG =  force $!! force m1 <&> force pb1

    independenceCondition = force $!! ((findCospanCommuter $!! Isomorphism) $!! k1k2ToG) $!! l1l2ToG
