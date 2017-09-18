{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Analysis.ParallelIndependent where

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms

-- | Algorithm used to determine independence between two rules
-- Cond1 -> 3 pullbacks and two iso tests
-- Cond2 -> 2 pullbacks and one iso test
-- Cond3 -> classical delete-use
data Algorithm = Cond1 | Cond2 | Cond3
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

-- | Checks if two transformations are independent (just delete-use),
-- works with delete-use or pullback checking.
isIndependent :: (E'PairCofinitary morph, DPO morph, Complete morph) =>
  IndependenceType -> Algorithm -> MorphismsConfig morph -> Production morph -> Production morph -> Bool
isIndependent ind algorithm conf p1' p2 = not $ conflict algorithm
  where
    p1 = case ind of
           Parallel     -> p1'
           Sequentially -> invertProductionWithoutNacs p1'

    satisfyingPairs = findJointSurjectiveApplicableMatches conf p1 p2

    conflict Cond1 = any (cond1 p1 p2) satisfyingPairs
    conflict Cond2 = any (cond2 p1 p2) satisfyingPairs
    conflict Cond3 = any (\(m1,m2) -> isDeleteUse conf p1 (m1,m2) || isDeleteUse conf p2 (m2,m1)) satisfyingPairs

-- | Checks independence between transformations via 2 pullbacks
cond2 :: forall morph. (MAdhesive morph, FindMorphism morph, Complete morph) => Production morph -> Production morph -> (morph,morph) -> Bool
cond2 p1 p2 (m1,m2) = Prelude.null (findCospanCommuters (iso @morph) k1k2ToG l1l2ToG)
  where
    (_,pb1) = calculatePullback m1 m2

    a1 = m1 <&> leftMorphism p1
    a2 = m2 <&> leftMorphism p2
    (_,pb2) = calculatePullback a1 a2

    k1k2ToG = a1 <&> pb2
    l1l2ToG = m1 <&> pb1

-- | Checks independence between transformations via 3 pullbacks
cond1 :: (MAdhesive morph, FindMorphism morph, Complete morph) => Production morph -> Production morph -> (morph,morph) -> Bool
cond1 p1 p2 (m1,m2) = not (isIsomorphism a && isIsomorphism b)
  where
    (pb2,pb1) = calculatePullback m1 m2

    (a,_) = calculatePullbackAlongM (leftMorphism p1) pb1
    (b,_) = calculatePullbackAlongM (leftMorphism p2) pb2
