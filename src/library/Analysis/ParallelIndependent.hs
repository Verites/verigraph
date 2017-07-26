{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Analysis.ParallelIndependent where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import Util.Monad

-- | Algorithm used to determine independence between two rules
-- Cond1 -> 3 pullbacks and two iso tests
-- Cond2 -> 2 pullbacks and one iso test
-- Cond3 -> classical delete-use
data Algorithm = Cond1 | Cond2 | Cond3
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

-- | Checks if two transformations are independent (just delete-use),
-- works with delete-use or pullback checking.
isIndependent :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph, Complete cat morph) =>
  IndependenceType -> Algorithm -> Production cat morph -> Production cat morph -> cat Bool
isIndependent ind algorithm p1' p2 = do
  matchCandidates <- findJointlyEpicPairs (matchMorphism @cat, leftObject p1) (matchMorphism @cat, leftObject p2)
  matchPairs <- filterM canRewrite matchCandidates
  not <$> conflict algorithm matchPairs
  where
    p1 = case ind of
           Parallel     -> p1'
           Sequentially -> invertProductionWithoutNacs p1'

    canRewrite (m1, m2) = satisfiesRewritingConditions p1 m1 `andM` satisfiesRewritingConditions p2 m2

    conflict Cond1 = anyM (cond1 p1 p2)
    conflict Cond2 = anyM (cond2 p1 p2)
    conflict Cond3 = anyM (\(m1,m2) -> isDeleteUse p1 (m1,m2) `orM` isDeleteUse p2 (m2,m1))

-- | Checks independence between transformations via 2 pullbacks
cond2 :: forall cat morph. (LRNAdhesive cat morph, Complete cat morph, FindMorphism cat morph) => Production cat morph -> Production cat morph -> (morph,morph) -> cat Bool
cond2 p1 p2 (m1, m2) = do
    (_, pb1) <- calculatePullback m1 m2

    let a1 = m1 <&> leftMorphism p1
    let a2 = m2 <&> leftMorphism p2
    (_, pb2) <- calculatePullback a1 a2

    let k1k2ToG = a1 <&> pb2
    let l1l2ToG = m1 <&> pb1
    null <$> findCospanCommuters (iso @cat) k1k2ToG l1l2ToG

-- | Checks independence between transformations via 3 pullbacks
cond1 :: (LRNAdhesive cat morph, FindMorphism cat morph, Complete cat morph) => Production cat morph -> Production cat morph -> (morph,morph) -> cat Bool
cond1 p1 p2 (m1,m2) = do
  (pb2, pb1) <- calculatePullback m1 m2
  (a, _) <- calculatePullback (leftMorphism p1) pb1
  (b, _) <- calculatePullback (leftMorphism p2) pb2
  not <$> isIsomorphism a `andM` isIsomorphism b

