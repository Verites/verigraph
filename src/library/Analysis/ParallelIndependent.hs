module Analysis.ParallelIndependent where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Abstract.Morphism
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs

-- | Algorithm used to determine independence between two rules
-- Cond1 -> 3 pullbacks and two iso tests
-- Cond2 -> 2 pullbacks and one iso test
-- Cond3 -> classical delete-use
data Algorithm = Cond1 | Cond2 | Cond3
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

-- | Checks if two transformations are independent (just delete-use),
-- works with delete-use or pullback checking.
isIndependent :: (EpiPairs m, DPO m) =>
  IndependenceType -> Algorithm -> MorphismsConfig -> Production m -> Production m -> Bool
isIndependent ind algorithm conf p1' p2 = not $ conflict algorithm
  where
    p1 = case ind of
           Parallel     -> p1'
           Sequentially -> invertProductionWithoutNacs p1'

    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs

    conflict Cond1 = any (cond1 p1 p2) satisfyingPairs
    conflict Cond2 = any (cond2 p1 p2) satisfyingPairs
    conflict Cond3 = any (\(m1,m2) -> isDeleteUse conf p1 (m1,m2) || isDeleteUse conf p2 (m2,m1)) satisfyingPairs

-- | Checks independence between transformations via 2 pullbacks
cond2 :: (AdhesiveHLR m, FindMorphism m) => Production m -> Production m -> (m, m) -> Bool
cond2 p1 p2 (m1,m2) = Prelude.null (findCospanCommuter Isomorphism k1k2ToG l1l2ToG)
  where
    (_,pb1) = calculatePullback m1 m2

    a1 = compose (getLHS p1) m1
    a2 = compose (getLHS p2) m2
    (_,pb2) = calculatePullback a1 a2

    k1k2ToG = compose pb2 a1
    l1l2ToG = compose pb1 m1

-- | Checks independence between transformations via 3 pullbacks
cond1 :: (AdhesiveHLR m, FindMorphism m) => Production m -> Production m -> (m, m) -> Bool
cond1 p1 p2 (m1,m2) = not (isIsomorphism a && isIsomorphism b)
  where
    (pb2,pb1) = calculatePullback m1 m2

    (a,_) = calculatePullback (getLHS p1) pb1
    (b,_) = calculatePullback (getLHS p2) pb2
