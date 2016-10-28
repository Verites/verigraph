module Analysis.ParallelIndependent where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Abstract.Morphism
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs

data Algorithm = DeleteUse | Pullback
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

isIndependent :: (EpiPairs m, DPO m) =>
  IndependenceType -> Algorithm -> DPOConfig -> Production m -> Production m -> Bool
isIndependent ind algorithm conf p1' p2 = not $ conflict algorithm
  where
    p1 = case ind of
           Parallel     -> p1'
           Sequentially -> invertProductionWithoutNacs p1'

    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs

    conflict DeleteUse = any (\(m1,m2) -> (isDeleteUse conf p1 (m1,m2)) || (isDeleteUse conf p2 (m2,m1))) satisfyingPairs
    conflict Pullback = any (\(m1,m2) -> (pbTest p1 p2 m1 m2)) satisfyingPairs

pbTest :: (AdhesiveHLR m, FindMorphism m) => Production m -> Production m -> m -> m -> Bool
pbTest p1 p2 m1 m2 = Prelude.null (findIsoFromDomains pb1 pb2)
  where
    (pb1,_) = calculatePullback m1 m2

    a1 = compose (getLHS p1) m1
    a2 = compose (getLHS p2) m2
    (pb2,_) = calculatePullback a1 a2

findIsoFromDomains :: FindMorphism m => m -> m -> [m]
findIsoFromDomains a b = findMorphisms Isomorphism (domain a) (domain b)
