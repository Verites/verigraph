module Analysis.ParallelIndependent where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Abstract.Morphism
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs

data Algorithm = DeleteUse | Pullback | Cond1
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

    conflict Cond1 = any (uncurry (cond1 p1 p2)) satisfyingPairs
    conflict DeleteUse = any (\(m1,m2) -> isDeleteUse conf p1 (m1,m2) || isDeleteUse conf p2 (m2,m1)) satisfyingPairs
    conflict Pullback = any (uncurry (pbTest p1 p2)) satisfyingPairs

-- | Checks independence between transformations via pullback tests
pbTest :: (AdhesiveHLR m, FindMorphism m) => Production m -> Production m -> m -> m -> Bool
pbTest p1 p2 m1 m2 = Prelude.null (findIsoFromDomains (compose pb2 a1) (compose pb1 m1))
  where
    (_,pb1) = calculatePullback m1 m2

    a1 = compose (getLHS p1) m1
    a2 = compose (getLHS p2) m2
    (_,pb2) = calculatePullback a1 a2

cond1 :: (AdhesiveHLR m, FindMorphism m) => Production m -> Production m -> m -> m -> Bool
cond1 p1 p2 m1 m2 = not (isIsomorphism a && isIsomorphism b)
  where
    (pb2,pb1) = calculatePullback m1 m2
    
    (a,_) = calculatePullback (getLHS p1) pb1
    (b,_) = calculatePullback (getLHS p2) pb2
    
    

findIsoFromDomains :: FindMorphism m => m -> m -> [m]
--findIsoFromDomains a b = findIsomorphisms (domain a) (domain b)
findIsoFromDomains a b = findCospanCommuter Isomorphism a b
