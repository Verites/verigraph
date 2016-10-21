module Analysis.ParallelIndependent where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.Morphism
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs
import           TypedGraph.Graph
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

data Algorithm = DeleteUse | Pullback
data IndependenceType = Parallel | Sequentially deriving (Eq, Show)

--isIndependent :: (EpiPairs m, DPO m) => Algorithm -> DPOConfig -> Production m -> Production m -> Bool
isIndependent :: IndependenceType -> Algorithm -> DPOConfig -> GraphRule a b -> GraphRule a b -> Bool
isIndependent ind algorithm conf p1' p2 = not (conflict algorithm)
  where
    p1 = case ind of 
           Parallel -> p1'
           Sequentially -> invertProductionWithoutNacs p1'
    
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    
    conflict DeleteUse = any (\(m1,m2) -> (isDeleteUse conf p1 (m1,m2)) || (isDeleteUse conf p2 (m2,m1))) satisfyingPairs
    conflict Pullback = any (\(m1,m2) -> (pbTest p1 p2 m1 m2)) satisfyingPairs

-- pullback conflict test, when this function comes generic, move it to DiagramAlgorithms
pbTest :: GraphRule a b -> GraphRule a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
pbTest p1 p2 m1 m2 = Prelude.null (findIso pb1 pb2)
  where
    (pb1M,_) = calculatePullback m1 m2
    pb1 = domain pb1M
    
    a1 = compose (getLHS p1) m1
    a2 = compose (getLHS p2) m2
    (pb2M,_) = calculatePullback a1 a2
    pb2 = domain pb2M

findIso :: TypedGraph a b -> TypedGraph a b -> [TypedGraphMorphism a b]
findIso = findIsomorphisms
