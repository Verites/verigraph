{-# LANGUAGE TypeFamilies          #-}

module Analysis.ParallelIndependent where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.Morphism
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs
import           TypedGraph.Graph
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

--isParallelIndependentDU :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Int
--isParallelIndependentDU conf p1 p2 = if independent then 1 else 0

isParallelIndependentDU :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Bool
isParallelIndependentDU conf p1 p2 = independent
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    independent = any (\(m1,m2) -> (isDeleteUse conf p1 (m1,m2)) || (isDeleteUse conf p2 (m2,m1))) satisfyingPairs

--isParallelIndependentPB :: DPOConfig -> Production (TypedGraphMorphism a b) -> Production (TypedGraphMorphism a b) -> Int
--isParallelIndependentPB conf p1 p2 = if independent then 1 else 0

isParallelIndependentPB :: DPOConfig -> Production (TypedGraphMorphism a b) -> Production (TypedGraphMorphism a b) -> Bool
isParallelIndependentPB conf p1 p2 = independent
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    independent = any (\(m1,m2) -> (pbTest conf p1 p2 m1 m2)) satisfyingPairs

pbTest :: DPOConfig -> GraphRule a b -> GraphRule a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
pbTest conf p1 p2 m1 m2 = Prelude.null (findIso pb1 pb2)
  where
    (pb1M,_) = pbTGM m1 m2
    pb1 = getDomain pb1M
    
    a1 = compose (getLHS p1) m1
    a2 = compose (getLHS p2) m2
    (pb2M,_) = pbTGM a1 a2
    pb2 = getDomain pb2M

getDomain :: TypedGraphMorphism a b -> TypedGraph a b
getDomain = domain

pbTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
pbTGM = monomorphicPullback

findIso :: TypedGraph a b -> TypedGraph a b -> [TypedGraphMorphism a b]
findIso = findIsomorphisms
