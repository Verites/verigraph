module TypedGraph.Morphism.EpiPairs where

import           Category.AdhesiveHLR
import           Object.Graph                                     as G
import qualified Graph.GraphMorphism                             as GM
import           TypedGraph.Morphism.Core
import           TypedGraph.Partitions.GraphPartition            (generateGraphPartitions)
import           TypedGraph.Partitions.GraphPartitionToVerigraph (mountTypedGraphMorphisms)
import           TypedGraph.Partitions.VerigraphToGraphPartition (createDisjointUnion,
                                                                  createSatisfyingNacsDisjointUnion)

instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createJointlyEpimorphicPairs inj m1 m2 = map (mountTypedGraphMorphisms m1 m2) (generateGraphPartitions (createDisjointUnion (m1,inj) (m2,inj)))

  createAllSubobjects inj m1 = map fst part
    where
      m2 = GM.buildGraphMorphism G.empty G.empty [] []
      part = map (mountTypedGraphMorphisms m1 m2) (generateGraphPartitions (createDisjointUnion (m1,inj) (m2,inj)))

  createJointlyEpimorphicPairsFromNAC conf r nac =
    map (mountTypedGraphMorphisms r (codomain nac)) (generateGraphPartitions labeled)
    where
      injectiveMatch = matchRestriction conf == MonoMatches
      totalInjectiveNac = nacSatisfaction conf == MonomorphicNAC

      labeled = createSatisfyingNacsDisjointUnion (r, injectiveMatch) (nac, totalInjectiveNac)

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes,
  -- considering they have same domain
  -- and flags indicating the injective of each morphism
  calculateCommutativeSquaresAlongMonomorphism (m1,inj1) (m2,inj2) = commutativePairs
    where
      codomain1 = codomain m1
      codomain2 = codomain m2
      allPairs = map (mountTypedGraphMorphisms codomain1 codomain2)
                     (generateGraphPartitions (createDisjointUnion (codomain1,inj1) (codomain2,inj2)))
      commutativePairs = filter (\(x,y) -> x <&> m1 == y <&> m2) allPairs
