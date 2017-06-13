module Category.TypedGraph.JointlyEpimorphisms where

import           Abstract.Category.AdhesiveHLR
import           Abstract.Category.JointlyEpimorphisms
import           Category.TypedGraph                     ()
import           Data.Graphs                             as G
import qualified Data.Graphs.Morphism                    as GM
import           Data.TypedGraph.Morphism
import           Data.TypedGraph.Partition               (generateGraphPartitions)
import           Data.TypedGraph.Partition.FromVerigraph (createDisjointUnion,
                                                          createSatisfyingNacsDisjointUnion)
import           Data.TypedGraph.Partition.ToVerigraph   (mountTypedGraphMorphisms)

instance JointlyEpimorphisms (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createJointlyEpimorphicPairs inj m1 m2 =
    map
      (mountTypedGraphMorphisms m1 m2)
      (generateGraphPartitions (createDisjointUnion (m1,inj) (m2,inj)))

  createAllQuotients m1 = map fst part
    where
      m2 = GM.buildGraphMorphism G.empty G.empty [] []
      part = map
               (mountTypedGraphMorphisms m1 m2)
               (generateGraphPartitions (createDisjointUnion (m1,False) (m2,False)))

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
