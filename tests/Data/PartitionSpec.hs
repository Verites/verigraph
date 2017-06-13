module Data.PartitionSpec where


import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Math.Combinat.Numbers (bellNumber)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Partition


modifyNumTestCases :: Int -> SpecWith a -> SpecWith a
modifyNumTestCases x = modifyMaxSize (const x) . modifyMaxSuccess (const $ x + 1)

spec :: Spec
spec = do

  describe "allPartitionsOf" $ modifyNumTestCases 15 $ do
    prop "produces the correct number of partitions" $ \(NonNegative numElements) ->
      integerLength (allPartitionsOf [0 .. numElements - 1 :: Int]) == bellNumber numElements

    prop "produces valid partitions" $ \(NonNegative numElements) ->
      let elements = [0 .. numElements - 1 :: Int]
      in (`all` allPartitionsOf elements) $ \partition ->
        pairwiseDisjoint (Set.toList partition)
        && Set.unions (Set.toList partition) == Set.fromList elements

    prop "produces no duplicates" $ \(NonNegative numElements) ->
      let partitions = allPartitionsOf [0 .. numElements - 1 :: Int]
      in length partitions == Set.size (Set.fromList partitions)

  describe "addToPartition" $ modifyNumTestCases 10 $ do
    prop "produces the correct number of partitions" $ \(NonNegative numElements) newElement ->
      (newElement < 0 || numElements <= newElement) ==>
      forAll (partitionsOfSize numElements) $ \partition ->
        length (addToPartition newElement partition) == Set.size partition + 1

    prop "produces valid partitions" $ \(NonNegative numElements) newElement ->
      (newElement < 0 || numElements <= newElement) ==>
      forAll (partitionsOfSize numElements) $ \originalPartition ->
        (`all` addToPartition newElement originalPartition) $ \partition ->
          pairwiseDisjoint (Set.toList partition)
          && Set.unions (Set.toList partition) == Set.unions (Set.singleton newElement : Set.toList originalPartition)

    prop "produces no duplicates" $ \(NonNegative numElements) newElement ->
      (newElement < 0 || numElements <= newElement) ==>
      forAll (partitionsOfSize numElements) $ \originalPartition ->
        let partitions = addToPartition newElement originalPartition
        in length partitions == Set.size (Set.fromList partitions)

  describe "allRefinementsOf" $ modifyMaxSize (const 10) $ do
    prop "produces valid partitions" $ \(NonNegative numElements) ->
      forAll (partitionsOfSize numElements) $ \originalPartition ->
        (`all` allRefinementsOf originalPartition) $ \refinement ->
        pairwiseDisjoint (Set.toList refinement)
        && Set.unions (Set.toList refinement) == Set.unions (Set.toList originalPartition)

    prop "produces no duplicates" $ \(NonNegative numElements) ->
      forAll (partitionsOfSize numElements) $ \originalPartition ->
      let refinements = allRefinementsOf originalPartition
      in length refinements == Set.size (Set.fromList refinements)

    prop "produces actual refinements" $ \(NonNegative numElements) ->
      forAll (partitionsOfSize numElements) $ \originalPartition ->
      (`all` allRefinementsOf originalPartition) $ \refinement ->
        refinement `refines` originalPartition

    prop "produces the correct number of refinements" $ \(NonNegative numElements) ->
      let partitions = allPartitionsOf [0 .. numElements - 1 :: Int]
      in forAll (elements partitions) $ \originalPartition ->
        let naiveRefinements = filter (`refines` originalPartition) partitions
        in length (allRefinementsOf originalPartition) == length naiveRefinements


refines :: Ord a => Partition a -> Partition a -> Bool
refines refinement partition = all (containedInSomeBlockOf partition) (Set.toList refinement)
  where containedInSomeBlockOf partition refinedBlock = any (refinedBlock `Set.isSubsetOf`) (Set.toList partition)

partitionsOfSize :: Int -> Gen (Partition Int)
partitionsOfSize n = elements (allPartitionsOf [0 .. n - 1])

pairwiseDisjoint :: Ord a => [Set a] -> Bool
pairwiseDisjoint [] = True
pairwiseDisjoint [_] = True
pairwiseDisjoint (s:ss) = all (disjoint s) ss && pairwiseDisjoint ss
  where disjoint s1 s2 = Set.null (Set.intersection s1 s2)

integerLength :: [a] -> Integer
integerLength = fromIntegral . length
