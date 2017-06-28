module Data.PartitionSpec (spec) where


import           Control.Monad
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Math.Combinat.Numbers (bellNumber)
import           Test.Hspec

import           Data.Partition



spec :: Spec
spec = do

  describe "allPartitionsOf" $ do
    it "produces the correct number of partitions" $ forM_ [0..8] $ \numElements ->
      integerLength (allPartitionsOf [0 .. numElements - 1 :: Int]) `shouldBe` bellNumber numElements

    it "produces valid partitions" $ forM_ [0..8] $ \numElements -> do
      let elements = [0 .. numElements - 1 :: Int]
      forM (allPartitionsOf elements) $ \partition -> do
        Set.toList partition `shouldSatisfy` pairwiseDisjoint
        Set.unions (Set.toList partition) `shouldBe` Set.fromList elements

    it "produces no duplicates" $ forM_ [0..8] $ \numElements -> do
      let partitions = allPartitionsOf [0 .. numElements - 1 :: Int]
      length partitions `shouldBe` Set.size (Set.fromList partitions)

  describe "addToPartition" $  do
    it "produces the correct number of partitions" $
      forM_ [0..8] $ \numElements ->
      forM [numElements, -1, minBound, maxBound] $ \newElement ->
      forM (allPartitionsOf [0 .. numElements - 1 :: Int]) $ \originalPartition ->
        length (addToPartition newElement originalPartition) `shouldBe` Set.size originalPartition + 1

    it "produces valid partitions" $
      forM_ [0..8] $ \numElements ->
      forM [numElements, -1, minBound, maxBound] $ \newElement ->
      forM (allPartitionsOf [0 .. numElements - 1 :: Int]) $ \originalPartition ->
        forM (addToPartition newElement originalPartition) $ \partition -> do
          Set.toList partition `shouldSatisfy` pairwiseDisjoint
          Set.unions (Set.toList partition) `shouldBe` Set.unions (Set.singleton newElement : Set.toList originalPartition)

    it "produces no duplicates" $
      forM_ [0..8] $ \numElements ->
      forM [numElements, -1, minBound, maxBound] $ \newElement ->
      forM (allPartitionsOf [0 .. numElements - 1 :: Int]) $ \originalPartition -> do
        let partitions = addToPartition newElement originalPartition
        length partitions `shouldBe` Set.size (Set.fromList partitions)

  describe "allRefinementsOf" $ do
    it "produces valid partitions that are actual refinements" $
      forM_ [0..7] $ \numElements ->
      forM (allPartitionsOf [0 .. numElements - 1 :: Int]) $ \originalPartition ->
      forM (allRefinementsOf originalPartition) $ \refinement -> do
        Set.toList refinement `shouldSatisfy` pairwiseDisjoint
        Set.unions (Set.toList refinement) `shouldBe` Set.unions (Set.toList originalPartition)
        (refinement, originalPartition) `shouldSatisfy` uncurry refines

    it "produces no duplicates" $
      forM_ [0..7] $ \numElements ->
      forM (allPartitionsOf [0 .. numElements - 1 :: Int]) $ \originalPartition -> do
        let refinements = allRefinementsOf originalPartition
        length refinements `shouldBe` Set.size (Set.fromList refinements)

    it "produces the correct number of refinements" $ forM_ [0..7] $ \numElements -> do
      let partitions = allPartitionsOf [0 .. numElements - 1 :: Int]
      forM partitions $ \originalPartition -> do
        let naiveRefinements = filter (`refines` originalPartition) partitions
        length (allRefinementsOf originalPartition) `shouldBe` length naiveRefinements


refines :: Ord a => Partition a -> Partition a -> Bool
refines refinement partition = all (containedInSomeBlockOf partition) (Set.toList refinement)
  where containedInSomeBlockOf partition refinedBlock = any (refinedBlock `Set.isSubsetOf`) (Set.toList partition)

pairwiseDisjoint :: Ord a => [Set a] -> Bool
pairwiseDisjoint [] = True
pairwiseDisjoint [_] = True
pairwiseDisjoint (s:ss) = all (disjoint s) ss && pairwiseDisjoint ss
  where disjoint s1 s2 = Set.null (Set.intersection s1 s2)

integerLength :: [a] -> Integer
integerLength = fromIntegral . length
