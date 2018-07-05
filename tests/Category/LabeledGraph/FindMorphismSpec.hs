{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Category.LabeledGraph.FindMorphismSpec where

import           Control.Monad
import           Data.Foldable
import           Math.Combinat.Numbers          (stirling2nd)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck                as QuickCheck

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Base.Valid
import           Category.LabeledGraph
import           Data.LabeledGraph              as Graph
import           Data.LabeledGraph.Morphism     as Morphism
import           Data.LabeledGraph.QuickCheck
import           Data.Variable
import           Util.Test


maxGraphSize :: Int
maxGraphSize = 5

withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs = modifyMaxSize (`div` 4)



spec :: Spec
spec = withSmallerGraphs $ do

  describe "findMorphisms" $ do

    prop "always produces valid morphisms" $ \domain codomain ->
      forAllMorphismsBetween anyMorphism domain codomain $ \m ->
        isValid (m :: LabeledMorphism)


    context "from any empty graph" $ do
      it "always produces a single morphism" $
        property $ \codomain ->
          length (findMorphisms @LabeledMorphism anyMorphism Graph.empty codomain) == 1

      it "always produces a single monomorphism" $
        property $ \codomain ->
          length (findMorphisms @LabeledMorphism monic Graph.empty codomain) == 1

      it "always produces no epimorphism (unless the codomain is empty)" $
        property $ \codomain ->
          length (findMorphisms @LabeledMorphism epic Graph.empty codomain)
            == if Graph.null codomain then 1 else 0

      it "always produces no isomorphism (unless the codomain is empty)" $
        property $ \codomain ->
          length (findMorphisms @LabeledMorphism iso Graph.empty codomain)
            == if Graph.null codomain then 1 else 0


    context "between graphs with no edges or labels" $ do
      let makeGraph v = fromNodesAndEdges [ Node n Nothing | n <- [0 .. toEnum (v - 1)] ] []

      it "always produces v2^v1 morphisms" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (v2 ^ v1)
          (length (findMorphisms anyMorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces v2!/(v2-v1)! monomorphisms when v2 >= v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [v1..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (foldl' (*) 1 $ take v1 [v2, v2-1 .. 1])
          (length (findMorphisms monic (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no monomorphisms when v2 < v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..v1-1] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms monic (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces S(v1, v2) * v2! epimorphisms when v2 <= v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..v1] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (fromInteger (stirling2nd v1 v2) * foldl' (*) 1 [1..v2])
          (length (findMorphisms epic (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no epimorphisms when v2 > v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [v1+1..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms epic (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no isomorphism when v1 /= v2" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..maxGraphSize] $ \v2 ->
        when (v1 /= v2) $
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms iso (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces v1! isomorphisms when v1 == v2" $
        forM_ [1..maxGraphSize] $ \v ->
        assertEqual ("with " ++ show (v, v))
          (foldl' (*) 1 [1..v])
          (length (findMorphisms iso (makeGraph v) (makeGraph v) :: [LabeledMorphism]))


    context "between graphs with one node and no labels" $ do
      let
        makeGraph e =
          fromNodesAndEdges [Node 0 Nothing] [ Edge x 0 0 () | x <- [0 .. toEnum (e - 1)] ]

      it "always produces e2^e1 morphisms" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (e2 ^ e1)
          (length (findMorphisms anyMorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces e2!/(e2-e1)! monomorphisms when e2 >= e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [e1..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (foldl' (*) 1 $ take e1 [e2, e2-1 .. 1])
          (length (findMorphisms monic (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no monomorphisms when e2 < e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..e1-1] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms monic (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces S(e1, e2) * e2! epimorphisms when e2 <= e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..e1] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (fromInteger (stirling2nd e1 e2) * foldl' (*) 1 [1..e2])
          (length (findMorphisms epic (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no epimorphisms when e2 > e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [e1+1..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms epic (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no isomorphism when e1 /= e2" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..maxGraphSize] $ \e2 ->
        when (e1 /= e2) $
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms iso (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces v1! isomorphisms when e1 == e2" $
        forM_ [1..maxGraphSize] $ \e ->
        assertEqual ("with " ++ show (e, e))
          (foldl' (*) 1 [1..e])
          (length (findMorphisms iso (makeGraph e) (makeGraph e) :: [LabeledMorphism]))


    context "producing Monomorphisms" $ do
      prop "always produces valid morphisms" $ \domain codomain ->
        forAllMorphismsBetween monic domain codomain $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces monomorphisms" $ \domain codomain ->
        forAllMorphismsBetween monic domain codomain $ \m ->
          isMonic (m :: LabeledMorphism)

      prop "behaves like brute-force search then filter" $ \domain codomain ->
        let expected = filter isMonic (findAllMorphisms domain codomain)
        in equalLists expected (findMorphisms monic domain codomain :: [LabeledMorphism])

    context "producing Epimorphisms" $ do
      prop "always produces valid morphisms" $ \codomain ->
        forAll (randomSubgraphOf codomain) $ \domain ->
        forAllMorphismsBetween epic domain codomain $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces epimorphisms" $ \codomain ->
        forAll (randomSubgraphOf codomain) $ \domain ->
        forAllMorphismsBetween epic domain codomain $ \m ->
          isEpic (m :: LabeledMorphism)

      prop "behaves like brute-force search then filter" $ \domain codomain ->
        let expected = filter isEpic (findAllMorphisms domain codomain) :: [LabeledMorphism]
        in equalLists expected (findMorphisms epic domain codomain)


    context "producing Isomorphisms" $ do
      prop "always produces valid morphisms" $ \graph ->
        forAllMorphismsBetween iso graph graph $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces isomorphisms" $ \graph ->
        forAllMorphismsBetween iso graph graph $ \m ->
          isIsomorphism (m :: LabeledMorphism)

      prop "always produces epimorphisms" $ \graph ->
        forAllMorphismsBetween iso graph graph $ \m ->
          isEpic (m :: LabeledMorphism)

      prop "always produces monomorphisms" $ \graph ->
        forAllMorphismsBetween iso graph graph $ \m ->
          isMonic (m :: LabeledMorphism)

      prop "behaves like brute-force search then filter" $ \domain codomain ->
        let expected = filter isIsomorphism (findAllMorphisms domain codomain) :: [LabeledMorphism]
        in equalLists expected (findMorphisms iso domain codomain)

    it "doesn't map labeled nodes to unlabeled nodes" $ do
      let g1 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["x"]), Node 1 (Just $ Variable 4 ["y"])] []
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 (Just $ Variable 5 ["a"]), Node 2 (Just $ Variable 6 ["b"])] []
      let morphisms = findMorphisms anyMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 4
      forM_ morphisms $ \m -> do
        lookupNodeId 0 m `shouldNotBe` Just 0
        lookupNodeId 1 m `shouldNotBe` Just 0

    it "restricts the mapping of nodes with same label" $ do
      let g1 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["x"]), Node 1 (Just $ Variable 3 ["x"])] []
      let g2 = fromNodesAndEdges [Node 0 (Just $ Variable 4 ["a"]), Node 1 (Just $ Variable 5 ["b"])] []
      let morphisms = findMorphisms anyMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupNodeId 0 m `shouldBe` lookupNodeId 1 m

    it "restricts the mapping of edges with same source" $ do
      let g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 1 ()]
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 1 ()]
      let morphisms = findMorphisms anyMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupEdgeId 0 m `shouldBe` lookupEdgeId 1 m

    it "restricts the mapping of edges with same target" $ do
      let g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 0 ()]
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 1 ()]
      let morphisms = findMorphisms anyMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupEdgeId 0 m `shouldBe` lookupEdgeId 1 m


  describe "findCospanCommuters" $ modifyMaxDiscardRatio (const 500) $ do

    context "producing any morphisms" $ modifyMaxSize (const 25) $ modifyMaxSuccess (const 70) $
      prop "behaves as exhaustive search then filter" $ \g1 g2 g3 ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \left ->
        forAllMorphismsBetween anyMorphism g3 g2 $ \right ->
        let
          expected = filter (\m -> right <&> m == left) (findAllMorphisms g1 g3)
        in
          equalLists (expected :: [LabeledMorphism]) (findCospanCommuters anyMorphism left right)

    context "producing monomorphisms" $ modifyMaxSize (const 25) $
      prop "behaves as exhaustive search then filter" $ \g2 g3 ->
        forAll (randomSubgraphOf g3) $ \g1 ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \left ->
        forAllMorphismsBetween anyMorphism g3 g2 $ \right ->
        let expected = filter (\m -> right <&> m == left) (findMonomorphisms g1 g3)
        in equalLists (expected :: [LabeledMorphism]) (findCospanCommuters monic left right)

    context "producing epimorphisms" $ modifyMaxSize (const 25) $
      prop "behaves as exhaustive search then filter" $ \g1 g2 ->
        forAll (randomSubgraphOf g1) $ \g3 ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \left ->
        forAllMorphismsBetween anyMorphism g3 g2 $ \right ->
        let expected = filter (\m -> right <&> m == left) (findEpimorphisms g1 g3)
        in equalLists (expected :: [LabeledMorphism]) (findCospanCommuters epic left right)

    context "producing isomorphisms" $ modifyMaxSize (const 25) $ do
      prop "behaves as exhaustive search then filter" $ \g1 g2 ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \left ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \right ->
        let expected = filter (\m -> right <&> m == left) (findIsomorphisms g1 g1)
        in equalLists (expected :: [LabeledMorphism]) (findCospanCommuters iso left right)

      prop "finds some isomorphism when left = right" $ \g1 g2 ->
        forAllMorphismsBetween anyMorphism g1 g2 $ \leftAndRight ->
        let
          expected = filter (\m -> leftAndRight <&> m == leftAndRight) (findIsomorphisms g1 g1)
          result = findCospanCommuters iso leftAndRight leftAndRight :: [LabeledMorphism]
        in result /= [] && equalLists result expected
