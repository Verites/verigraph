{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.CocompleteSpec where

import           Data.Foldable
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Abstract.Category.Cocomplete
import           Abstract.Category.FinitaryCategory
import           Base.Isomorphic
import           Base.Valid
import           Category.LabeledGraph
import           Data.EnumMap                       (EnumMap)
import qualified Data.EnumMap                       as EnumMap
import           Data.EnumSet                       (EnumSet)
import qualified Data.EnumSet                       as EnumSet
import           Data.LabeledGraph                  as Graph
import           Data.LabeledGraph.Morphism         as Morphism
import           Data.LabeledGraph.QuickCheck       ()
import           Data.Variable
import           Test.QuickCheck
import           Util.Test


maxGraphSize :: Int
maxGraphSize = 5

withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs = modifyMaxSize (`div` 4)


spec :: Spec
spec = do

  describe "initialObject" $ do
    it "is valid" $
      initialObject (undefined :: LabeledMorphism) `shouldSatisfy` isValid

    it "is the empty graph" $
      initialObject (undefined :: LabeledMorphism) `shouldBe` Graph.empty

    prop "has a unique morphism to any other graph" $ \g ->
      let initial = initialObject (undefined :: LabeledMorphism)
      in length (findAllMorphisms initial g :: [LabeledMorphism]) == 1

  describe "calculateCoproduct" $ do
    prop "has valid embeddings" $ \g1 g2 ->
      let (inj1, inj2) = calculateCoproduct g1 g2
      in isValid inj1 && isValid (inj2 :: LabeledMorphism)

    prop "has monic embeddings" $ \g1 g2 ->
      let (inj1, inj2) = calculateCoproduct g1 g2
      in isMonomorphism inj1 && isMonomorphism (inj2 :: LabeledMorphism)

    prop "has embeddings with same codomain" $ \g1 g2 ->
      let (inj1, inj2) = calculateCoproduct g1 g2
      in codomain inj1 == codomain (inj2 :: LabeledMorphism)

    prop "has embeddings with disjoint images" $ \g1 g2 ->
      let
        (inj1, inj2) = calculateCoproduct g1 g2
        nodeImageIntersection = EnumSet.intersection
          (image $ Morphism.nodeMap inj1)
          (image $ Morphism.nodeMap inj2)
        edgeImageIntersection = EnumSet.intersection
          (image $ Morphism.edgeMap inj1)
          (image $ Morphism.edgeMap inj2)
        varImageIntersection = EnumSet.intersection
          (image $ Morphism.variableMap inj1)
          (image $ Morphism.variableMap inj2)
      in EnumSet.null nodeImageIntersection && EnumSet.null edgeImageIntersection && EnumSet.null varImageIntersection

    prop "is jointly surjective" $ \g1 g2 ->
      let
        (inj1, inj2) = calculateCoproduct g1 g2
        coproductObj = codomain inj1
        nodeImages = EnumSet.union (image $ Morphism.nodeMap inj1) (image $ Morphism.nodeMap inj2)
        edgeImages = EnumSet.union (image $ Morphism.edgeMap inj1) (image $ Morphism.edgeMap inj2)
        varImages = EnumSet.union (image $ Morphism.variableMap inj1) (image $ Morphism.variableMap inj2)
      in
        nodeImages == EnumSet.fromList (nodeIds coproductObj)
          && edgeImages == EnumSet.fromList (edgeIds coproductObj)
          && varImages == freeVariableSet coproductObj


  describe "calculateNCoproduct" $ modifyMaxSuccess (const 100) $
    prop "is the same as folding calculateProduct" $ \g gs ->
      let
        expected = foldl' binaryCoproduct [identity g :: LabeledMorphism] (gs :: [LabeledGraph])
        binaryCoproduct :: [LabeledMorphism] -> LabeledGraph -> [LabeledMorphism]
        binaryCoproduct injs g =
          let (inj1, inj2) = calculateCoproduct (codomain $ head injs) g
          in map (inj1 <&>) injs ++ [inj2]
        actual = calculateNCoproduct (g :| gs)
      in
        expected ~= actual

  describe "calculateCoequalizer" $ modifyMaxSize (const 25) $ modifyMaxSuccess (const 100) $ do
    it "collapses only the necessary nodes" $ do
      let a = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing] []
      let b = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing, Node 3 Nothing] []
      let f = fromGraphsAndLists a b  [(0,0), (1,1), (2,2)] [] []
      let g = fromGraphsAndLists a b [(0,1), (1,1), (2,2)] [] []
      let expectedCoeqObject = fromNodesAndEdges [Node 4 Nothing, Node 5 Nothing, Node 6 Nothing] []
      let expectedCoeq = fromGraphsAndLists b expectedCoeqObject [(0,4), (1,4), (2,5), (3,6)] [] []
      calculateCoequalizer f g `shouldBeIsomorphicTo` expectedCoeq

    it "collapses only the necessary edges" $ do
      let a = fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 (), Edge 1 0 0 (), Edge 2 0 0 ()]
      let b = fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 (), Edge 1 0 0 (), Edge 2 0 0 (), Edge 3 0 0 ()]
      let f = fromGraphsAndLists a b [(0,0)] [(0,0), (1,1), (2,2)] []
      let g = fromGraphsAndLists a b [(0,0)] [(0,1), (1,1), (2,2)] []
      let expectedCoeqObject = fromNodesAndEdges [Node 1 Nothing] [Edge 4 1 1 (), Edge 5 1 1 (), Edge 6 1 1 ()]
      let expectedCoeq = fromGraphsAndLists b expectedCoeqObject [(0,1)] [(0,4), (1,4), (2,5), (3,6)] []
      calculateCoequalizer f g `shouldBeIsomorphicTo` expectedCoeq

    it "collapses only the necessary variables" $ do
      let a = fromNodesAndEdges [Node 0 $ Just $ Variable 1 ["x"], Node 1 $ Just $ Variable 2 ["y"], Node 2 $ Just $ Variable 3 ["z"]] []
      let f = identity a :: LabeledMorphism
      let g = fromGraphsAndLists a a [(0,1), (1,0), (2,2)] [] [(1,2), (2,1)]
      let expectedCoeqObject = fromNodesAndEdges [Node 0 $ Just $ Variable 4 ["a"], Node 2 $ Just $ Variable 5 ["b"]] []
      let expectedCoeq = fromGraphsAndLists a expectedCoeqObject [(0,0), (1,0), (2,2)] [] [(0,4), (1,4), (2,5)]
      calculateCoequalizer f g `shouldBeIsomorphicTo` expectedCoeq

    prop "is valid" $ \g1 g2 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \g ->
        isValid (calculateCoequalizer f g :: LabeledMorphism)

    prop "has the correct domain" $ \g1 g2 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \g ->
        domain (calculateCoequalizer f g :: LabeledMorphism) == g2

    prop "is surjective" $ \g1 g2 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \g ->
        isSurjective (calculateCoequalizer f g :: LabeledMorphism)

    prop "coequalizes the morphisms" $ \g1 g2 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \g ->
      let coeq = calculateCoequalizer f g :: LabeledMorphism
      in coeq <&> f == coeq <&> g



  describe "calculateNCoequalizer" $ modifyMaxSuccess (const 100) $ modifyMaxSize (const 25) $ do
    prop "is the same as calculateCoequalizer when given two morphisms" $ \a b ->
      forAllMorphismsBetween GenericMorphism a b $ \f ->
      forAllMorphismsBetween GenericMorphism a b $ \g ->
        calculateNCoequalizer (f :| [g]) ~= calculateCoequalizer f (g :: LabeledMorphism)

    prop "is the same as folding calculateCoequalizer" $ \a b ->
      forAllMorphismsBetween GenericMorphism a b $ \f ->
      forAll (sublistOf $ findAllMorphisms a b) $ \fs ->
      let
        (expected, _) = foldl' binaryCoequalizer (identity (codomain f), f :: LabeledMorphism) fs
        binaryCoequalizer (prevCoequalizer, composed) f =
          let coequalizerStep = calculateCoequalizer composed (prevCoequalizer <&> f)
          in (coequalizerStep <&> prevCoequalizer, coequalizerStep <&> composed)
        actual = calculateNCoequalizer (f :| fs)
      in
        expected ~= actual



image :: (Enum k, Enum a) => EnumMap k a -> EnumSet a
image = EnumSet.fromList . EnumMap.elems
