{-# LANGUAGE OverloadedStrings #-}
module LabeledGraph.MorphismSpec where


import qualified Data.Map                as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck         as QuickCheck


import           Abstract.Morphism       as Morphism
import           Abstract.Valid
import           Abstract.Variable
import           LabeledGraph            as Graph
import           LabeledGraph.Morphism   as Morphism
import           LabeledGraph.QuickCheck ()



main :: IO ()
main =
  hspec spec


withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs =
  modifyMaxSize (`div` 4)


spec :: Spec
spec = do

  describe "identity" $ do

    prop "is always valid" $ \g ->
      isValid (Morphism.id g :: LabeledMorphism)

    prop "is always an isomorphism" $ \g ->
      isIsomorphism (Morphism.id g :: LabeledMorphism)

    prop "maps nodes to themselves" $ \g ->
      (`all` nodes g) $ \n ->
        applyToNode n (Morphism.id g) == Just n

    prop "maps edges to themselves" $ \g ->
      (`all` edges g) $ \e ->
        applyToEdge e (Morphism.id g) == Just e

    prop "maps variables to themselves" $ \g ->
      (`all` freeVariablesOf g) $ \v ->
        applyToVariable v (Morphism.id g) == Just v


  describe "compose" $ withSmallerGraphs $ do

    prop "is always valid" $ \g1 g2 g3 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f1 ->
      forAllMorphismsBetween GenericMorphism g2 g3 $ \f2 ->
        isValid (compose f1 f2 :: LabeledMorphism)

    prop "maps elements correctly" $ \g1 g2 g3 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f1 ->
      forAllMorphismsBetween GenericMorphism g2 g3 $ \f2 ->
      let
        mapsCorrectly elements applyTo =
          (`all` elements g1) $ \e ->
            applyTo e (compose f1 f2 :: LabeledMorphism)
              == (applyTo e f1 >>= (`applyTo` f2))
      in
        mapsCorrectly nodes applyToNode
          && mapsCorrectly edges applyToEdge
          && mapsCorrectly freeVariablesOf applyToVariable


  describe "isMonomorphism" $ do
    let
      g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g2 = fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g3 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 ()]

    it "is false when nodes are collapsed" $
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 1)] [] :: LabeledMorphism
      in isMonomorphism m `shouldBe` False

    it "is false when edges are collapsed" $
      let m = fromGraphsAndLists g1 g3 [(0, 0), (1, 1)] [(0, 0), (1, 0)] [] :: LabeledMorphism
      in isMonomorphism m `shouldBe` False

    it "is false when variables are collapsed" $
      let m = fromGraphsAndLists g1 g1 [(0, 0), (1, 1)] [(0, 0), (1, 1)] [("x", "a"), ("y", "a")] :: LabeledMorphism
      in isMonomorphism m `shouldBe` False

    it "is true for a monomorphism" $
      let m = fromGraphsAndLists g2 g1 [(0, 0)] [(0, 0), (1, 1)] [] :: LabeledMorphism
      in isMonomorphism m `shouldBe` True


  describe "isEpimorphism" $ do
    let
      g1 = fromNodesAndEdges [Node 0 (Just "a"), Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g2 = fromNodesAndEdges [Node 0 (Just "a")] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g3 = fromNodesAndEdges [Node 0 (Just "a"), Node 1 Nothing] [Edge 0 0 0 ()]
      g4 = fromNodesAndEdges [Node 0 (Just "a"), Node 1 (Just "b")] [Edge 0 0 0 (), Edge 1 0 0 ()]

    it "is false when some node isn't reached" $
      let m = fromGraphsAndLists g2 g1 [(0, 0)] [(0, 0), (1, 1)] [] :: LabeledMorphism
      in isEpimorphism m `shouldBe` False

    it "is false when some edge isn't reached" $
      let m = fromGraphsAndLists g3 g1 [(0, 0), (1, 1)] [(0, 0)] [] :: LabeledMorphism
      in isEpimorphism m `shouldBe` False

    it "is false when some variable isn't reached" $
      let m = fromGraphsAndLists g1 g4 [(0, 0), (1, 1)] [(0, 0), (1, 1)] [] :: LabeledMorphism
      in isEpimorphism m `shouldBe` False

    it "is true for an epimorphism" $
      let m = fromGraphsAndLists g4 g2 [(0, 0), (1, 0)] [(0, 0), (1, 1)] [] :: LabeledMorphism
      in isEpimorphism m `shouldBe` True


  describe "isIsomorphism" $ withSmallerGraphs $ do

    prop "is true iff isMonomorphism && isEpimorphism" $ \domain codomain ->
      forAllMorphismsBetween GenericMorphism domain codomain $ \m ->
        isIsomorphism (m :: LabeledMorphism) == (isMonomorphism m && isEpimorphism m)


  describe "fromGraphsAndLists" $ do
    let
      g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "y")] [Edge 0 0 0 (), Edge 1 1 1 ()]
      g2 = fromNodesAndEdges [Node 0 (Just "a")] [Edge 0 0 0 ()]

    it "has correct domain and codomain" $ do
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 0)] []
      domain m `shouldBe` g1
      codomain m `shouldBe` g2

    it "maps variables according to the nodes" $ do
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 0)] []
      variableMap m `shouldBe` Map.fromList [("x", "a"), ("y", "a")]


  describe "orphanNodes" $ withSmallerGraphs $ do

    prop "is empty for total morphisms" $ \domain codomain ->
      forAllMorphismsBetween GenericMorphism domain codomain $ \m ->
        orphanNodes m == []

    it "contains all and only domain nodes that aren't mapped" $ do
      let
        g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "y")] [Edge 0 0 0 (), Edge 1 1 1 ()]
        g2 = fromNodesAndEdges [Node 0 (Just "a")] [Edge 0 0 0 ()]
        m = fromGraphsAndLists g1 g2 [(0, 0)] [(0, 0)] []
      orphanNodes m `shouldBe` [1]


  describe "orphanEdges" $ withSmallerGraphs $ do

    prop "is empty for total morphisms" $ \domain codomain ->
      forAllMorphismsBetween GenericMorphism domain codomain $ \m ->
        orphanEdges m == []

    it "contains all and only domain edges that aren't mapped" $ do
      let
        g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "y")] [Edge 0 0 0 (), Edge 1 1 1 ()]
        g2 = fromNodesAndEdges [Node 0 (Just "a")] [Edge 0 0 0 ()]
        m = fromGraphsAndLists g1 g2 [(0, 0)] [(0, 0)] []
      orphanEdges m `shouldBe` [1]


  describe "orphanVariables" $ withSmallerGraphs $ do

    prop "is empty for total morphisms" $ \domain codomain ->
      forAllMorphismsBetween GenericMorphism domain codomain $ \m ->
        orphanVariables m == []

    it "contains all and only domain variables that aren't mapped" $ do
      let
        g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "y")] [Edge 0 0 0 (), Edge 1 1 1 ()]
        g2 = fromNodesAndEdges [Node 0 (Just "a")] [Edge 0 0 0 ()]
        m = fromGraphsAndLists g1 g2 [(0, 0)] [(0, 0)] []
      orphanVariables m `shouldBe` ["y"]



forAllMorphismsBetween :: (QuickCheck.Testable prop, FindMorphism a, Show a) =>
  MorphismType -> Obj a -> Obj a -> (a -> prop) -> Property
forAllMorphismsBetween restriction g1 g2 f =
  let
    morphisms = findMorphisms restriction g1 g2
  in
    morphisms /= [] ==> forAll (elements morphisms) f
