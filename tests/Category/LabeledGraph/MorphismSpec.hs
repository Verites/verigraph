{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Category.LabeledGraph.MorphismSpec where

import qualified Data.Map                           as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Abstract.Category.FinitaryCategory
import           Base.Valid
import           Category.LabeledGraph
import qualified Data.EnumMap                       as EnumMap
import           Data.LabeledGraph                  as Graph
import           Data.LabeledGraph.Morphism         as Morphism
import           Data.LabeledGraph.QuickCheck       ()
import           Data.Variable
import           Util.Test


main :: IO ()
main = hspec spec

withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs = modifyMaxSize (`div` 4)

spec :: Spec
spec = do
  describe "validation" $ do

    it "rejects partial node mappings" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 Nothing] []
      Morphism.fromGraphsAndLists g1 empty [] [] [] `shouldNotSatisfy` isValid

    it "rejects partial edge mappings" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 ()]
      Morphism.fromGraphsAndLists g1 g1 [(0, 0)] [] [] `shouldNotSatisfy` isValid

    it "rejects partial variable mappings" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 (Just $ Variable 0 ["x"])] []
      let g2 = Graph.fromNodesAndEdges [Node 0 Nothing] []
      Morphism.fromGraphsAndLists g1 g2 [] [] [] `shouldNotSatisfy` isValid

    it "rejects mapping an edge and its source inconsistently" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 1 ()]
      let g2 = Graph.fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 1 ()]
      Morphism.fromGraphsAndLists g1 g2 [(0,0), (1,0)] [(0,0)] [] `shouldNotSatisfy` isValid

    it "rejects mapping an edge and its target inconsistently" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 1 ()]
      let g2 = Graph.fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 1 ()]
      Morphism.fromGraphsAndLists g1 g2 [(0,1), (1,1)] [(0,0)] [] `shouldNotSatisfy` isValid

    it "rejects mapping a labeled node to an unlabeled node" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 (Just $ Variable 0 ["x"]), Node 1 Nothing] []
      Morphism.fromGraphsAndLists g1 g1 [(0,1), (1,1)] [] [(0, 0)] `shouldNotSatisfy` isValid

    it "accepts mapping an unlabeled node to a labeled node" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 (Just $ Variable 0 ["x"]), Node 1 Nothing] []
      Morphism.fromGraphsAndLists g1 g1 [(0,0), (1,0)] [] [(0, 0)] `shouldSatisfy` isValid

    it "rejects mapping a node and its label inconsistently" $ do
      let g1 = Graph.fromNodesAndEdges [Node 0 (Just $ Variable 0 ["x"]), Node 1 (Just $ Variable 1 ["y"])] []
      let m = LabeledMorphism g1 g1 (EnumMap.fromList [(0,1), (1,1)]) EnumMap.empty (EnumMap.fromList [(0, 0)])
      m `shouldNotSatisfy` isValid

  describe "identity" $ do

    prop "is always valid" $ \g ->
      isValid (identity @LabeledMorphism g)

    prop "is always an isomorphism" $ \g ->
      isIsomorphism (identity @LabeledMorphism g)

    prop "maps nodes to themselves" $ \g ->
      (`all` nodes g) $ \n ->
        applyToNode n (identity g) == Just n

    prop "maps edges to themselves" $ \g ->
      (`all` edges g) $ \e ->
        applyToEdge e (identity g) == Just e

    prop "maps variables to themselves" $ \g ->
      (`all` freeVariableIdsOf g) $ \v ->
        lookupVarId v (identity g) == Just v


  describe "compose" $ withSmallerGraphs $ do

    prop "is always valid" $ \g1 g2 g3 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f1 ->
      forAllMorphismsBetween GenericMorphism g2 g3 $ \f2 ->
        isValid (compose f2 f1 :: LabeledMorphism)

    prop "maps elements correctly" $ \g1 g2 g3 ->
      forAllMorphismsBetween GenericMorphism g1 g2 $ \f1 ->
      forAllMorphismsBetween GenericMorphism g2 g3 $ \f2 ->
      let
        mapsCorrectly elements applyTo =
          (`all` elements g1) $ \e ->
            applyTo e (compose f2 f1 :: LabeledMorphism)
              == (applyTo e f1 >>= (`applyTo` f2))
      in
        mapsCorrectly nodes applyToNode
          && mapsCorrectly edges applyToEdge
          && mapsCorrectly freeVariableIdsOf lookupVarId


  describe "isMonomorphism" $ do
    let
      g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g2 = fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g3 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 ()]

    it "is false when nodes are collapsed" $
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 1)] []
      in isMonomorphism m `shouldBe` False

    it "is false when edges are collapsed" $
      let m = fromGraphsAndLists g1 g3 [(0, 0), (1, 1)] [(0, 0), (1, 0)] []
      in isMonomorphism m `shouldBe` False

    it "is false when variables are collapsed" $
      let m = fromGraphsAndLists g1 g1 [(0, 0), (1, 1)] [(0, 0), (1, 1)] [(3, 5), (4, 5)]
      in isMonomorphism m `shouldBe` False

    it "is true for a monomorphism" $
      let m = fromGraphsAndLists g2 g1 [(0, 0)] [(0, 0), (1, 1)] []
      in isMonomorphism m `shouldBe` True


  describe "isEpimorphism" $ do
    let
      g1 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["a"]), Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g2 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["a"])] [Edge 0 0 0 (), Edge 1 0 0 ()]
      g3 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["a"]), Node 1 Nothing] [Edge 0 0 0 ()]
      g4 = fromNodesAndEdges [Node 0 (Just $ Variable 3 ["a"]), Node 1 (Just $ Variable 4 ["b"])] [Edge 0 0 0 (), Edge 1 0 0 ()]

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
      g1 = fromNodesAndEdges [Node 0 (Just $ Variable 2 ["x"]), Node 1 (Just $ Variable 3 ["y"])] [Edge 0 0 0 (), Edge 1 1 1 ()]
      g2 = fromNodesAndEdges [Node 0 (Just $ Variable 4 ["a"])] [Edge 0 0 0 ()]

    it "has correct domain and codomain" $ do
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 0)] []
      domain m `shouldBe` g1
      codomain m `shouldBe` g2

    it "maps variables according to the nodes" $ do
      let m = fromGraphsAndLists g1 g2 [(0, 0), (1, 0)] [(0, 0), (1, 0)] []
      variableMap m `shouldBe` EnumMap.fromList [(2, 4), (3, 4)]
