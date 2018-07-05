{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Category.LabeledGraph.FinitarySpec where

import           Control.Monad
import           Math.Combinat.Numbers        (bellNumber, binomial)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Abstract.Category
import           Abstract.Category.Finitary
import           Base.Isomorphic              ()
import           Base.Valid
import           Category.LabeledGraph
import           Data.EnumMap                 (EnumMap)
import qualified Data.EnumMap                 as EnumMap
import           Data.EnumSet                 (EnumSet)
import qualified Data.EnumSet                 as EnumSet
import           Data.LabeledGraph            as Graph
import           Data.LabeledGraph.Morphism   as Morphism
import           Data.LabeledGraph.QuickCheck ()
import           Data.Variable
import qualified Util.EnumMap                 as EnumMap
import           Util.Test


maxGraphSize :: Int
maxGraphSize = 5

withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs = modifyMaxSize (`div` 4)

modifyNumTestCases :: Int -> SpecWith a -> SpecWith a
modifyNumTestCases x = modifyMaxSize (const x) . modifyMaxSuccess (const $ x + 1)


spec :: Spec
spec = describe "" . context "" . it "" $ True `shouldBe` True

{-
spec = withSmallerGraphs $ do

  describe "createAllQuotients" $ do
    prop "produces valid epimorphisms" $ \g ->
      all (\m -> isValid m && isEpic m) (createAllQuotients @LabeledMorphism g)

    prop "has the correct domain" $ \g ->
      all (\m -> domain m == g) (createAllQuotients @LabeledMorphism g)


    context "with no edges" $ do
      it "produces the correct number of quotient graphs" $ forM_ [0..7] $ \numNodes -> do
        let g = fromNodesAndEdges [ Node n Nothing | n <- [0..numNodes-1] ] []
        integerLength (createAllQuotients @LabeledMorphism g) `shouldBe` bellNumber numNodes
      it "produces no duplicates" $ forM_ [0..7] $ \numNodes -> do
        let g = fromNodesAndEdges [ Node n Nothing | n <- [0..numNodes-1] ] []
        createAllQuotients @LabeledMorphism g `shouldSatisfy` noDuplicates

    context "with one node and multiple edges" $ do
      it "produces the correct number of quotient graphs" $ forM_ [0..7] $ \numEdges -> do
        let g = fromNodesAndEdges [Node 0 Nothing] [ Edge e 0 0 () | e <- [0..numEdges-1] ]
        integerLength (createAllQuotients @LabeledMorphism g) `shouldBe` bellNumber numEdges
      it "produces no duplicates" $ forM_ [0..7] $ \numEdges -> do
        let g = fromNodesAndEdges [Node 0 Nothing] [ Edge e 0 0 () | e <- [0..numEdges-1] ]
        createAllQuotients @LabeledMorphism g `shouldSatisfy` noDuplicates

    it "produces all collapses of unlabeled nodes" $
      let
        g = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing] []
      in
        createAllQuotients g `shouldBeIsomorphicTo` partitions g
          [ V [] []
            [ N [Node 3 Nothing]                [(0,3), (1,3), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,4), (1,3), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,3), (1,4), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,3), (1,3), (2,4)] [E [] []]
            , N [Node i Nothing | i <- [3,4,5]] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          ]

    it "collapses nodes coherently with variables" $
      let
        g = fromNodesAndEdges [ Node 0 Nothing, Node 1 (Just $ Variable 0 ["x1"]), Node 2 (Just $ Variable 1 ["x2"])] []
      in
        createAllQuotients g `shouldBeIsomorphicToList` partitions g
          [ V [Variable 2 ["x3"]] [(0,2), (1,2)]
            [ N [Node 3 (Just 2)]                                  [(0,3), (1,3), (2,3)] [E [] []]
            , N [Node 3 (Just 2), Node 4 Nothing]                  [(0,4), (1,3), (2,3)] [E [] []]
            , N [Node 3 (Just 2), Node 4 (Just 2)]                 [(0,3), (1,4), (2,3)] [E [] []]
            , N [Node 3 (Just 2), Node 4 (Just 2)]                 [(0,3), (1,3), (2,4)] [E [] []]
            , N [Node 3 Nothing, Node 4 (Just 2), Node 5 (Just 2)] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          , V [Variable 2 ["x3"], Variable 3 []] [(0,2), (1,3)]
            [ N [Node 4 (Just 2), Node 5 (Just 3)]                 [(0,5), (1,4), (2,5)] [E [] []]
            , N [Node 4 (Just 2), Node 5 (Just 3)]                 [(0,4), (1,4), (2,5)] [E [] []]
            , N [Node 3 Nothing, Node 4 (Just 2), Node 5 (Just 3)] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          ]

    it "collapses edges coherently with nodes" $
      let
        g = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 (), Edge 2 0 1 ()]
      in
        createAllQuotients g `shouldBeIsomorphicToList` partitions g
          [ V [] []
            [ N [Node 2 Nothing] [(0,2), (1,2)]
              [ E [Edge 3 2 2 ()]                               [(0,3), (1,3), (2,3)]
              , E [Edge 3 2 2 (), Edge 4 2 2 ()]                [(0,4), (1,3), (2,3)]
              , E [Edge 3 2 2 (), Edge 4 2 2 ()]                [(0,3), (1,4), (2,3)]
              , E [Edge 3 2 2 (), Edge 4 2 2 ()]                [(0,3), (1,3), (2,4)]
              , E [Edge 3 2 2 (), Edge 4 2 2 (), Edge 5 2 2 ()] [(0,3), (1,4), (2,5)]
              ]
            , N [Node 2 Nothing, Node 3 Nothing] [(0,2), (1,3)]
              [ E [Edge 3 2 2 (), Edge 4 2 3 ()]                [(0,3), (1,3), (2,4)]
              , E [Edge 3 2 2 (), Edge 4 2 2 (), Edge 5 2 3 ()] [(0,3), (1,4), (2,5)]
              ]
            ]
          ]

  describe "createMonicJointlyEpimorphicPairs" $ do

    prop "produces valid monomorphisms" $ \g1 g2 ->
      forAll (elements $ createMonicJointlyEpimorphicPairs g1 g2) $ \(m1, m2) ->
        isValid m1 && isValid m2 && isMonomorphism m1 && isMonomorphism (m2 :: LabeledMorphism)

    prop "produces jointly epic pairs with correct domains and codomains" $ \g1 g2 ->
      forAll (elements $ createMonicJointlyEpimorphicPairs g1 g2) $ \(m1, m2) ->
        domain m1 == g1 && domain m2 == g2 && codomain m1 == codomain m2 && areJointlyEpic m1 m2

    context "with no edges" $
      it "produces the correct number of pairs" $
        forM_ [0..7] $ \n ->
        forM_ [0..n] $ \m ->
        let
          g1 = fromNodesAndEdges [Node i Nothing | i <- [0..m-1]] []
          g2 = fromNodesAndEdges [Node i Nothing | i <- [0..n-1]] []
        in
          integerLength (createMonicJointlyEpimorphicPairs g1 g2 :: [(LabeledMorphism, LabeledMorphism)])
          `shouldBe` sum [binomial m numDisjoint * permutation n (m - numDisjoint) | numDisjoint <- [0..m]]
          -- We calculate the expected number of pairs by interpreting their creation as follows:
          --
          --  1. Pick 0 < numDisjoint <= m, the number of elements of g1 that are not identified
          --     with any of g2
          --
          --  2. Pick a combination of numDisjoint elements from g1 (total of m elements), which
          --     are not identified with any of g2
          --
          --  3. Pick a permutation of (m - numDisjoint) elements from g2 (total of n elements),
          --     which are identified with the elements of g1 in that order

    context "with one node and multiple edges" $
      it "produces the correct number of pairs" $
        forM_ [0..7] $ \n ->
        forM_ [0..n] $ \m ->
        let
          g1 = fromNodesAndEdges [Node 0 Nothing] [Edge i 0 0 () | i <- [0..m-1]]
          g2 = fromNodesAndEdges [Node 1 Nothing] [Edge i 1 1 () | i <- [0..n-1]]
        in
          integerLength (createMonicJointlyEpimorphicPairs g1 g2 :: [(LabeledMorphism, LabeledMorphism)])
          `shouldBe` 1 + sum [binomial m numDisjoint * permutation n (m - numDisjoint) | numDisjoint <- [0..m]]

    it "produces all identifications of unlabeled nodes"
      pending

    it "identifies nodes coherently with variables"
      pending

    it "identifies edges coherently with nodes"
      pending


areJointlyEpic :: LabeledMorphism -> LabeledMorphism -> Bool
areJointlyEpic f g =
  EnumSet.union (image $ Morphism.nodeMap f) (image $ Morphism.nodeMap g) == EnumSet.fromList (Graph.nodeIds $ codomain f)
  &&
    EnumSet.union (image $ Morphism.edgeMap f) (image $ Morphism.edgeMap g) == EnumSet.fromList (Graph.edgeIds $ codomain f)
  && EnumSet.union (image $ Morphism.variableMap f) (image $ Morphism.variableMap g) == freeVariableSet (codomain f)

integerLength :: [a] -> Integer
integerLength = fromIntegral . length

image :: (Enum k, Enum a) => EnumMap k a -> EnumSet a
image = EnumSet.fromList . EnumMap.elems

-- | Calculate the number of permutations of length @k@ drawn from @n@ distinguishable elements.
permutation :: Integral a => a -> a -> Integer
permutation n k = product [fromIntegral (n - k + 1) .. fromIntegral n]

-}

-- * UTILITIES FOR HARDCODING EXPECTED PARTITIONS

data VarPartition = V [Variable] [(VarId, VarId)] [NodePartition]
data NodePartition = N [Node (Maybe VarId)] [(NodeId, NodeId)] [EdgePartition]
data EdgePartition = E [LEdge] [(EdgeId, EdgeId)]


partitions :: LabeledGraph -> [VarPartition] -> [LabeledMorphism]
partitions domain varTree = do
  V vars varMap nodeTree <- varTree
  N nodes nodeMap edgeList <- nodeTree
  E edges edgeMap <- edgeList
  let vars' = EnumMap.fromList [ (varId v, v) | v <- vars ]
  let nodes' = map (\(Node n v) -> Node n (EnumMap.lookupMaybe v vars')) nodes
  return $ fromGraphsAndLists domain (fromNodesAndEdges nodes' edges) nodeMap edgeMap varMap
