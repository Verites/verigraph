{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.JointlyEpimorphismsSpec where

import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Math.Combinat.Numbers                 (bellNumber)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Abstract.Category.FinitaryCategory
import           Abstract.Category.JointlyEpimorphisms
import           Base.Isomorphic                       ()
import           Base.Valid
import           Category.LabeledGraph
import           Data.EnumMap                          (EnumMap)
import qualified Data.EnumMap                          as EnumMap
import           Data.EnumSet                          (EnumSet)
import qualified Data.EnumSet                          as EnumSet
import           Data.LabeledGraph                     as Graph
import           Data.LabeledGraph.Morphism            as Morphism
import           Data.LabeledGraph.QuickCheck          ()
import           Data.Variable
import           Util.Test


maxGraphSize :: Int
maxGraphSize = 5

withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs = modifyMaxSize (`div` 4)

modifyNumTestCases :: Int -> SpecWith a -> SpecWith a
modifyNumTestCases x = modifyMaxSize (const x) . modifyMaxSuccess (const $ x + 1)


spec :: Spec
spec = withSmallerGraphs $ do

  describe "createAllQuotients" $ do
    prop "produces valid epimorphisms" $ \g ->
      all (\m -> isValid m && isEpimorphism m) (createAllQuotients g :: [LabeledMorphism])

    prop "has the correct domain" $ \g ->
      all (\m -> domain m == g) (createAllQuotients g :: [LabeledMorphism])


    context "with no edges" $ modifyNumTestCases 15 $
      prop "produces the correct number of quotient graphs" $ \(NonNegative numNodes) ->
        let
          g = fromNodesAndEdges [ Node n Nothing | n <- [0..numNodes-1] ] []
        in
          integerLength (createAllQuotients g :: [LabeledMorphism]) == bellNumber numNodes

    context "with one node and multiple edges" $ modifyNumTestCases 15 $
      prop "produces the correct number of quotient graphs" $ \(NonNegative numEdges) ->
        let
          g = fromNodesAndEdges [Node 0 Nothing][ Edge e 0 0 () | e <- [0..numEdges-1] ]
        in
          integerLength (createAllQuotients g :: [LabeledMorphism]) == bellNumber numEdges

    it "produces all collapses of unlabeled nodes" $
      let
        g = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing] []
      in
        createAllQuotients g `shouldBeIsomorphicTo` partitions g
          [ V []
            [ N [Node 3 Nothing]                [(0,3), (1,3), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,4), (1,3), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,3), (1,4), (2,3)] [E [] []]
            , N [Node i Nothing | i <- [3,4]]   [(0,3), (1,3), (2,4)] [E [] []]
            , N [Node i Nothing | i <- [3,4,5]] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          ]

    it "collapses nodes coherently with variables" $
      let
        g = fromNodesAndEdges [ Node 0 Nothing, Node 1 (Just "x1"), Node 2 (Just "x2")] []
      in
        createAllQuotients g `shouldBeIsomorphicToList` partitions g
          [ V [("x1","x3"), ("x2","x3")]
            [ N [Node 3 (Just "x3")]                                     [(0,3), (1,3), (2,3)] [E [] []]
            , N [Node 3 (Just "x3"), Node 4 Nothing]                     [(0,4), (1,3), (2,3)] [E [] []]
            , N [Node 3 (Just "x3"), Node 4 (Just "x3")]                 [(0,3), (1,4), (2,3)] [E [] []]
            , N [Node 3 (Just "x3"), Node 4 (Just "x3")]                 [(0,3), (1,3), (2,4)] [E [] []]
            , N [Node 3 Nothing, Node 4 (Just "x3"), Node 5 (Just "x3")] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          , V [("x1","x3"), ("x2","x4")]
            [ N [Node 4 (Just "x3"), Node 5 (Just "x4")]                 [(0,5), (1,4), (2,5)] [E [] []]
            , N [Node 4 (Just "x3"), Node 5 (Just "x4")]                 [(0,4), (1,4), (2,5)] [E [] []]
            , N [Node 3 Nothing, Node 4 (Just "x3"), Node 5 (Just "x4")] [(0,3), (1,4), (2,5)] [E [] []]
            ]
          ]

    it "collapses edges coherently with nodes" $
      let
        g = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 0 (), Edge 2 0 1 ()]
      in
        createAllQuotients g `shouldBeIsomorphicToList` partitions g
          [ V []
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


data VarPartition = V [(Variable, Variable)] [NodePartition]
data NodePartition = N [LNode] [(NodeId, NodeId)] [EdgePartition]
data EdgePartition = E [LEdge] [(EdgeId, EdgeId)]


partitions :: LabeledGraph -> [VarPartition] -> [LabeledMorphism]
partitions domain varTree = do
  V varMap nodeTree <- varTree
  N nodes nodeMap edgeList <- nodeTree
  E edges edgeMap <- edgeList
  return $ fromGraphsAndLists domain (fromNodesAndEdges nodes edges) nodeMap edgeMap varMap


integerLength :: [a] -> Integer
integerLength = fromIntegral . length

enumMapImage :: (Enum k, Enum a) => EnumMap k a -> EnumSet a
enumMapImage = EnumSet.fromList . EnumMap.elems

mapImage :: Ord a => Map k a -> Set a
mapImage = Set.fromList . Map.elems
