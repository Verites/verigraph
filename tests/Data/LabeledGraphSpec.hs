{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Data.LabeledGraphSpec where

import           Test.Hspec

import qualified Data.EnumMap                 as EnumMap
import           Data.LabeledGraph            as Graph
import           Data.LabeledGraph.QuickCheck ()
import           Data.Variable


spec :: Spec
spec = do
  describe "instance FreeVariables LNode" $ do

    describe "renameVariable" $ do
      let rename0to1 = renameVariable @LNode 0 (Variable 1 ["y"])
      it "renames the intended variable" $
        rename0to1 (Node 0 $ Just $ Variable 0 ["x"]) `shouldBe` Node 0 (Just $ Variable 1 ["y"])
      it "preserves other variables" $
        rename0to1 (Node 0 $ Just $ Variable 2 ["z"]) `shouldBe` Node 0 (Just $ Variable 2 ["z"])
      it "preserves unlabeled nodes" $
        rename0to1 (Node 0 Nothing) `shouldBe` Node 0 Nothing

    describe "renameVariables" $ do
      let rename = renameVariables @LNode $
            EnumMap.fromList [(0, Variable 10 ["x"]), (1, Variable 11 ["y"])]
      it "renames the intended variables" $ do
        rename (Node 0 $ Just $ Variable 0 ["a"]) `shouldBe` Node 0 (Just $ Variable 10 ["x"])
        rename (Node 0 $ Just $ Variable 1 ["b"]) `shouldBe` Node 0 (Just $ Variable 11 ["y"])
      it "preserves other variables" $
        rename (Node 0 $ Just $ Variable 3 ["c"]) `shouldBe` Node 0 (Just $ Variable 3 ["c"])
      it "preserves unlabeled nodes" $
        rename (Node 0 Nothing) `shouldBe` Node 0 Nothing


  describe "instance FreeVariables LNode" $ do
    let graph = Graph.fromNodesAndEdges
          [Node 0 Nothing, Node 1 (Just $ Variable 4 ["a"]), Node 2 (Just $ Variable 5 ["b"]), Node 3 (Just $ Variable 6 ["c"])] []

    describe "renameVariable" $ do
      let renamed = renameVariable 4 (Variable 10 ["x"]) graph
      it "renames the intended variable" $
        lookupNode 1 renamed `shouldBe` Just (Node 1 (Just $ Variable 10 ["x"]))
      it "preserves other variables" $ do
        lookupNode 2 renamed `shouldBe` lookupNode 2 graph
        lookupNode 3 renamed `shouldBe` lookupNode 3 graph
      it "preserves unlabeled nodes" $
        lookupNode 0 renamed `shouldBe` lookupNode 0 graph

    describe "renameVariables" $ do
      let subst = EnumMap.fromList [(4, Variable 10 ["x"]), (5, Variable 11 ["y"])]
      let renamed = renameVariables subst graph
      it "renames the intended variables" $ do
        lookupNode 1 renamed `shouldBe` Just (Node 1 (Just $ Variable 10 ["x"]))
        lookupNode 2 renamed `shouldBe` Just (Node 2 (Just $ Variable 11 ["y"]))
      it "preserves other variables" $
        lookupNode 3 renamed `shouldBe` lookupNode 3 graph
      it "preserves unlabeled nodes" $
        lookupNode 0 renamed `shouldBe` lookupNode 0 graph
