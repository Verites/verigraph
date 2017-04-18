{-# LANGUAGE OverloadedStrings #-}
module Data.LabeledGraphSpec where

import qualified Data.Map                     as Map
import           Test.Hspec

import           Data.LabeledGraph            as Graph
import           Data.LabeledGraph.QuickCheck ()
import           Data.Variable


spec :: Spec
spec = do
  describe "instance FreeVariables LNode" $ do

    describe "renameVariable" $ do
      it "renames the intended variable" $
        renameVariable "x" "y" (Node 0 $ Just "x") `shouldBe` (Node 0 (Just "y") :: LNode)
      it "preserves other variables" $
        renameVariable "x" "y" (Node 0 $ Just "z") `shouldBe` (Node 0 (Just "z") :: LNode)
      it "preserves unlabeled nodes" $
        renameVariable "x" "y" (Node 0 Nothing) `shouldBe` (Node 0 Nothing :: LNode)

    describe "renameVariables" $ do
      let subst = Map.fromList [("a", "x"), ("b", "y")] :: Renaming
      it "renames the intended variables" $ do
        renameVariables subst (Node 0 $ Just "a") `shouldBe` (Node 0 (Just "x") :: LNode)
        renameVariables subst (Node 0 $ Just "b") `shouldBe` (Node 0 (Just "y") :: LNode)
      it "preserves other variables" $
        renameVariables subst (Node 0 $ Just "c") `shouldBe` (Node 0 (Just "c") :: LNode)
      it "preserves unlabeled nodes" $
        renameVariables subst (Node 0 Nothing) `shouldBe` (Node 0 Nothing :: LNode)


  describe "instance FreeVariables LNode" $ do
    let graph = Graph.fromNodesAndEdges [Node 0 Nothing, Node 1 (Just "a"), Node 2 (Just "b"), Node 3 (Just "c")] []

    describe "renameVariable" $ do
      let renamed = renameVariable "a" "x" graph
      it "renames the intended variable" $
        lookupNode 1 renamed `shouldBe` Just (Node 1 (Just "x"))
      it "preserves other variables" $ do
        lookupNode 2 renamed `shouldBe` lookupNode 2 graph
        lookupNode 3 renamed `shouldBe` lookupNode 3 graph
      it "preserves unlabeled nodes" $
        lookupNode 0 renamed `shouldBe` lookupNode 0 graph

    describe "renameVariables" $ do
      let subst = Map.fromList [("a", "x"), ("b", "y")] :: Renaming
      let renamed = renameVariables subst graph
      it "renames the intended variables" $ do
        lookupNode 1 renamed `shouldBe` Just (Node 1 (Just "x"))
        lookupNode 2 renamed `shouldBe` Just (Node 2 (Just "y"))
      it "preserves other variables" $
        lookupNode 3 renamed `shouldBe` lookupNode 3 graph
      it "preserves unlabeled nodes" $
        lookupNode 0 renamed `shouldBe` lookupNode 0 graph
