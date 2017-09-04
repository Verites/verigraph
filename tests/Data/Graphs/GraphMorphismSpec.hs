module Data.Graphs.GraphMorphismSpec where

import           Test.Hspec

import           Data.Graphs
import           Data.Graphs.Morphism


spec :: Spec
spec = do

  describe "removeNodeFromDomain" $ do
    it "Should not remove node with incident edges" $ do
      removeNodeFromDomain 1 g1 `shouldBe` g1
      removeNodeFromDomain 2 g1 `shouldBe` g1

    it "Should remove node without incident edges" $ do
      removeNodeFromDomain 3 g1 `shouldBe` g2
      removeNodeFromDomain 4 g1 `shouldBe` g3

  describe "removeNodeFromCodomain" $ do
    it "Should not remove node with incident edges" $ do
      removeNodeFromCodomain 1 g1 `shouldBe` g1
      removeNodeFromCodomain 2 g1 `shouldBe` g1

    it "Should remove node without incident edges" $ do
      removeNodeFromCodomain 3 g1 `shouldBe` g4
      removeNodeFromCodomain 4 g1 `shouldBe` g5

g1' :: Graph (Maybe a) (Maybe b)
g1' = build [1,2,3,4] [(1,1,2)]

g2' :: Graph (Maybe a) (Maybe b)
g2' = build [1,2,4] [(1,1,2)]

g3' :: Graph (Maybe a) (Maybe b)
g3' = build [1,2,3] [(1,1,2)]


g1 :: GraphMorphism (Maybe a) (Maybe b)
g1 = buildGraphMorphism g1' g1' [(1,1),(2,2),(3,3),(4,4)] [(1,1)]

g2 :: GraphMorphism (Maybe a) (Maybe b)
g2 = buildGraphMorphism g2' g1' [(1,1),(2,2),(4,4)] [(1,1)]

g3 :: GraphMorphism (Maybe a) (Maybe b)
g3 = buildGraphMorphism g3' g1' [(1,1),(2,2),(3,3)] [(1,1)]

g4 :: GraphMorphism (Maybe a) (Maybe b)
g4 = buildGraphMorphism g1' g2' [(1,1),(2,2),(4,4)] [(1,1)]

g5 :: GraphMorphism (Maybe a) (Maybe b)
g5 = buildGraphMorphism g1' g3' [(1,1),(2,2),(3,3)] [(1,1)]
