{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Category.TypedGraph.FinalPullbackComplementSpec where

import           Test.Hspec

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import qualified Category.TypedGraph          as TGraph
import qualified Data.Graphs                  as Graph
import qualified Data.Graphs.Morphism         as G
import qualified Data.TypedGraph              as TG
import           Data.TypedGraph.Morphism
import qualified XML.GGXReader                as XML

spec :: Spec
spec = context "Final Pullback Complement" fpbcTest

fpbcTest :: Spec
fpbcTest =
  it "Should return precalculated results" $ do

  let fileName = "tests/grammars/fpbc.ggx"
  (gg,_,_) <- XML.readGrammar fileName False undefined

  let (r1:r2:r3:_) = map snd (productions gg)
      test1 = prepareTest1 r1
      test2 = prepareTest2 r2
      test3 = prepareTest3 r3

  verifyAnyPullback test1
  verifyR1 test1

  verifyAnyPullback test2
  verifyR2 test2

  verifyAnyPullback test3
  verifyR3 test3

-- Gets rules and set them to appropriated morphisms tests
prepareTest1 r1 = TGraph.runCat config $ do
  let m = leftMorphism r1
  let l = invert (rightMorphism r1)
  (k,l') <- calculateFinalPullbackComplementOfMonoAny m l
  return (m,l,k,l')

prepareTest2 r2 = TGraph.runCat config $ do
  let
    m = leftMorphism r2
    l = foldr
          removeNodeFromDomain
          (invert (rightMorphism r2))
          (nodeIdsFromCodomain (rightMorphism r2))
  (k,l') <- calculateFinalPullbackComplementOfMonoAny m l
  return (m,l,k,l')

prepareTest3 r3 = TGraph.runCat config $ do
  let
    m = leftMorphism r3
    node = head (nodeIdsFromCodomain (rightMorphism r3))
    l = createNodeOnDomain
          (node + Graph.NodeId 1)
          (G.applyNodeIdUnsafe (rightObject r3) node)
          (head (nodeIdsFromDomain (rightMorphism r3)))
          (invert (rightMorphism r3))
  (k, l') <- calculateFinalPullbackComplementOfMonoAny m l
  return (m,l,k,l')

-- Generic tests for any pullback square
verifyAnyPullback (m,l,k,l') =
  do
    domain l `shouldBe` domain k
    codomain l `shouldBe` domain m
    codomain k `shouldBe` domain l'
    codomain m `shouldBe` codomain l'
    m <&> l `shouldBe` l' <&> k
    isMonomorphism l `shouldBe` isMonomorphism l'
    isMonomorphism m `shouldBe` isMonomorphism k

isMonomorphism = TGraph.runCat config . isMonic

config = TGraph.Config Graph.empty TGraph.AllMatches

-- Specific tests

verifyR1 (_,_,k,l') =
  do
    length (nodeIdsFromCodomain k) `shouldBe` 1
    length (edgeIdsFromCodomain k) `shouldBe` 1
    TGraph.runCat config (isIsomorphism l') `shouldBe` True

verifyR2 (_,_,k,l') = TG.null (codomain k) `shouldBe` True

verifyR3 (_,_,k,l') =
  do
    length (nodeIdsFromCodomain k) `shouldBe` 2
    length (edgeIdsFromCodomain k) `shouldBe` 4
