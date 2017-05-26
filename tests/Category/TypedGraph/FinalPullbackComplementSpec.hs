{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Category.TypedGraph.FinalPullbackComplementSpec where

import           Test.Hspec

import           Abstract.Category.FinalPullbackComplement
import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Category.TypedGraph.FinalPullbackComplement
import qualified Data.TypedGraph as TG
import           Data.TypedGraph.Morphism
import qualified Data.Graphs as G
import qualified Data.Graphs.Morphism as G
import qualified XML.GGXReader                    as XML

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
prepareTest1 r1 = (m,l,k,l')
  where
    m = getLHS r1
    l = invert (getRHS r1)
    (k,l') = calculateFinalPullbackComplement m l

prepareTest2 r2 = (m,l,k,l')
  where
    m = getLHS r2
    l = foldr
          removeNodeFromDomain
          (invert (getRHS r2))
          (nodeIdsFromCodomain (getRHS r2))
    (k,l') = calculateFinalPullbackComplement m l

prepareTest3 r3 = (m,l,k,l')
  where
    m = getLHS r3
    node = head (nodeIdsFromCodomain (getRHS r3))
    l = createNodeOnDomain
          (node + G.NodeId 1)
          (G.applyNodeIdUnsafe (codomain (getRHS r3)) node)
          (head (nodeIdsFromDomain (getRHS r3)))
          (invert (getRHS r3))
    (k,l') = calculateFinalPullbackComplement m l

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

-- Specific tests

verifyR1 (_,_,k,l') =
  do
    length (nodeIdsFromCodomain k) `shouldBe` 1
    length (edgeIdsFromCodomain k) `shouldBe` 1
    isIsomorphism l' `shouldBe` True

verifyR2 (_,_,k,l') = TG.null (codomain k) `shouldBe` True

verifyR3 (_,_,k,l') =
  do
    length (nodeIdsFromCodomain k) `shouldBe` 2
    length (edgeIdsFromCodomain k) `shouldBe` 4
