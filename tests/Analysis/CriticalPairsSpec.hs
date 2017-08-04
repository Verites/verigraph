module Analysis.CriticalPairsSpec where

import           Data.Matrix             hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import qualified Category.TypedGraph     as TGraph
import qualified Category.TypedGraphRule as TGRule
import qualified Data.Graphs             as Graph
import qualified XML.GGXReader           as XML

fileName1 = "tests/grammars/teseRodrigo.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
tGraphConfig = TGraph.Config Graph.empty TGraph.MonicMatches
tGRuleConfig = TGRule.Config tGraphConfig TGraph.MonicMatches

spec :: Spec
spec = context "Critical Pairs Test" cpaTest

cpaTest :: Spec
cpaTest = do
    it "first-order" $ do
      (gg1,_,_) <- XML.readGrammar fileName1 False tGRuleConfig
      let fstRules = map snd (productions gg1)

      testTeseRodrigoConflicts fstRules

    it "second-order" $ do
      (_,gg2,_) <- XML.readGrammar fileName2 False tGRuleConfig
      let sndRules = map snd (productions gg2)

      testSndOrderConflicts sndRules

testTeseRodrigoConflicts rules =
  do
    testCase findAllDeleteUse rules $
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 2 6 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 1 0 4 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"

    testCase findAllProduceDangling rules $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"

    testCase findAllDeleteUseAndProduceDangling rules $
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 2 6 0 1 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 1 0 4 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"

    testCase findAllProduceForbid rules $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 1 0 0 )\n"
  where
    testCase findDependencies rules expected = show (length <$> pairwise findDependencies rules) `shouldBe` expected
    pairwise f items = TGraph.runCat tGraphConfig . mapM (uncurry f) .
      matrix (length items) (length items) $ \(i,j) ->
        (items !! (i-1), items !! (j-1))

testSndOrderConflicts rules =
  do
    testCase findAllDeleteUse rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 5 0 0 )\n"++
        "( 0 0 0 3 0 )\n"++
        "( 0 0 0 0 3 )\n"

    testCase findAllProduceDangling rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"

    testCase findAllDeleteUseAndProduceDangling rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 5 0 0 )\n"++
        "( 0 0 0 3 0 )\n"++
        "( 0 0 0 0 3 )\n"

    testCase findAllProduceForbid rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
  where
    testCase findDependencies rules expected = show (length <$> pairwise findDependencies rules) `shouldBe` expected
    pairwise f items = TGRule.runCat tGRuleConfig . mapM (uncurry f) .
      matrix (length items) (length items) $ \(i,j) ->
        (items !! (i-1), items !! (j-1))
