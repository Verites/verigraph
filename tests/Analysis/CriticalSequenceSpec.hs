module Analysis.CriticalSequenceSpec where

import           Data.Matrix                  hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.CriticalSequence
import qualified Category.TypedGraph          as TGraph
import qualified Category.TypedGraphRule      as TGRule
import qualified Data.Graphs                  as Graph
import           Rewriting.DPO.TypedGraphRule (SndOrderRule)
import qualified XML.GGXReader                as XML

fileName1 = "tests/grammars/teseRodrigo.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
tGraphConfig = TGraph.Config Graph.empty TGraph.AllMatches
tGRuleConfig = TGRule.Config tGraphConfig TGRule.AllMatches

spec :: Spec
spec = context "Critical Sequences Test" csaTest

csaTest :: Spec
csaTest = do
    it "first-order" $ do
      (gg1,_,_) <- XML.readGrammar fileName1 False tGRuleConfig
      let fstRules = map snd (productions gg1)

      testTeseRodrigoDependencies fstRules

    it "second-order" $ do
      (_,gg2,_) <- XML.readGrammar fileName2 False tGRuleConfig
      let sndRules = map snd (productions gg2)

      testSndOrderDependencies sndRules

testTeseRodrigoDependencies rules =
  do
    testCase findAllProduceUse rules $
        "( 0 1 0 0 0 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 2 0 0 0 )\n"
    testCase findAllDeleteForbid rules $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    testCase findAllRemoveDangling rules $
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    testCase findAllDeliverDelete rules $
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"
    testCase findAllDeliverDangling rules $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 0 0 0 0 )\n"
    testCase findAllForbidProduce rules $
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 1 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    testCase findAllProduceUseAndRemoveDangling rules $
        "( 0 1 0 0 1 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 2 0 0 0 )\n"
    testCase findAllDeliverDeleteAndDeliverDangling rules $
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 1 0 0 0 )\n"
  where
    testCase findDependencies rules expected = show (length <$> pairwise findDependencies rules) `shouldBe` expected
    pairwise f items = TGraph.runCat tGraphConfig . mapM (uncurry f) .
      matrix (length items) (length items) $ \(i,j) ->
        (items !! (i-1), items !! (j-1))

testSndOrderDependencies :: [SndOrderRule n e] -> IO ()
testSndOrderDependencies rules =
  do
    testCase findAllProduceUse rules $
        "( 0 0 1 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllDeleteForbid rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllRemoveDangling rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllDeliverDelete rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllDeliverDangling rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllForbidProduce rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllProduceUseAndRemoveDangling rules $
        "( 0 0 1 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    testCase findAllDeliverDeleteAndDeliverDangling rules $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
  where
    testCase findDependencies rules expected = show (length <$> pairwise findDependencies rules) `shouldBe` expected
    pairwise f items = TGRule.runCat tGRuleConfig . mapM (uncurry f) .
      matrix (length items) (length items) $ \(i,j) ->
        (items !! (i-1), items !! (j-1))
