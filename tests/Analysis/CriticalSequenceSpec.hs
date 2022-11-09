module Analysis.CriticalSequenceSpec (spec) where

import           Data.Matrix               hiding ((<|>))
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.CriticalSequence
import           Category.TypedGraphRule   ()
import qualified XML.GGXReader             as XML

fileName1 = "tests/grammars/teseRodrigo.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf :: Category morph => MorphismsConfig morph
dpoConf = MorphismsConfig anyMorphism
testCase findDependencies rules expected = show (pairwise (findDependencies dpoConf []) rules) `shouldBe` expected

spec :: Spec
spec = context "Critical Sequences Test" csaTest

csaTest :: Spec
csaTest = do
    it "first-order" $ do
      (gg1,_,_) <- XML.readGrammar fileName1 False dpoConf
      let fstRules = map snd (productions gg1)

      testTeseRodrigoDependencies fstRules

    it "second-order" $ do
      (_,gg2,_) <- XML.readGrammar fileName2 False dpoConf
      let sndRules = map snd (productions gg2)

      testSndOrderDependencies sndRules

testTeseRodrigoDependencies rules =
  do
    testCase findAllProduceUse rules $
        "┌                 ┐\n\
        \│ 0 1 0 0 0 0 0 0 │\n\
        \│ 0 0 3 0 0 0 0 0 │\n\
        \│ 0 0 0 1 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 1 0 2 0 0 0 │\n\
        \└                 ┘"
    testCase findAllDeleteForbid rules $
        "┌                 ┐\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \└                 ┘"
    testCase findAllRemoveDangling rules $
        "┌                 ┐\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \└                 ┘"
    testCase findAllDeliverDelete rules $
        "┌                 ┐\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \│ 0 0 3 0 0 0 0 0 │\n\
        \│ 0 0 0 1 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \└                 ┘"
    testCase findAllDeliverDangling rules $
        "┌                 ┐\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 1 0 0 0 0 0 │\n\
        \└                 ┘"
    testCase findAllForbidProduce rules $
        "┌                 ┐\n\
        \│ 0 3 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 1 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \└                 ┘"
    testCase findAllProduceUseAndRemoveDangling rules $
        "┌                 ┐\n\
        \│ 0 1 0 0 1 0 0 0 │\n\
        \│ 0 0 3 0 0 0 0 0 │\n\
        \│ 0 0 0 1 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 1 0 2 0 0 0 │\n\
        \└                 ┘"
    testCase findAllDeliverDeleteAndDeliverDangling rules $
        "┌                 ┐\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \│ 0 0 3 0 0 0 0 0 │\n\
        \│ 0 0 0 1 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 1 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 0 0 0 │\n\
        \│ 0 0 1 0 1 0 0 0 │\n\
        \└                 ┘"

testSndOrderDependencies rules =
  do
    testCase findAllProduceUse rules $
        "┌           ┐\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllDeleteForbid rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllRemoveDangling rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllDeliverDelete rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllDeliverDangling rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllForbidProduce rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllProduceUseAndRemoveDangling rules $
        "┌           ┐\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"
    testCase findAllDeliverDeleteAndDeliverDangling rules $
        "┌           ┐\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 1 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \│ 0 0 0 0 0 │\n\
        \└           ┘"

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
