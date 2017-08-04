module Analysis.EssentialCriticalPairsSpec where

import           Data.Matrix                     hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.EssentialCriticalPairs
import qualified Category.TypedGraph             as TGraph
import qualified Category.TypedGraphRule         as TGRule
import qualified Data.Graphs                     as Graph
import qualified XML.GGXReader                   as XML

fileName1 = "tests/grammars/elevator.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
dpoConf = TGRule.Config (TGraph.Config Graph.empty TGraph.MonicMatches) TGRule.MonicMatches
testCase findEssentialCP rules expected = expected `shouldBe` show (pairwise findEssentialCP rules)

spec :: Spec
spec = context "Essential Critical Pairs Test" ecpaTest

ecpaTest :: Spec
ecpaTest = do
    it "first-order" $ do
      (gg1,_,_) <- XML.readGrammar fileName1 False dpoConf
      let fstRules = map snd (productions gg1)

      testElevatorConflicts fstRules

    it "second-order" $ do
      (_,gg2,_) <- XML.readGrammar fileName2 False dpoConf
      let sndRules = map snd (productions gg2)

      testSndOrderConflicts sndRules

testSndOrderConflicts rules =
  testCase findAllEssentialDeleteUse' rules $
    "( 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 )\n"++
    "( 0 0 2 0 0 )\n"++
    "( 0 0 0 1 0 )\n"++
    "( 0 0 0 0 1 )\n"
  where
    findAllEssentialDeleteUse' x y = TGRule.runCat dpoConf $ findAllEssentialDeleteUse x y

testElevatorConflicts rules =
  testCase findAllEssentialDeleteUse' rules $
    "( 1 1 0 0 1 0 1 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 1 1 1 0 0 )\n"++
    "( 0 0 0 0 0 1 1 0 1 )\n"++
    "( 0 0 0 0 1 0 1 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 1 1 1 0 1 )\n"
  where
    findAllEssentialDeleteUse' x y = TGraph.runCat (TGRule.fstOrderConfig dpoConf) $ findAllEssentialDeleteUse x y

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
