module Analysis.EssentialCriticalPairsSpec (spec) where

import           Data.Matrix                     hiding ((<|>))
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.EssentialCriticalPairs
import           Category.TypedGraphRule         ()
import qualified XML.GGXReader                   as XML

fileName1 = "tests/grammars/elevator.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf :: Category morph => MorphismsConfig morph
dpoConf = MorphismsConfig monic
testCase findEssentialCP rules expected = expected `shouldBe` show (pairwise (findEssentialCP dpoConf) rules)

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
  testCase findAllEssentialDeleteUse rules $
    "( 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 )\n"++
    "( 0 0 2 0 0 )\n"++
    "( 0 0 0 1 0 )\n"++
    "( 0 0 0 0 1 )\n"

testElevatorConflicts rules =
  testCase findAllEssentialDeleteUse rules $
    "( 1 1 0 0 1 0 1 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 1 1 1 0 0 )\n"++
    "( 0 0 0 0 0 1 1 0 1 )\n"++
    "( 0 0 0 0 1 0 1 0 0 )\n"++
    "( 0 0 0 0 0 0 0 0 0 )\n"++
    "( 0 0 0 0 1 1 1 0 1 )\n"

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
