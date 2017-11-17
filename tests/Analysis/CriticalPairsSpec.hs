module Analysis.CriticalPairsSpec (spec) where

import           Data.Matrix             hiding ((<|>))
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Category.TypedGraphRule ()
import qualified XML.GGXReader           as XML

fileName1 = "tests/grammars/teseRodrigo.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf :: Category morph => MorphismsConfig morph
dpoConf = MorphismsConfig monic
testCase findConflicts rules expected = show (pairwise (findConflicts dpoConf []) rules) `shouldBe` expected

spec :: Spec
spec = context "Critical Pairs Test" cpaTest

cpaTest :: Spec
cpaTest = do
    describe "first-order" $ do
      (gg1,_,_) <- runIO $ XML.readGrammar fileName1 False dpoConf
      let rules = map snd (productions gg1)

      it "delete-use" $
        testCase findAllDeleteUse rules $
          "( 2 0 0 0 0 0 0 0 )\n"++
          "( 0 3 0 0 0 0 0 0 )\n"++
          "( 0 2 6 0 0 0 0 0 )\n"++
          "( 0 0 0 1 0 0 0 0 )\n"++
          "( 0 0 1 0 4 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"

      it "produce-dangling" $
        testCase findAllProduceDangling rules $
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 1 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"

      it "delete-use and produce-dangling" $
        testCase findAllDeleteUseAndProduceDangling rules $
          "( 2 0 0 0 0 0 0 0 )\n"++
          "( 0 3 0 0 0 0 0 0 )\n"++
          "( 0 2 6 0 1 0 0 0 )\n"++
          "( 0 0 0 1 0 0 0 0 )\n"++
          "( 0 0 1 0 4 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"

      it "produce-forbid" $
        testCase findAllProduceForbid rules $
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 2 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 0 0 0 )\n"++
          "( 0 0 0 0 0 1 0 0 )\n"

    describe "second-order" $ do
      (_,gg2,_) <- runIO $ XML.readGrammar fileName2 False dpoConf
      let rules = map snd (productions gg2)

      it "delete-use" $
        testCase findAllDeleteUse rules $
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 5 0 0 )\n"++
            "( 0 0 0 3 0 )\n"++
            "( 0 0 0 0 3 )\n"

      it "produce-dangling" $
        testCase findAllProduceDangling rules $
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"

      it "delete-use and produce-dangling" $
        testCase findAllDeleteUseAndProduceDangling rules $
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 5 0 0 )\n"++
            "( 0 0 0 3 0 )\n"++
            "( 0 0 0 0 3 )\n"

      it "produce-forbid" $
        testCase findAllProduceForbid rules $
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"++
            "( 0 0 0 0 0 )\n"

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
