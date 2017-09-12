module Analysis.ParallelIndependentSpec (spec) where

import           Data.Matrix                  hiding ((<|>))
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import qualified XML.GGXReader                as XML

-- This test is based on that the pullback scheme for detecting parallel
-- or sequentially independece is identical to find for delete-use.
-- Considering the simplicity of the delete-use and assuming it is correct,
-- we compare it with pullback scheme that runs two calculatePullback calls for instance.

filenames = ["tests/grammars/nacs2rule.ggx"
            ,"tests/grammars/teseRodrigo.ggx"
            ,"tests/grammars/secondOrderMatchTest.ggx"
            ,"tests/grammars/elevator.ggx"
            ,"tests/grammars/mutex.ggx"]
dpoConf = MorphismsConfig anyMorphism

spec :: Spec
spec = context "Parallel Independence Test" parIndepTest

parIndepTest :: Spec
parIndepTest =
  describe "Check Parallel/Sequentially independent rules for pullbacks against delete-use" $ do
    it "Parallel Test for Cond1" $
      mapM_ (\fn -> test1 dpoConf fn Parallel Cond1) filenames

    it "Parallel Test for Cond2" $
      mapM_ (\fn -> test1 dpoConf fn Parallel Cond2) filenames

    it "Sequentially Test for Cond1" $
      mapM_ (\fn -> test1 dpoConf fn Sequentially Cond1) filenames

    it "Sequentially Test for Cond2" $
      mapM_ (\fn -> test1 dpoConf fn Sequentially Cond2) filenames

test1 dpoConf fileName alg pb =
  do
    (gg,_,_) <- XML.readGrammar fileName False dpoConf

    let rules = map snd (productions gg)
        analysisPB = pairwiseCompare (isIndependent alg pb dpoConf) rules
        analysisDU = pairwiseCompare (isIndependent alg Cond3 dpoConf) rules

    analysisDU `shouldBe` analysisPB

pairwiseCompare :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) -> compare (items !! (i-1)) (items !! (j-1))
