module Analysis.ParallelIndependentSpec where

import           Data.Matrix                  hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import qualified Category.TypedGraph          as TGraph
import qualified Category.TypedGraphRule      as TGRule
import qualified Data.Graphs                  as Graph
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
tGraphConf = TGraph.Config Graph.empty TGraph.AllMatches
tGRuleConf = TGRule.Config tGraphConf TGRule.AllMatches

spec :: Spec
spec = context "Parallel Independence Test" parIndepTest

parIndepTest :: Spec
parIndepTest =
  describe "Check Parallel/Sequentially independent rules for pullbacks against delete-use" $ do
    it "Parallel Test for Cond1" $
      mapM_ (\fn -> test1 tGraphConf fn Parallel Cond1) filenames

    it "Parallel Test for Cond2" $
      mapM_ (\fn -> test1 tGraphConf fn Parallel Cond2) filenames

    it "Sequentially Test for Cond1" $
      mapM_ (\fn -> test1 tGraphConf fn Sequentially Cond1) filenames

    it "Sequentially Test for Cond2" $
      mapM_ (\fn -> test1 tGraphConf fn Sequentially Cond2) filenames

test1 tGraphConf fileName alg pb =
  do
    (gg,_,_) <- XML.readGrammar fileName False tGRuleConf

    let rules = map snd (productions gg)
        analysisPB = pairwiseCompare tGraphConf (isIndependent alg pb) rules
        analysisDU = pairwiseCompare tGraphConf (isIndependent alg Cond3) rules

    analysisDU `shouldBe` analysisPB

pairwiseCompare :: TGraph.Config n e -> (a -> a -> TGraph.CatM n e Bool) -> [a] -> Matrix Bool
pairwiseCompare conf compare items = TGraph.runCat conf . mapM (uncurry compare) .
  matrix (length items) (length items) $ \(i,j) -> (items !! (i-1), items !! (j-1))
