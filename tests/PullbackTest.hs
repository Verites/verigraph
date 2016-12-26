import           Analysis.ParallelIndependent
import           Abstract.DPO
import           Data.Matrix                     hiding ((<|>))
import qualified Grammar.Core               as GG
import           Test.HUnit
import qualified XML.GGXReader                   as XML
import           Utils

-- This test is based on that the pullback scheme for detecting parallel
-- or sequentially independece is identical to find for delete-use.
-- Considering the simplicity of the delete-use and assuming it is correct,
-- we compare it with pullback scheme that runs two calculatePullback calls for instance.

main :: IO Counts
main =
  do
    let filenames =
          ["tests/grammars/nacs2rule.ggx"
          ,"tests/grammars/teseRodrigo.ggx"
          ,"tests/grammars/secondOrderMatchTest.ggx"
          ,"tests/grammars/elevator.ggx"
          ,"tests/grammars/mutex.ggx"]

        --dpoConf = MorphismsConfig AnyMatches MonomorphicNAC
        dpoConf = MorphismsConfig MonoMatches MonomorphicNAC

        tests fn =
            [TestLabel ("Parallel Test for " ++ fn) (test1 dpoConf fn Parallel)
            ,TestLabel ("Sequentially Test for " ++ fn) (test1 dpoConf fn Sequentially)]

        allTests = TestList $ concatMap tests filenames

    runTestTT allTests

test1 dpoConf fileName alg =
  TestCase $
    do
      (gg,_,_) <- XML.readGrammar fileName False dpoConf

      let rules = map snd (GG.rules gg)
          analysisDU = pairwiseCompare (isIndependent alg DeleteUse dpoConf) rules
          analysisPB = pairwiseCompare (isIndependent alg Pullback dpoConf) rules

      assertEqual (show alg ++ "Independent on " ++ fileName) analysisDU analysisPB

pairwiseCompare :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) -> compare (items !! (i-1)) (items !! (j-1))
