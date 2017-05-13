import           Data.Matrix                  hiding ((<|>))
import           Test.HUnit

import           Abstract.DPO
import           Analysis.ParallelIndependent
import qualified XML.GGXReader                as XML

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

        dpoConf = MorphismsConfig AnyMatches MonomorphicNAC

        tests fn =
            [ TestLabel ("Parallel Test for " ++ fn ++ " with Cond1") (test1 dpoConf fn Parallel Cond1)
            , TestLabel ("Parallel Test for " ++ fn ++ " with Cond2") (test1 dpoConf fn Parallel Cond2)
            , TestLabel ("Sequentially Test for " ++ fn ++ " with Cond1") (test1 dpoConf fn Sequentially Cond1)
            , TestLabel ("Sequentially Test for " ++ fn ++ " with Cond2") (test1 dpoConf fn Sequentially Cond2)]

        allTests = TestList $ concatMap tests filenames

    runTestTT allTests

test1 dpoConf fileName alg pb =
  TestCase $
    do
      (gg,_,_) <- XML.readGrammar fileName False dpoConf

      let rules = map snd (productions gg)
          analysisPB = pairwiseCompare (isIndependent alg pb dpoConf) rules
          analysisDU = pairwiseCompare (isIndependent alg Cond3 dpoConf) rules

      assertEqual (show alg ++ "Independent on " ++ fileName) analysisDU analysisPB

pairwiseCompare :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) -> compare (items !! (i-1)) (items !! (j-1))
