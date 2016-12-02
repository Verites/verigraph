import           Abstract.AdhesiveHLR      (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Data.Matrix               hiding ((<|>))
import           Test.HUnit
import qualified TypedGraph.GraphGrammar   as GG
import qualified XML.GGXReader             as XML
import           Utils

main :: IO ()
main = do
  let fileName = "tests/grammars/teseRodrigo.ggx"
      dpoConf = MorphismsConfig MonoMatches PartiallyMonomorphicNAC
  (gg,_) <- XML.readGrammar fileName False dpoConf

  let rules = map snd (GG.rules gg)

  runTests
    [ testTeseRodrigo (MorphismsConfig MonoMatches PartiallyMonomorphicNAC) rules
    , testTeseRodrigo (MorphismsConfig MonoMatches MonomorphicNAC) rules
    , testTeseRodrigo (MorphismsConfig AnyMatches PartiallyMonomorphicNAC) rules
    , testTeseRodrigo (MorphismsConfig AnyMatches MonomorphicNAC) rules
    ]

testTeseRodrigo dpoConf rules =
  "tese rodrigo" ~:
    [ testTeseRodrigoConflicts dpoConf rules
    , testTeseRodrigoDeps dpoConf rules
    ]

printConf dpoConf =
  show (matchRestriction dpoConf) ++
  " " ++
  show (nacSatisfaction dpoConf)

testTeseRodrigoConflicts dpoConf rules =
  "conflicts" ~:
    [ testCase "Delete-Use" findAllDeleteUse $
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 2 6 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 1 0 4 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Produce-Dangling" findAllProduceDangling $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Delete-Use and Produce-Dangling" findAllDeleteUseAndProduceDangling $
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 2 6 0 1 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 1 0 4 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Produce-Forbid" findAllProduceForbid $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 2 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 1 0 0 )\n"
    ]
  where
    testCase name findConflicts expected =
      "Test " ++ name ++ printConf dpoConf ~:
        [expected ~=? show (pairwise (findConflicts dpoConf) rules)]

testTeseRodrigoDeps dpoConf rules =
  "dependencies" ~:
    [ testCase "Produce-Use" findAllProduceUse $
        "( 0 1 0 0 0 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 2 0 0 0 )\n"
    , testCase "Delete-Forbid" findAllDeleteForbid $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Remove-Dangling" findAllRemoveDangling $
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Deliver-Delete" findAllDeliverDelete $
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 0 0 0 )\n"
    , testCase "Deliver-Dangling" findAllDeliverDangling $
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 0 0 0 0 )\n"
    , testCase "Forbid-Produce" findAllForbidProduce $
        "( 0 3 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 1 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"
    , testCase "Produce-Use and Remove-Dangling" findAllProduceUseAndRemoveDangling $
        "( 0 1 0 0 1 0 0 0 )\n"++
        "( 0 0 3 0 0 0 0 0 )\n"++
        "( 0 0 0 1 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 1 0 2 0 0 0 )\n"
    ]
  where
  testCase name findDependencies expected =
    "Test " ++ name ++ printConf dpoConf ~:
      [expected ~=? show (pairwise (findDependencies dpoConf) rules)]


pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
