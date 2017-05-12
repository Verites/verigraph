import           Abstract.AdhesiveHLR      (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Data.Matrix               hiding ((<|>))
import           Test.HUnit
import           Utils
import qualified XML.GGXReader             as XML

main :: IO ()
main = do
  let fileName1 = "tests/grammars/teseRodrigo.ggx"
      fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
      dpoConf = MorphismsConfig MonoMatches PartiallyMonomorphicNAC
  (gg1,_,_) <- XML.readGrammar fileName1 False dpoConf
  (_,gg2,_) <- XML.readGrammar fileName2 False dpoConf

  let fstRules = map snd (rules gg1)
      sndRules = map snd (rules gg2)

  runTests $
    [ testTeseRodrigo (MorphismsConfig MonoMatches PartiallyMonomorphicNAC) fstRules
    , testTeseRodrigo (MorphismsConfig MonoMatches MonomorphicNAC) fstRules
    , testTeseRodrigo (MorphismsConfig AnyMatches PartiallyMonomorphicNAC) fstRules
    , testTeseRodrigo (MorphismsConfig AnyMatches MonomorphicNAC) fstRules
    ] ++
    [testSndOrder (MorphismsConfig AnyMatches MonomorphicNAC) sndRules]

testTeseRodrigo dpoConf rules =
  "tese rodrigo" ~:
    [ testTeseRodrigoConflicts dpoConf rules
    , testTeseRodrigoDeps dpoConf rules
    ]

testSndOrder dpoConf rules =
  "second order" ~:
    [ testSndOrderConflicts dpoConf rules
    , testSndOrderDeps dpoConf rules
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

testSndOrderConflicts dpoConf rules =
  "conflicts" ~:
    [ testCase "Delete-Use" findAllDeleteUse $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 5 0 0 )\n"++
        "( 0 0 0 3 0 )\n"++
        "( 0 0 0 0 3 )\n"
    , testCase "Produce-Dangling" findAllProduceDangling $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Delete-Use and Produce-Dangling" findAllDeleteUseAndProduceDangling $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 5 0 0 )\n"++
        "( 0 0 0 3 0 )\n"++
        "( 0 0 0 0 3 )\n"
    , testCase "Produce-Forbid" findAllProduceForbid $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
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

testSndOrderDeps dpoConf rules =
  "dependencies" ~:
    [ testCase "Produce-Use" findAllProduceUse $
        "( 0 0 1 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Delete-Forbid" findAllDeleteForbid $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Remove-Dangling" findAllRemoveDangling $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Deliver-Delete" findAllDeliverDelete $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Deliver-Dangling" findAllDeliverDangling $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Forbid-Produce" findAllForbidProduce $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    , testCase "Produce-Use and Remove-Dangling" findAllProduceUseAndRemoveDangling $
        "( 0 0 1 0 0 )\n"++
        "( 0 0 1 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"
    ]
  where
  testCase name findDependencies expected =
    "Test " ++ name ++ printConf dpoConf ~:
      [expected ~=? show (pairwise (findDependencies dpoConf) rules)]


pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
