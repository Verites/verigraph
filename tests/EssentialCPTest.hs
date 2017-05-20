import           Abstract.Category.AdhesiveHLR            (EpiPairs)
import           Abstract.Category.DPO
import           Analysis.EssentialCriticalPairs
import           Data.Matrix                     hiding ((<|>))
import           Test.HUnit
import           Utils
import qualified XML.GGXReader                   as XML

main :: IO ()
main = do
  let fileName1 = "tests/grammars/elevator.ggx"
      fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
      dpoConf = MorphismsConfig MonoMatches PartiallyMonomorphicNAC
  (gg1,_,_) <- XML.readGrammar fileName1 False dpoConf
  (_,gg2,_) <- XML.readGrammar fileName2 False dpoConf

  let rules1 = map snd (productions gg1)
      rules2 = map snd (productions gg2)

  runTests
    [ testElevator (MorphismsConfig MonoMatches PartiallyMonomorphicNAC) rules1
    , testSndOrder (MorphismsConfig MonoMatches PartiallyMonomorphicNAC) rules2
    ]

testElevator dpoConf rules =
  "teste elevator" ~:
    [ testElevatorConflicts dpoConf rules ]

testSndOrder dpoConf rules =
  "teste snd-order" ~:
    [ testSndOrderConflicts dpoConf rules ]

printConf dpoConf =
  show (matchRestriction dpoConf) ++
  " " ++
  show (nacSatisfaction dpoConf)

testSndOrderConflicts dpoConf rules =
  "conflicts" ~:
    [ testCase "Essential Second-Order Delete-Use" findAllEssentialDeleteUse $
        "( 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 )\n"++
        "( 0 0 2 0 0 )\n"++
        "( 0 0 0 1 0 )\n"++
        "( 0 0 0 0 1 )\n"
    ]
  where
    testCase name findConflicts expected =
      "Test " ++ name ++ printConf dpoConf ~:
        [expected ~=? show (pairwise (findConflicts dpoConf) rules)]

testElevatorConflicts dpoConf rules =
  "conflicts" ~:
    [ testCase "Essential Delete-Use" findAllEssentialDeleteUse $
        "( 1 1 0 0 1 0 1 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 1 1 0 0 )\n"++
        "( 0 0 0 0 0 1 1 0 1 )\n"++
        "( 0 0 0 0 1 0 1 0 0 )\n"++
        "( 0 0 0 0 0 0 0 0 0 )\n"++
        "( 0 0 0 0 1 1 1 0 4 )\n"
    ]
  where
    testCase name findConflicts expected =
      "Test " ++ name ++ printConf dpoConf ~:
        [expected ~=? show (pairwise (findConflicts dpoConf) rules)]

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
