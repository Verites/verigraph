module CtlTest.TestSemantics
  ( tests
  ) where


import qualified Data.Char                      as Char
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import qualified Data.List                      as List
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (State, Test)

import           Logic.Ctl
import           Logic.Model


tests :: [Test]
tests =
  [ testGroup "boolean expressions" testBooleanExpressions
  , testGroup "{E|A}X" testTemporalNext
  , testGroup "{E|A}F" testTemporalFuture
  , testGroup "{E|A}G" testTemporalGlobally
  , testGroup "{E|A}U" testTemporalUntil
  ]


testBooleanExpressions :: [Test]
testBooleanExpressions =
  let
    model =
      assembleModel
        [ ["1 []"]
        , ["2 [p]"]
        , ["3 [q]"]
        , ["4 [p q]"]
        ]
  in
    [ testModelChecker "true" model [1, 2, 3, 4]
    , testModelChecker "false" model []
    , testModelChecker "p" model [2, 4]
    , testModelChecker "q" model [3, 4]
    , testModelChecker "~p" model [1, 3]
    , testModelChecker "p && q" model [4]
    , testModelChecker "p || q" model [2, 3, 4]
    , testModelChecker "p -> q" model [1, 3, 4]
    , testModelChecker "p <-> q" model [1, 4]
    , testModelChecker "~(p <-> q)" model [2, 3]
    ]


testTemporalNext :: [Test]
testTemporalNext =
  let
    model =
      assembleModel
        [ ["1 []", "2 []", "3 [p]", "4 [p]"]
        , ["3 [p]", "2 []"]
        ]
  in
    [ testModelChecker "EX p" model [2, 3]
    , testModelChecker "AX p" model [2, 4]
    , testModelChecker "~(EX p || AX p)" model [1]
    ]


testTemporalFuture :: [Test]
testTemporalFuture =
  let
    model =
      assembleModel
        [ ["1 []", "2 []", "3 []", "4 [p]", "5 []"]
        , ["1 []", "1 []"]
        ]
  in
    [ testModelChecker "EF p" model [1, 2, 3, 4]
    , testModelChecker "AF p" model [2, 3, 4]
    , testModelChecker "~(EF p || AF p)" model [5]
    ]


testTemporalGlobally :: [Test]
testTemporalGlobally =
  let
    model =
      assembleModel
        [ ["1 []", "2 [p]", "3 [p]", "4 [p]"]
        , ["2 []", "2 []", "5 []"]
        , ["3 [p]", "3 [p]"]
        ]
  in
    [ testModelChecker "EG p" model [2, 3, 4]
    , testModelChecker "AG p" model [3, 4]
    , testModelChecker "~(EG p || AG p)" model [1, 5]
    ]


testTemporalUntil :: [Test]
testTemporalUntil =
  let
    model =
      assembleModel
        [ ["1 []", "2 [p]", "3 [p]", "4 [q]", "5 [p]", "5 [p]"]
        , ["5 [p]", "3 [p]"]
        , ["3 [p]", "6 [p q]", "1 []"]
        ]
  in
    [ testModelChecker "E[p U q]" model [2, 3, 4, 5, 6]
    , testModelChecker "A[p U q]" model [2, 3, 4, 6]
    , testModelChecker "~(E[p U q] || A[p U q])" model [1]
    ]


testModelChecker :: String -> KripkeStructure String -> [Int] -> Test
testModelChecker expr model expected =
  testCase expr $ statesThatSatisfy expr model @?= List.sort expected


statesThatSatisfy :: String -> KripkeStructure String -> [Int]
statesThatSatisfy exprText model =
  case parseExpr "" exprText of
    Left err ->
      error ("Error parsing '"++ exprText ++"':\n"++ show err)

    Right expr ->
      List.sort (satisfyExpr' model expr)


assembleModel :: [[String]] -> KripkeStructure String
assembleModel description =
  let
    (states, transitions') =
      parseLines description (IntMap.empty, Set.empty)

    (transitions, _) =
      Set.foldr addTransition ([], 0) transitions'

    addTransition (src, tgt) (ts, uid) =
      (Transition uid src tgt [] : ts, uid + 1)
  in
    KripkeStructure (IntMap.elems states) transitions


type PartialModel =
  (IntMap (State String), Set (Int, Int))


parseLines :: [[String]] -> PartialModel -> PartialModel
parseLines lines model =
  foldr parseLine model lines


parseLine :: [String] -> PartialModel -> PartialModel
parseLine stateTexts model =
  addStates (map parseState stateTexts) model


addStates :: [(Int, [String])] -> PartialModel -> PartialModel
addStates [] model =
  model

addStates [n1] (states, transitions) =
  (addState n1 states, transitions)

addStates (n1:n2:rest) (states, transitions) =
  let
    states' =
      addState n1 states

    transitions' =
      Set.insert (fst n1, fst n2) transitions
  in
    addStates (n2:rest) (states', transitions')


addState :: (Int, [String]) -> IntMap (State String) -> IntMap (State String)
addState (stateId, props) states =
  let
    addProps Nothing =
      State stateId props
    addProps (Just (State _ props')) =
      State stateId (props `List.union` props')
  in
    IntMap.alter (Just . addProps) stateId states


parseState :: String -> (Int, [String])
parseState text =
  let
    (stateId, text') =
      List.break Char.isSpace text

    ('[' : atomsText) =
      List.takeWhile (/= ']') $ List.dropWhile (/= '[') text'

    atoms =
      split Char.isSpace atomsText
  in
    (read stateId, atoms)


split :: (a -> Bool) -> [a] -> [[a]]
split _ [] =
  [[]]

split prop xs =
  let
    (first, rest') =
      List.break prop xs

    rest =
      List.dropWhile prop rest'
  in
    if List.null rest then
      [first, rest]
    else
      first : split prop rest
