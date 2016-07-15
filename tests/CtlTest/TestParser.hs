module CtlTest.TestParser
  ( parserTests
  ) where


import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit as HUnit
import Text.PrettyPrint.Leijen

import Logic.Ctl
import CtlTest.Utils ()


parserTests :: [Test]
parserTests =
  [ testGroup "atomic expressions" $ parseTest
      [ ("true", Literal True)
      , ("false", Literal False)
      , ("foo_bar1'", Atom "foo_bar1'")
      , ("(foo)", Atom "foo")
      , ("((true))", Literal True)
      ]
      []

  , testGroup "unary expressions" $ parseTest
    [ ("~foo", Not$ Atom "foo")
    , ("AX foo", Temporal$A$X$ Atom "foo")
    , ("AF foo", Temporal$A$F$ Atom "foo")
    , ("AG foo", Temporal$A$G$ Atom "foo")
    , ("EX foo", Temporal$E$X$ Atom "foo")
    , ("EF foo", Temporal$E$F$ Atom "foo")
    , ("EG foo", Temporal$E$G$ Atom "foo")
    , ("AF AG foo", Temporal$A$F$ Temporal$A$G$ Atom "foo")
    , ("~AX~foo", Not$ Temporal$A$X$ Not$ Atom "foo")
    , ("(AF (AG foo))", Temporal$A$F$ Temporal$A$G$ Atom "foo")
    , ("~((AX~foo))", Not$ Temporal$A$X$ Not$ Atom "foo")
    ]
    []

  , testGroup "binary boolean expressions" $ parseTest
    [ ("foo && bar && baz", Atom "foo" `And` (Atom "bar" `And` Atom "baz"))
    , ("foo || bar || baz", Atom "foo" `Or` (Atom "bar" `Or` Atom "baz"))
    , ("(foo || foo) && (foo || foo)", And (Atom "foo" `Or` Atom "foo")
                                           (Atom "foo" `Or` Atom "foo"))
    , ("(foo && foo) || (foo && foo)", Or (Atom "foo" `And` Atom "foo")
                                           (Atom "foo" `And` Atom "foo"))
    , ("AX( ~foo && AF foo )", Temporal$A$X$ (Not$ Atom "foo") `And` (Temporal$A$F$ Atom "foo"))
    ]
    [ "foo && bar || baz"
    , "foo || bar && baz"
    ]

  , testGroup "implicative expressions" $ parseTest
    [ ("foo <-> bar", Atom "foo" `Equiv` Atom "bar")
    , ("foo -> bar -> baz", Atom "foo" `Implies` (Atom "bar" `Implies` Atom "baz"))
    , ("(foo -> bar) <-> ~foo || bar", Equiv (Atom "foo" `Implies` Atom "bar")
                                               ((Not$ Atom "foo") `Or` Atom "bar"))
    , ("(foo <-> bar) -> ~foo || bar", Implies (Atom "foo" `Equiv` Atom "bar")
                                               ((Not$ Atom "foo") `Or` Atom "bar"))
    , ("(foo -> bar) -> baz", (Atom "foo" `Implies` Atom "bar") `Implies` Atom "baz")
    , ("~(foo <-> bar)", Not$ Atom "foo" `Equiv` Atom "bar")
    , ("AF((foo -> bar))", Temporal$A$F$ Atom "foo" `Implies` Atom "bar")
    ]
    [ "foo <-> bar <-> baz"
    , "foo <-> bar -> baz"
    , "foo -> bar <-> baz"
    ]

  , testGroup "binary temporal expressions" $ parseTest
    [ ("A[foo U bar]", Temporal$A$ Atom "foo" `U` Atom "bar")
    , ("E[foo U bar]", Temporal$E$ Atom "foo" `U` Atom "bar")
    , ("E[A[foo U bar] U E[foo U bar]]", Temporal$E$U (Temporal$A$ Atom "foo" `U` Atom "bar")
                                                      (Temporal$E$ Atom "foo" `U` Atom "bar"))
    , ("A[foo <-> bar U foo -> bar]", Temporal$A$U (Atom "foo" `Equiv` Atom "bar")
                                                   (Atom "foo" `Implies` Atom "bar"))
    , ("EF A[foo U bar]", Temporal$E$F$ Temporal$A$ Atom "foo" `U` Atom "bar")
    ]
    []

  , testProperty "parse after pretty printing" prop_parseAfterPrettyPrint
  ]
  where
    parseTest ps ns =
      map positiveTest ps ++ map negativeTest ns

    positiveTest (text, expr) =
      testCase text . HUnit.assert $ assertParse expr text

    negativeTest text =
      testCase text . HUnit.assert $ assertNoParse text


prop_parseAfterPrettyPrint :: Expr -> Bool
prop_parseAfterPrettyPrint expr =
  let
    prettyPrint expr =
      let
        simpleDoc = renderPretty 0.7 100 (pretty expr)
      in
        displayS simpleDoc ""
  in
  parseExpr "" (prettyPrint expr) == Right expr


assertParse :: Expr -> String -> HUnit.Assertion
assertParse expected text =
  case parseExpr "" text of
    Right actual ->
      HUnit.assertEqual ("When parsing '" ++ text ++ "'") expected actual

    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)


assertNoParse :: String -> HUnit.Assertion
assertNoParse text =
  case parseExpr "" text of
    Right expr ->
      HUnit.assertFailure ("Expected '" ++ text ++ "' not to parse, but parsed to: \n" ++ show expr)

    Left  _ ->
      HUnit.assertBool "" True
