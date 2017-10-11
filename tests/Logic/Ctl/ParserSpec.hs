module Logic.Ctl.ParserSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit
import           Test.QuickCheck
import           Data.Text.Prettyprint.Doc (Pretty(..))

import           Logic.Ctl
import           Logic.Ctl.TestUtils     ()


spec :: Spec
spec = do

  it "always parses the result of pretty printing" $
    property $ \expr -> 
      parseExpr "" (show $ pretty expr) == Right expr


  context "with atomic expressions" $ do

    it "parses 'true'" $
      "true" `shouldParseTo` Literal True

    it "parses 'false'" $
      "false" `shouldParseTo` Literal False

    it "parses 'foo_bar1'" $
      "foo_bar1'" `shouldParseTo` Atom "foo_bar1'"

    it "parses '(foo)'" $
      "(foo)" `shouldParseTo` Atom "foo"

    it "parses '((true))'" $
      "((true))" `shouldParseTo` Literal True


  context "with unary expressions" $ do

    it "parses '~foo'" $
      "~foo" `shouldParseTo` (Not$ Atom "foo")

    it "parses 'AX foo'" $
      "AX foo" `shouldParseTo` (Temporal$A$X$ Atom "foo")

    it "parses 'AF foo'" $
      "AF foo" `shouldParseTo` (Temporal$A$F$ Atom "foo")

    it "parses 'AG foo'" $
      "AG foo" `shouldParseTo` (Temporal$A$G$ Atom "foo")

    it "parses 'EX foo'" $
      "EX foo" `shouldParseTo` (Temporal$E$X$ Atom "foo")

    it "parses 'EF foo'" $
      "EF foo" `shouldParseTo` (Temporal$E$F$ Atom "foo")

    it "parses 'EG foo'" $
      "EG foo" `shouldParseTo` (Temporal$E$G$ Atom "foo")

    it "parses 'AF AG foo'" $
      "AF AG foo" `shouldParseTo` (Temporal$A$F$ Temporal$A$G$ Atom "foo")

    it "parses '~AX~foo'" $
      "~AX~foo" `shouldParseTo` (Not$ Temporal$A$X$ Not$ Atom "foo")

    it "parses '(AF (AG foo))'" $
      "(AF (AG foo))" `shouldParseTo` (Temporal$A$F$ Temporal$A$G$ Atom "foo")

    it "parses '~((AX~foo))'" $
      "~((AX~foo))" `shouldParseTo` (Not$ Temporal$A$X$ Not$ Atom "foo")


  context "with binary boolean expressions" $ do

    it "parses 'foo && bar && baz'" $
      "foo && bar && baz" `shouldParseTo` And (Atom "foo") (Atom "bar" `And` Atom "baz")

    it "parses 'foo || bar || baz'" $
      "foo || bar || baz" `shouldParseTo` Or (Atom "foo") (Atom "bar" `Or` Atom "baz")

    it "parses '(foo || foo) && (foo || foo)'" $
      "(foo || foo) && (foo || foo)"
        `shouldParseTo` And (Atom "foo" `Or` Atom "foo") (Atom "foo" `Or` Atom "foo")

    it "parses '(foo && foo) || (foo && foo)'" $
      "(foo && foo) || (foo && foo)"
        `shouldParseTo` Or (Atom "foo" `And` Atom "foo") (Atom "foo" `And` Atom "foo")

    it "parses 'AX( ~foo && AF foo )'" $
      "AX( ~foo && AF foo )"
        `shouldParseTo` (Temporal$A$X$ (Not$ Atom "foo") `And` (Temporal$A$F$ Atom "foo"))

    it "does not parse 'foo && bar || baz'" $
      assertNoParse "foo && bar || baz"

    it "does not parse 'foo || bar && baz'" $
      assertNoParse "foo || bar && baz"


  context "with implicative expressions" $ do

    it "parses 'foo <-> bar'" $
      "foo <-> bar" `shouldParseTo` (Atom "foo" `Equiv` Atom "bar")

    it "parses 'foo -> bar -> baz'" $
      "foo -> bar -> baz" `shouldParseTo` (Atom "foo" `Implies` (Atom "bar" `Implies` Atom "baz"))

    it "parses '(foo -> bar) <-> ~foo || bar'" $
      "(foo -> bar) <-> ~foo || bar"
        `shouldParseTo` Equiv (Atom "foo" `Implies` Atom "bar") ((Not$ Atom "foo") `Or` Atom "bar")

    it "parses '(foo <-> bar) -> ~foo || bar'" $
      "(foo <-> bar) -> ~foo || bar"
        `shouldParseTo` Implies (Atom "foo" `Equiv` Atom "bar") ((Not$ Atom "foo") `Or` Atom "bar")

    it "parses '(foo -> bar) -> baz'" $
      "(foo -> bar) -> baz" `shouldParseTo` ((Atom "foo" `Implies` Atom "bar") `Implies` Atom "baz")

    it "parses '~(foo <-> bar)'" $
      "~(foo <-> bar)" `shouldParseTo` (Not$ Atom "foo" `Equiv` Atom "bar")

    it "parses 'AF((foo -> bar))'" $
      "AF((foo -> bar))" `shouldParseTo` (Temporal$A$F$ Atom "foo" `Implies` Atom "bar")

    it "does not parse 'foo <-> bar <-> baz'" $
      assertNoParse "foo <-> bar <-> baz"

    it "does not parse 'foo <-> bar -> baz'" $
      assertNoParse "foo <-> bar -> baz"

    it "does not parse 'foo -> bar <-> baz'" $
      assertNoParse  "foo -> bar <-> baz"


  context "with binary temporal expressions" $ do

    it "parses 'A[foo U bar]'" $
      "A[foo U bar]" `shouldParseTo` (Temporal$A$ Atom "foo" `U` Atom "bar")

    it "parses 'E[foo U bar]'" $
      "E[foo U bar]" `shouldParseTo` (Temporal$E$ Atom "foo" `U` Atom "bar")

    it "parses 'E[A[foo U bar] U E[foo U bar]]'" $
      "E[A[foo U bar] U E[foo U bar]]"
        `shouldParseTo`
          (Temporal$E$U (Temporal$A$ Atom "foo" `U` Atom "bar")
                        (Temporal$E$ Atom "foo" `U` Atom "bar"))

    it "parses 'A[foo <-> bar U foo -> bar]'" $
      "A[foo <-> bar U foo -> bar]"
        `shouldParseTo`
          (Temporal$A$U (Atom "foo" `Equiv` Atom "bar")
                        (Atom "foo" `Implies` Atom "bar"))

    it "parses 'EF A[foo U bar]'" $
      "EF A[foo U bar]" `shouldParseTo` (Temporal$E$F$ Temporal$A$ Atom "foo" `U` Atom "bar")



shouldParseTo :: String -> Expr -> HUnit.Assertion
shouldParseTo text expected =
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
