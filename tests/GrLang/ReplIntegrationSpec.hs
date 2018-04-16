module GrLang.ReplIntegrationSpec (spec) where

import           System.Exit    (ExitCode (..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec

spec :: Spec
spec = mapM_ testCase
  [ "graph-morphism-rule", "conflict-essence" ]

testCase :: String -> SpecWith (Arg (IO ()))
testCase name = it name $ do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode
    "stack" ["exec", "verigraph-repl", "--", prefix ++ name ++ ".lua"] ""
  exitCode `shouldBe` ExitSuccess
  expectedErr <- readFile (prefix ++ name ++ ".stderr")
  stdErr `shouldBe` expectedErr
  expectedOut <- readFile (prefix ++ name ++ ".stdout")
  stdOut `shouldBe` expectedOut
  where prefix = "tests/GrLang/ReplIntegrationSpec/"
