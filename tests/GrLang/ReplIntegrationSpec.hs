module GrLang.ReplIntegrationSpec (spec) where

import           System.Exit    (ExitCode (..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec

spec :: Spec
spec = mapM_ testCase
  [ "graph-morphism-rule" ]

testCase :: String -> SpecWith (Arg (IO ()))
testCase name = it name $ do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode
    "stack" ["exec", "verigraph-repl", "--", prefix ++ name ++ ".lua"] ""
  exitCode `shouldBe` ExitSuccess
  expectedOut <- readFile (prefix ++ name ++ ".stdout")
  expectedErr <- readFile (prefix ++ name ++ ".stderr")
  stdOut `shouldBe` expectedOut
  stdErr `shouldBe` expectedErr
  where prefix = "tests/GrLang/ReplIntegrationSpec/"
