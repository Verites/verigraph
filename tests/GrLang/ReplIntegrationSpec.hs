module GrLang.ReplIntegrationSpec (spec) where

import           System.Directory (listDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (takeBaseName, takeExtension)
import           System.Process   (readProcessWithExitCode)
import           Test.Hspec

spec :: Spec
spec = do
  files <- runIO $ listDirectory "tests/GrLang/ReplIntegrationSpec"
  let cases = map takeBaseName . filter ((== ".lua") . takeExtension) $ files
  mapM_ testCase cases

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
