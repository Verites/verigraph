module Utils where

import           Control.Monad
import           System.Exit
import           Test.HUnit

runTests :: Testable t => t -> IO ()
runTests testable =
  do
    counts <- runTestTT (test testable)
    when (failures counts > 0) exitFailure
