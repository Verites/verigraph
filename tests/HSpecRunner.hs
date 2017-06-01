-- file test/Main.hs
module Main where

import qualified HSpecTests
import           Test.Hspec

main :: IO ()
main = hspec (parallel HSpecTests.spec)
