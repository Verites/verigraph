module Main where

import           Test.Framework        (defaultMain, testGroup)

import qualified CtlTest.TestParser    as Parser
import qualified CtlTest.TestSemantics as Semantics


main :: IO ()
main =
  defaultMain
    [ testGroup "Parser" Parser.tests
    , testGroup "Semantics" Semantics.tests
    ]
