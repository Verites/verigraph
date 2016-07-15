module Main where

import Test.Framework (defaultMain, testGroup)

import CtlTest.TestParser (parserTests)

main :: IO ()
main =
  defaultMain [ testGroup "Parser" parserTests ]
