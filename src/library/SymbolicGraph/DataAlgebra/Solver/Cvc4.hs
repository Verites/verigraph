{-# LANGUAGE OverloadedStrings #-}
module SymbolicGraph.DataAlgebra.Solver.Cvc4 (cvc4) where

import           SymbolicGraph.DataAlgebra.Solver
import           SymbolicGraph.DataAlgebra.Solver.SmtLib

import           Data.Text.Lazy                          (Text)
import qualified Data.Text.Lazy                          as T
import           Data.Text.Lazy.IO                       (hGetContents, hPutStr)
import           System.IO                               (hClose)
import           System.Process



cvc4 :: Solver
cvc4 =
  makeSmtLibSolver checkSat


checkSat :: Text -> IO (Maybe Bool)
checkSat problem =
  do
    (Just pipeToSolver, Just pipeFromSolver, _, _) <- createProcess cvc4Process
    hPutStr pipeToSolver problem
    hClose pipeToSolver
    result <- hGetContents pipeFromSolver
    case T.words result of
      ["sat"]   -> return (Just True)
      ["unsat"] -> return (Just False)
      _         -> return Nothing


cvc4Process :: CreateProcess
cvc4Process = (proc "cvc4" ["--lang", "smt"]) { std_in = CreatePipe, std_out = CreatePipe }
