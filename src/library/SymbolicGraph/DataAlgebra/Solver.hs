module SymbolicGraph.DataAlgebra.Solver (Solver(..)) where

import           SymbolicGraph.DataAlgebra



data Solver =
  Solver
    { checkSatisfability :: [Restriction] -> IO (Maybe Bool)
    , checkStrengthening :: [Restriction] -> [Restriction] -> IO (Maybe Bool)
    }
