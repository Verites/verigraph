module GraphGrammar.Core (
      grammar
    , Grammar
    , start
    , constraints
    , rules
) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO

data (AdhesiveHLR m) => Grammar m =
  Grammar {
      start       :: Obj m
    , constraints :: [Constraint m]
    , rules       :: [(String, Production m)]
    }

grammar :: (AdhesiveHLR m) => Obj m -> [Constraint m] -> [(String, Production m)] -> Grammar m
grammar = Grammar
