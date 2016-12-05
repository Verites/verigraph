module GraphGrammar.Core (
      grammar
    , Grammar
    , start
    , constraints
    , rules
) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO

data Grammar m =
  Grammar {
      start       :: Obj m
    , constraints :: [Constraint m]
    , rules       :: [(String, Production m)]
    }

grammar :: Obj m -> [Constraint m] -> [(String, Production m)] -> Grammar m
grammar = Grammar
