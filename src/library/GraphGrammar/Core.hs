{-|
Description : Grammar structure.

Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
Stability   : experimental

A grammar is defined as a start object, a set of transformation rules,
and a set of constraints for the potencial generated objects.
The morphism type is kept generic.
-}

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
