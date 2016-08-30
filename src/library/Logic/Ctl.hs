{-|
Description : Syntax and model checking for CTL.

Maintainer  : Guilherme G. Azzi <ggazzi@inf.ufrgs.br>
Stability   : provisional
-}
module Logic.Ctl
  (
  -- * CTL Expressions
    module Logic.Ctl.Base
  , parseExpr

  -- * Model checking
  , check
  , module Logic.Ctl.Semantics
  ) where


import           Logic.Ctl.Base
import           Logic.Ctl.Parser
import           Logic.Ctl.Semantics
import           Logic.Model


-- | Check if the given expression holds in the given state of the Kripke structure.
check :: KripkeStructure String -> Expr -> Int -> Bool
check model expr s0 = s0 `elem` satisfyExpr' model expr
