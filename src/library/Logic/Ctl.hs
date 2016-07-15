module Logic.Ctl
  ( check
  , module Logic.Ctl.Parser
  , module Logic.Ctl.Base
  , module Logic.Ctl.Semantics
  ) where


import Logic.Ctl.Parser
import Logic.Ctl.Base
import Logic.Ctl.Semantics
import Logic.Model


check :: TransitionSystem String -> Expr -> Int -> Bool
check model expr s0 = s0 `elem` satisfyExpr' model expr
