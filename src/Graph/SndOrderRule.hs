module Graph.SndOrderRule (
  SndOrderRule,
  sndOrderRule
  ) where

import Graph.RuleMorphism

data SndOrderRule a b =
  SndOrderRule {
    left  :: RuleMorphism a b
  , right :: RuleMorphism a b
  }

sndOrderRule :: RuleMorphism a b -> RuleMorphism a b -> SndOrderRule a b
sndOrderRule = SndOrderRule
