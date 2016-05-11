module Graph.SndOrderRule{- (
  SndOrderRule,
  sndOrderRule
  ) -}where

import Abstract.Valid
import Graph.RuleMorphism

data SndOrderRule a b =
  SndOrderRule {
    left  :: RuleMorphism a b
  , right :: RuleMorphism a b
  , nacs  :: [RuleMorphism a b]
  } deriving (Eq, Show)

sndOrderRule :: RuleMorphism a b -> RuleMorphism a b -> [RuleMorphism a b] -> SndOrderRule a b
sndOrderRule = SndOrderRule

instance Valid (SndOrderRule a b) where
    valid (SndOrderRule l r nacs) =
      -- fix needs Eq GraphRule
      --domain l == domain r &&
      valid l &&
      valid r &&
      all valid nacs
