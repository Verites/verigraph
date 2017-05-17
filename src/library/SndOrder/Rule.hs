module SndOrder.Rule (
    SndOrderRule
  , addMinimalSafetyNacs
  , applySndOrderRule
  , applySecondOrder
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           SndOrder.Rule.Core
import           SndOrder.Rule.DPO
import           TypedGraph.DPO.GraphRule

-- | Receives a function that works with a second order and a first order rule.
-- Apply this function on all possible combinations of rules.
applySecondOrder ::
     ((String, SndOrderRule a b) -> (String, GraphRule a b) -> [t])
  -> [(String, GraphRule a b)] -> [(String, SndOrderRule a b)] -> [t]
applySecondOrder f fstRules = concatMap (\r -> concatMap (f r) fstRules)

-- | Applies a named second order rule to a named first order rule with all possible matches,
-- and generates named first order rules as result.
applySndOrderRule :: MorphismsConfig -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule conf (sndName,sndRule) (fstName,fstRule) =
  let
    matches =
      findApplicableMatches conf sndRule fstRule

    newRules =
      map (`rewrite` sndRule) matches

    newNames =
      map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
  in
    zip newNames newRules
