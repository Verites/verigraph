{-|
Module      : Scheduling
Description : Provides algorithms to apply second-order rules.
-}
module Rewriting.DPO.TypedGraphRule.Scheduling
  ( asLongAsPossible
  , oneStep
  , specific
  ) where

import           Data.Maybe                   (fromMaybe, isNothing)

import           Abstract.Rewriting.DPO       (MorphismsConfig,findApplicableMatches,rewrite)
import           Category.TypedGraphRule      (RuleMorphism)
import           Rewriting.DPO.TypedGraph     (TypedGraphRule)
import           Rewriting.DPO.TypedGraphRule (SndOrderRule)

-- second-order rule is synonymous of 2-rule.
-- first-order rule is synonymous of rule.
type NamedRule a b = (String,TypedGraphRule a b)
type Named2Rule a b = (String,SndOrderRule a b)

-- | Apply "AsLongAsPossible" any second-order rule.
-- It is limited to `n` rewritings.
asLongAsPossible :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> [NamedRule a b] -> Int -> (String,[NamedRule a b])
asLongAsPossible c s f n = asLongAsPossible_ c s f ("limit of rewritings: " ++ show n) n

-- Each iteration applies a rewrite, removes the old rule, and put the new rule in the end of the rules list.
asLongAsPossible_ :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> [NamedRule a b] -> String -> Int -> (String,[NamedRule a b])
asLongAsPossible_ _ _ fstRules log 0 = (log ++ "\n limit of rewritings exceeded.",fstRules) 
asLongAsPossible_ conf sndRules fstRules log n =
  if isNothing matches
    then (log++"\nno more matches.",fstRules)
    else asLongAsPossible_ conf sndRules newFstOrder newLog (n-1)
  where
    newLog = log ++ "\n " ++ r1Name ++ " --" ++ fst r2 ++ "--> " ++ newName ++ "."
    matches = getOneMatch conf sndRules [] fstRules
    (Just (m,r2,r1Name,oldRules)) = matches
    newName = r1Name ++ "_" ++ fst r2
    newRule = rewrite m (snd r2)
    newFstOrder = oldRules ++ [(newName,newRule)]

-- Returns just one match (if it exits) between [2-rules] and [rules].
-- Also, returns the list of rules without the matched one.
getOneMatch :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> [NamedRule a b] -> [NamedRule a b] -> Maybe (RuleMorphism a b, Named2Rule a b, String, [NamedRule a b])
getOneMatch _ _ _ [] = Nothing
getOneMatch conf sndRules rs1 (r:rs2) =
  let matches = getOneMatch_ conf r sndRules
      (Just (m,r2,r1Name)) = matches
  in  if isNothing matches then getOneMatch conf sndRules (r:rs1) rs2 else Just (m,r2,r1Name,rs1 ++ rs2)

getOneMatch_ :: MorphismsConfig (RuleMorphism a b) -> NamedRule a b -> [Named2Rule a b] -> Maybe (RuleMorphism a b, Named2Rule a b, String)
getOneMatch_ _ _ [] = Nothing
getOneMatch_ conf rule (r2:r2s) =
  let matches = findApplicableMatches conf (snd r2) (snd rule)
  in  if Prelude.null matches then getOneMatch_ conf rule r2s else Just (head matches, r2, fst rule)

-- | Apply "oneStep".
-- All matches from 2-rules to rules are applied once.
oneStep :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> [NamedRule a b] -> (String,[NamedRule a b])
oneStep conf sndRules fstRules = (logs,rules)
  where
    info = map (oneStep_ conf sndRules) fstRules
    logs = concatMap fst info
    rules = concatMap snd info

oneStep_ :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> NamedRule a b -> (String,[NamedRule a b])
oneStep_ conf sndRules rule = (logs,rules)
  where
    info = map (applyAllMatches conf rule) sndRules
    logs = concatMap fst info
    rules = concatMap snd info

applyAllMatches :: MorphismsConfig (RuleMorphism a b) -> NamedRule a b -> Named2Rule a b -> (String,[NamedRule a b])
applyAllMatches conf r1 r2 = (log,namedRules)
  where
    matches = findApplicableMatches conf (snd r2) (snd r1)
    newRules = map (`rewrite` snd r2) matches
    newRulesId = zip newRules ([0..]::[Int])
    namedRules = map (\(r,id) -> (fst r1 ++"_"++ fst r2 ++"_"++show id,r)) newRulesId
    log = concatMap (\(name,_) -> "\n "++ fst r1 ++" --"++ fst r2 ++"--> "++name++".") namedRules

-- | Apply "Specific" from a 2rule to a rule.
-- Receives a 2-rule name and a rule name, and rewrites all matches between them.
specific :: MorphismsConfig (RuleMorphism a b) -> [Named2Rule a b] -> [NamedRule a b] -> String -> String -> (String,[NamedRule a b])
specific conf sndRules fstRules name2Rule nameRule = applyAllMatches conf (nameRule,rule1) (name2Rule,rule2)
  where
    rule1 = getRule fstRules nameRule
    rule2 = getRule sndRules name2Rule

getRule :: [(String, a)] -> String -> a
getRule rules name = fromMaybe (error ("specific: "++name++" rule not found.")) (lookup name rules)
