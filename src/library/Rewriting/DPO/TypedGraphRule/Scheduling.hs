{-|
Module      : Scheduling
Description : Provides algorithms to apply second-order rules.
-}
module Rewriting.DPO.TypedGraphRule.Scheduling
  ( asLongAsPossible
  , oneStep
  , specific
  ) where

import           Data.Maybe                   (fromMaybe)

import           Abstract.Rewriting.DPO       (findApplicableMatches,rewrite)
import qualified Category.TypedGraphRule as TGRule
import           Rewriting.DPO.TypedGraph     (TypedGraphRule)
import           Rewriting.DPO.TypedGraphRule (SndOrderRule)

-- second-order rule is synonymous of 2-rule.
-- first-order rule is synonymous of rule.
type NamedRule n e = (String,TypedGraphRule n e)
type Named2Rule n e = (String,SndOrderRule n e)

-- | Apply "AsLongAsPossible" any second-order rule.
-- It is limited to `n` rewritings.
asLongAsPossible :: [Named2Rule n e] -> [NamedRule n e] -> Int -> TGRule.CatM n e (String,[NamedRule n e])
asLongAsPossible s f n = asLongAsPossible_ s f ("limit of rewritings: " ++ show n) n

-- Each iteration applies a rewrite, removes the old rule, and put the new rule in the end of the rules list.
asLongAsPossible_ :: [Named2Rule n e] -> [NamedRule n e] -> String -> Int -> TGRule.CatM n e (String,[NamedRule n e])
asLongAsPossible_ _ fstRules log 0 = return (log ++ "\n limit of rewritings exceeded.",fstRules) 
asLongAsPossible_ sndRules fstRules log n = do
  matches <- getOneMatch sndRules [] fstRules
  case matches of
    Nothing -> return (log++"\nno more matches.",fstRules)
    Just (m,r2,r1Name,oldRules) -> do
      let newName = r1Name ++ "_" ++ fst r2
          newLog = log ++ "\n " ++ r1Name ++ " --" ++ fst r2 ++ "--> " ++ newName ++ "."
      newRule <- rewrite m (snd r2)
      let newFstOrder = oldRules ++ [(newName,newRule)]
      asLongAsPossible_ sndRules newFstOrder newLog (n-1)

-- Returns just one match (if it exits) between [2-rules] and [rules].
-- Also, returns the list of rules without the matched one.
getOneMatch :: [Named2Rule n e] -> [NamedRule n e] -> [NamedRule n e] -> TGRule.CatM n e (Maybe (TGRule.Morphism n e, Named2Rule n e, String, [NamedRule n e]))
getOneMatch _ _ [] = return Nothing
getOneMatch sndRules rs1 (r:rs2) = do
  match <- getOneMatch_ r sndRules
  case match of 
    Nothing -> getOneMatch sndRules (r:rs1) rs2
    Just (m,r2,r1Name) -> return (Just (m,r2,r1Name,rs1 ++ rs2))

getOneMatch_ :: NamedRule n e -> [Named2Rule n e] -> TGRule.CatM n e (Maybe (TGRule.Morphism n e, Named2Rule n e, String))
getOneMatch_ _ [] = return Nothing
getOneMatch_ rule (r2:r2s) = do
  matches <- findApplicableMatches (snd r2) (snd rule)
  case matches of
    [] -> getOneMatch_ rule r2s
    m:_ ->  return $ Just (m, r2, fst rule)

-- | Apply "oneStep".
-- All matches from 2-rules to rules are applied once.
oneStep :: [Named2Rule n e] -> [NamedRule n e] -> TGRule.CatM n e (String,[NamedRule n e])
oneStep sndRules fstRules = do
  info <- mapM (oneStep_ sndRules) fstRules
  let
    logs = concatMap fst info
    rules = concatMap snd info
  return (logs,rules)

oneStep_ :: [Named2Rule n e] -> NamedRule n e -> TGRule.CatM n e (String,[NamedRule n e])
oneStep_ sndRules rule = do
  info <- mapM (applyAllMatches rule) sndRules
  let
    logs = concatMap fst info
    rules = concatMap snd info
  return (logs,rules)

applyAllMatches :: NamedRule n e -> Named2Rule n e -> TGRule.CatM n e (String,[NamedRule n e])
applyAllMatches r1 r2 = do
  matches <- findApplicableMatches (snd r2) (snd r1)
  newRules <- mapM (`rewrite` snd r2) matches
  let
    namedRules = zipWith (\r id -> (fst r1 ++"_"++ fst r2 ++"_"++show (id :: Int), r)) newRules [0..]
    log = concatMap (\(name,_) -> "\n "++ fst r1 ++" --"++ fst r2 ++"--> "++name++".") namedRules
  return (log, namedRules)

-- | Apply "Specific" from a 2rule to a rule.
-- Receives a 2-rule name and a rule name, and rewrites all matches between them.
specific :: [Named2Rule n e] -> [NamedRule n e] -> String -> String -> TGRule.CatM n e (String,[NamedRule n e])
specific sndRules fstRules name2Rule nameRule = applyAllMatches (nameRule,rule1) (name2Rule,rule2)
  where
    rule1 = getRule fstRules nameRule
    rule2 = getRule sndRules name2Rule

getRule :: [(String, a)] -> String -> a
getRule rules name = fromMaybe (error ("specific: "++name++" rule not found.")) (lookup name rules)
