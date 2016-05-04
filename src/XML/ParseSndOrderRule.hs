module XML.ParseSndOrderRule where

import Graph.RuleMorphism
import Graph.SndOrderRule
import Data.Char         (toLower)
import Data.List         (groupBy,sortOn)
import Data.String.Utils (join,split)
import XML.ParsedTypes

type Side = String
type Name = String

type SndOrderRuleSide = (Side, Name, Rule)

parseSndOrderRules :: [RuleWithNacs] -> [SndOrderRule a b]
parseSndOrderRules rules = map (uncurry sndOrderRule) instantiatedRules
  where
    rulesWithSide = map getSndOrderRuleSide rules
    groupedRules = groupRules rulesWithSide
    instantiatedRules = map instantiateSndOrderRule groupedRules

-- Parse SndOrderRule names in the form: 2rule_left_ruleName
getSndOrderRuleSide :: RuleWithNacs -> SndOrderRuleSide
getSndOrderRuleSide (rule@(name,_,_,_),_) = (side, ruleName, rule)
  where
    splitted = split "_" name
    side = if (length splitted) < 3 then error "Error parsing 2rule name" else map toLower $ splitted !! 1
    ruleName = join "_" (tail (tail splitted))

-- put together rules in pairs (left,right)
groupRules :: [SndOrderRuleSide] -> [(SndOrderRuleSide,SndOrderRuleSide)]
groupRules rules = map (\l -> if name (head l) == "left" then (head l, last l) else (last l, head l)) grouped
  where
    name (_,x,_) = x
    sorted = sortOn name rules
    grouped = groupBy (\x y -> name x == name y) sorted

-- TODO
instantiateSndOrderRule :: (SndOrderRuleSide,SndOrderRuleSide) -> (RuleMorphism a b, RuleMorphism a b)
instantiateSndOrderRule _ = error "ha hae"
