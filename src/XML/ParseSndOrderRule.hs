module XML.ParseSndOrderRule where

import Data.Char         (toLower)
import Data.List         (groupBy,sortOn)
import Data.String.Utils (join,split)
import XML.ParsedTypes

type Side = String
type Name = String

type SndOrderRuleSide = (Side, Name, Rule)

getRuleMapping :: SndOrderRuleSide -> [Mapping]
getRuleMapping (_,_,(_,_,_,x)) = x

getLeftObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getLeftObjNameMapping (_,_,(_,left,_,_)) (_,_,(_,right,_,_)) = getObjNameMapping left right

getRightObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getRightObjNameMapping (_,_,(_,_,left,_)) (_,_,(_,_,right,_)) = getObjNameMapping left right
    
getObjNameMapping :: ParsedTypedGraph -> ParsedTypedGraph -> [Mapping]
getObjNameMapping (_,nodesL,edgesL) (_,nodesR,edgesR) = mapNodes ++ mapEdges
  where
    fNodes = \(id,m,_) -> case m of
                            Just n -> [(id,n)]
                            Nothing -> []
    fEdges = \(id,m,_,_,_) -> case m of
                                Just n -> [(id,n)]
                                Nothing -> []
    nodesLMap = concatMap fNodes nodesL
    nodesRMap = concatMap fNodes nodesR
    edgesLMap = concatMap fEdges edgesL
    edgesRMap = concatMap fEdges edgesR
    
    mapNodes = map (\(id,n) -> (find n nodesRMap, Nothing, id)) nodesLMap
    mapEdges = map (\(id,n) -> (find n edgesRMap, Nothing, id)) edgesLMap

find :: String -> [(String, String)] -> String
find _ [] = error "2rule, map of object names wrong"
find n ((a,b):xs) = if n == b then a else find n xs

getLeftGraph :: SndOrderRuleSide -> ParsedTypedGraph
getLeftGraph (_,_,(_,x,_,_)) = x

getRightGraph :: SndOrderRuleSide -> ParsedTypedGraph
getRightGraph (_,_,(_,_,x,_)) = x

parseSndOrderRules :: [RuleWithNacs] -> [(SndOrderRuleSide,SndOrderRuleSide)]
parseSndOrderRules rules = groupedRules
  where
    rulesWithSide = map getSndOrderRuleSide rules
    groupedRules = groupRules rulesWithSide

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
