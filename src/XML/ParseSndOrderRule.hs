module XML.ParseSndOrderRule where

import Data.Char         (toLower)
import Data.List         ((\\),find,groupBy,sortOn)
import Data.Maybe        (fromMaybe)
import Data.String.Utils (join,split)
import Graph.TypedGraphMorphism
import XML.ParsedTypes

getRuleMapping :: SndOrderRuleSide -> [Mapping]
getRuleMapping (_,_,(_,_,_,x)) = x

getLeftObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getLeftObjNameMapping (_,_,(_,left,_,_)) (_,_,(_,right,_,_)) = getObjNameMapping left right

getRightObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getRightObjNameMapping (_,_,(_,_,left,_)) (_,_,(_,_,right,_)) = getObjNameMapping left right
    
getObjNameMapping :: ParsedTypedGraph -> ParsedTypedGraph -> [Mapping]
getObjNameMapping (_,nodesL,edgesL) (_,nodesR,edgesR) = mapNodes ++ mapEdges
  where
    f id m = case m of
               Just n -> [(id,n)]
               Nothing -> []
    fNodes = \(id,m,_) -> f id m
    fEdges = \(id,m,_,_,_) -> f id m
    nodesLMap = concatMap fNodes nodesL
    nodesRMap = concatMap fNodes nodesR
    edgesLMap = concatMap fEdges edgesL
    edgesRMap = concatMap fEdges edgesR
    
    getMap f = map (\(id,n) -> (find2 n f, Nothing, id))
    mapNodes = getMap nodesRMap nodesLMap
    mapEdges = getMap edgesRMap edgesLMap

find2 :: String -> [(String, String)] -> String
find2 n list = fst $
  fromMaybe
    (error "2rule, wrong map of object names")
    (find (\(_,b) -> n == b) list)

getLeftGraph :: SndOrderRuleSide -> ParsedTypedGraph
getLeftGraph (_,_,(_,x,_,_)) = x

getRightGraph :: SndOrderRuleSide -> ParsedTypedGraph
getRightGraph (_,_,(_,_,x,_)) = x

parseSndOrderRules :: [RuleWithNacs] -> [(SndOrderRuleSide,SndOrderRuleSide,[SndOrderRuleSide])]
parseSndOrderRules rules = groupedRules
  where
    rulesWithSide = map getSndOrderRuleSide rules
    groupedRules = groupRules rulesWithSide

-- Parse SndOrderRule names in the form: 2rule_left_ruleName or 2rule_nacName_ruleName
getSndOrderRuleSide :: RuleWithNacs -> SndOrderRuleSide
getSndOrderRuleSide (rule@(name,_,_,_),_) = (side, ruleName, rule)
  where
    splitted = split "_" name
    side = if (length splitted) < 3
             then error "Error parsing 2rule name"
             else map toLower $ splitted !! 1
    ruleName = join "_" (tail (tail splitted))

-- put together rules in the form (left,right,[nacs])
groupRules :: [SndOrderRuleSide] -> [(SndOrderRuleSide,SndOrderRuleSide,[SndOrderRuleSide])]
groupRules rules = map
                     (\list ->
                        let left = getLeft list
                            right = getRight list
                            remainList = list \\ [left,right] in
                            (left,right,remainList)) grouped
  where
    side (x,_,_) = x
    name (_,x,_) = x
    sorted = sortOn name rules
    grouped = groupBy (\x y -> name x == name y) sorted
    getLeft list = fromMaybe (error ("Second order rule without left")) ((findSide "left") list)
    getRight list = fromMaybe (error "Second order rule without right") ((findSide "right") list)
    findSide str = find (\x -> side x == str)

-- left and right must have the same domain
getObjetcNameMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [Mapping]
getObjetcNameMorphism left right = nodesMap ++ edgesMap
  where
    nodesMap = getMap applyNodeTGMUnsafe (nodesDomain left)
    edgesMap = getMap applyEdgeTGMUnsafe (edgesDomain left)
    getMap f = map (\e -> (show (f right e), Nothing, show (f left e)))
