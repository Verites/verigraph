module XML.ParseSndOrderRule where

import Abstract.Morphism
import Data.Char         (toLower)
import Data.List         ((\\),find,groupBy,sortOn)
import Data.Maybe        (fromMaybe)
import Data.String.Utils (join,split)
import Graph.Graph
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
    
    mapNodes = map (\(id,n) -> (find2 n nodesRMap, Nothing, id)) nodesLMap
    mapEdges = map (\(id,n) -> (find2 n edgesRMap, Nothing, id)) edgesLMap

find2 :: String -> [(String, String)] -> String
find2 _ [] = error "2rule, wrong map of object names"
find2 n ((a,b):xs) = if n == b then a else find2 n xs

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
    side = if (length splitted) /= 3 then error "Error parsing 2rule name" else map toLower $ splitted !! 1
    ruleName = join "_" (tail (tail splitted))

-- put together rules in pairs (left,right)
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
    getLeft list = fromMaybe (error ("Second order rule without left")) (find (\x -> side x == "left") list)
    getRight list = fromMaybe (error "Second order rule without right") (find (\x -> side x == "right") list)

-- left and right must have the same domain
getObjetcNameMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [Mapping]
getObjetcNameMorphism left right = nodesMap ++ edgesMap
  where
    nodesMap = map (\n -> (show (applyNodeTotalTGMMap right n), Nothing, show (applyNodeTotalTGMMap left n))) (nodes (domain (domain left)))
    edgesMap = map (\e -> (show (applyEdgeTotalTGMMap right e), Nothing, show (applyEdgeTotalTGMMap left e))) (edges (domain (domain left)))

applyNodeTotalTGMMap :: TypedGraphMorphism a b -> NodeId -> NodeId
applyNodeTotalTGMMap m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNodeTGM m n

applyEdgeTotalTGMMap :: TypedGraphMorphism a b -> EdgeId -> EdgeId
applyEdgeTotalTGMMap m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdgeTGM m e
