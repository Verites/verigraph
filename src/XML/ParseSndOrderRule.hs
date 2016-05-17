{-|
Module      : ParseSndOrderRule
Description : Imports 2-rules from .ggx to verigraph
Stability   : experimental

AGG implements first order rules in the SPO approach, to model 2-rules
(second order rules in the DPO approach) a translation is needed.

AGG model: a rule is two graphs and a partial mapping between them.

@
  N
  ▲
  │
  L ───▶ R
@

Verigraph second order model:

@
        nl       fl       fr
    Nl◀──────La◀─────\<Lb\>─────▶Lc
    ▲        ▲        ▲        ▲
 nal│      la│      lb│      lc│
    │        │        │        │
    ^   nk   ^   fk   ^   gk   ^
    Nk◀──────Ka◀─────\<Kb\>─────▶Kc
    v        v        v        v
 nar│      ra│      rb│      rc│
    │        │        │        │
    v   nr   ▼   fr   ▼   gr   ▼
    Nr◀──────Ra◀─────\<Rb\>─────▶Rc
@

The second order rules in AGG must be represented as two first order rules with some aditional maps.

This maps bind two graph in different rules, to represent it we use the object names in AGG.

The object names map must bind the graphs La to Ra and Lc to Rc,
if there a NAC these maps will be needes: Nl to Nr, La to Nl and Ra to Nr.

Besides that, rule names in agg must follow this form: 2rule_(left|right|nacid)_(ruleName)

The translation from first order rules in the SPO to DPO is straightforward,
and additionally with object name maps, all second order rule can be instantiated.
-}

module XML.ParseSndOrderRule (
    parseSndOrderRules
  , getLeftObjNameMapping
  , getRightObjNameMapping
  , getObjectNacNameMorphism
  , getObjectNameMorphism
  ) where

import Abstract.Morphism
import Data.Char         (toLower)
import Data.List         ((\\),find,groupBy,sortOn)
import Data.Maybe        (mapMaybe,fromMaybe)
import Data.String.Utils (join,split)
import Graph.Graph
import Graph.GraphMorphism
import Graph.TypedGraphMorphism
import XML.ParsedTypes

getLeftObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getLeftObjNameMapping (_,_,(_,left,_,_)) (_,_,(_,right,_,_)) = getObjNameMapping left right

getRightObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getRightObjNameMapping (_,_,(_,_,left,_)) (_,_,(_,_,right,_)) = getObjNameMapping left right

getObjNameMapping :: ParsedTypedGraph -> ParsedTypedGraph -> [Mapping]
getObjNameMapping (_,nodesL,edgesL) (_,nodesR,edgesR) = mapNodes ++ mapEdges
  where
    f id m = case m of
               Just n -> Just (id,n)
               Nothing -> Nothing
    
    fNodes (id,m,_) = f id m
    fEdges (id,m,_,_,_) = f id m
    
    nodesLMap = mapMaybe fNodes nodesL
    nodesRMap = mapMaybe fNodes nodesR
    edgesLMap = mapMaybe fEdges edgesL
    edgesRMap = mapMaybe fEdges edgesR
    
    getMap f = mapMaybe
                 (\(id,n) -> case find2 n f of
                    Just x -> Just (x, Nothing, id)
                    Nothing -> Nothing)
    
    mapNodes = getMap (adjMono nodesRMap) nodesLMap
    mapEdges = getMap (adjMono edgesRMap) edgesLMap

find2 :: String -> [(String, String)] -> Maybe String
find2 n list = case find (\(_,b) -> n == b) list of
                      Just (x,_) -> Just x
                      Nothing -> Nothing

adjMono :: [(String, String)] -> [(String, String)]
adjMono [] = []
adjMono ((id,objName):xs) = newObjNames ++ (adjMono xs)
  where
    splitted = split "|" objName
    newObjNames = map (\name -> (id,name)) splitted

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

getObjectNacNameMorphism :: GraphMorphism a b -> [Mapping]
getObjectNacNameMorphism m = nodesMap ++ edgesMap
  where
    nodesMap = parseNonMonoObjNames (group (getMap applyNodeUnsafe (nodes (domain m))))
    edgesMap = parseNonMonoObjNames (group (getMap applyEdgeUnsafe (edges (domain m))))
    
    getMap f = map (\e -> (show (f m e), Nothing, show e))
    group = groupBy (\(x,_,_) (y,_,_) -> x == y)

parseNonMonoObjNames :: [[Mapping]] -> [Mapping]
parseNonMonoObjNames [] = []
parseNonMonoObjNames (x:xs) = (a,b,newObjName) : (parseNonMonoObjNames xs)
  where
    (a,b,_) = head x
    allObjNames = map (\(_,_,y) -> y) x
    newObjName = join "|" allObjNames

-- left and right must have the same domain
getObjectNameMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [Mapping]
getObjectNameMorphism left right = nodesMap ++ edgesMap
  where
    nodesMap = getMap applyNodeTGMUnsafe (nodesDomain left)
    edgesMap = getMap applyEdgeTGMUnsafe (edgesDomain left)
    getMap f = map (\e -> (show (f right e), Nothing, show (f left e)))
