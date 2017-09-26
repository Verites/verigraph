{-|
Module      : ParseSndOrderRule
Description : Imports 2-rules from .ggx to verigraph
Stability   : experimental

AGG implements first-order rules in the SPO approach, to model 2-rules
(second-order rules in the DPO approach) a translation is needed.

AGG model: a rule is two graphs and a partial mapping between them.

@
  N
  ▲
  │
  L ───▶ R
@

Verigraph second-order model:

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

The second-order rules in AGG must be represented as two first-order rules with some aditional maps.

This maps bind two graph in different rules, to represent it we use the object names in AGG.

The object names map must bind the graphs La to Ra and Lc to Rc,
if there a NAC these maps will be needes: Nl to Nr, La to Nl and Ra to Nr.

Besides that, rule names in agg must follow this form: 2rule_(left|right|nacid)_(ruleName)

The translation from first-order rules in the SPO to DPO is straightforward,
and additionally with object name maps, all second-order rule can be instantiated.
-}

module XML.ParseSndOrderRule
  ( parseSndOrderRules
  , getLeftObjNameMapping
  , getRightObjNameMapping
  , getObjectNacNameMorphism
  , getObjectNacNameMorphismNodes
  , getObjectNacNameMorphismEdges
  , getObjectNameMorphism
  ) where

import           Data.Char                (toLower)
import           Data.Function            (on)
import           Data.List                (find, groupBy, intercalate, sortBy, sortOn, (\\))
import           Data.Maybe               (fromMaybe, mapMaybe)

import           Abstract.Category
import           Category.Graph           ()
import           Category.TypedGraph      ()
import qualified Data.Graphs              as G
import           Data.Graphs.Morphism     as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism as TGM
import           Util.List
import           XML.ParsedTypes

-- | Gets the object name map between the left of two rules
getLeftObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getLeftObjNameMapping (_,_,((_,left,_,_),_)) (_,_,((_,right,_,_),_)) = getObjNameMapping left right

-- | Gets the object name map between the right of two rules
getRightObjNameMapping :: SndOrderRuleSide -> SndOrderRuleSide -> [Mapping]
getRightObjNameMapping (_,_,((_,_,left,_),_)) (_,_,((_,_,right,_),_)) = getObjNameMapping left right

-- | Gets the object name map between two ParsedTypedGraph
getObjNameMapping :: ParsedTypedGraph -> ParsedTypedGraph -> [Mapping]
getObjNameMapping (_,nodesL,edgesL) (_,nodesR,edgesR) = mapNodes ++ mapEdges
  where
    f id (Just n) = Just (id,n)
    f _ _         = Nothing

    fNodes (id,m,_) = f id m
    fEdges (id,m,_,_,_) = f id m

    nodesLMap = mapMaybe fNodes nodesL
    nodesRMap = mapMaybe fNodes nodesR
    edgesLMap = mapMaybe fEdges edgesL
    edgesRMap = mapMaybe fEdges edgesR

    getMap f = mapMaybe
                 (\(id,n) ->
                   case find (\(_,b) -> n == b) f of
                     Just (x,_) -> Just (x, Nothing, id)
                     _          -> Nothing)

    nonMono = concatMap
                (\(id,objName) ->
                  map
                    (\name -> (id,name))
                    (split "|" objName)
                )

    mapNodes = getMap (nonMono nodesRMap) nodesLMap
    mapEdges = getMap (nonMono edgesRMap) edgesLMap

-- | Receives all parsed 2-rules in the agg format (first-order rule with object name maps)
-- and converts to second-order rules on verigraph
parseSndOrderRules :: [RuleWithNacs] -> [(SndOrderRuleSide,SndOrderRuleSide,[SndOrderRuleSide])]
parseSndOrderRules = groupRules . map getSndOrderRuleSide

-- | Parse SndOrderRule names in the form: 2rule_left_ruleName or 2rule_nacName_ruleName
getSndOrderRuleSide :: RuleWithNacs -> SndOrderRuleSide
getSndOrderRuleSide rule@((name,_,_,_),_) = (side, ruleName, rule)
  where
    splitted = split "_" name
    side = if length splitted < 3
             then error "Error parsing 2rule name"
             else map toLower $ splitted !! 1
    ruleName = intercalate "_" (tail (tail splitted))

-- put together rules in the form (left,right,[nacs])
groupRules :: [SndOrderRuleSide] -> [(SndOrderRuleSide,SndOrderRuleSide,[SndOrderRuleSide])]
groupRules rules =
  map
    (\list ->
      let left = getLeft list
          right = getRight list
          remainList = list \\ [left,right]
       in (left,right,remainList)
     ) grouped
  where
    side (x,_,_) = x
    name (_,x,_) = x
    sorted = sortOn name rules
    grouped = groupBy ((==) `on` name) sorted
    getLeft list = fromMaybe (error "Second-order rule without left") (findSide "left" list)
    getRight list = fromMaybe (error "Second-order rule without right") (findSide "right" list)
    findSide str = find (\x -> side x == str)

-- TODO: replace applyNodeUnsafe for getNodeType?
-- | Given a morphism from some graph in the rule left to nac extracts the mapping
getObjectNacNameMorph :: GraphMorphism a b -> ([Mapping], [Mapping])
getObjectNacNameMorph m = (nodesMap m, edgesMap m)
  where
    adjustNonMono = parseNonMonoObjNames . group . sort
    nodesMap = adjustNonMono . getMap GM.applyNodeIdUnsafe . G.nodeIds . domain
    edgesMap = adjustNonMono . getMap GM.applyEdgeIdUnsafe . G.edgeIds . domain

    getMap f = map (\e -> (show (f m e), Nothing, show e))
    group = groupBy (\(x,_,_) (y,_,_) -> x == y)
    sort = sortBy (\(x,_,_) (y,_,_) -> compare x y)

-- | Given a morphism from some graph in the rule left to nac extracts the mapping
getObjectNacNameMorphism :: GraphMorphism a b -> [Mapping]
getObjectNacNameMorphism m = nods ++ edgs
  where
    (nods,edgs) = getObjectNacNameMorph m

-- | Given a morphism from some graph in the rule left to nac extracts the nodes mapping
getObjectNacNameMorphismNodes :: GraphMorphism a b -> [Mapping]
getObjectNacNameMorphismNodes m = fst (getObjectNacNameMorph m)

-- | Given a morphism from some graph in the rule left to nac extracts the edges mapping
getObjectNacNameMorphismEdges :: GraphMorphism a b -> [Mapping]
getObjectNacNameMorphismEdges m = snd (getObjectNacNameMorph m)

-- | Glues the non mono maps
parseNonMonoObjNames :: [[Mapping]] -> [Mapping]
parseNonMonoObjNames [] = []
parseNonMonoObjNames (x:xs) = (a,b,newObjName) : parseNonMonoObjNames xs
  where
    (a,b,_) = head x
    allObjNames = map (\(_,_,y) -> y) x
    newObjName = intercalate "|" allObjNames

-- | Given two morphisms with the same domain, maps the codomain of both according to the interface (domain graph)
-- Used to translate DPO in verigraph to SPO in ggx
getObjectNameMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [Mapping]
getObjectNameMorphism left right = nodesMap ++ edgesMap
  where
    nodesMap = getMap TGM.applyNodeIdUnsafe (nodeIds $ domain left)
    edgesMap = getMap TGM.applyEdgeIdUnsafe (edgeIds $ domain left)
    getMap f = map (\e -> (show (f right e), Nothing, show (f left e)))
