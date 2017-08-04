{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rewriting.DPO.TypedGraphRule where

import Control.Monad
import           Data.Maybe                            (fromMaybe)

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraph                   ()
import           Category.TypedGraphRule               
import           Data.Graphs                           as G
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import Util.Monad

-- | A second-order rule:
--
-- @
--         nl       nr
--     NL◀─────\<NK\>─────▶NR
--      ▲        ▲        ▲
--   nacL\\    nacK\\    nacR\\
--        \\        \\        \\
--         \\   ll   \\   lr   \\
--         LL◀─────\<LK\>─────▶LR
--         ▲        ▲        ▲
--    leftL│   leftK│   leftR│
--         │        │        │
--         │    kl  │    kr  │
--         KL◀─────\<KK\>─────▶KR
--         │        │        │
--   rightL│  rightK│  rightR│
--         │        │        │
--         ▼    rl  ▼    rr  ▼
--         RL◀─────\<RK\>─────▶RR
-- @
--
-- domain rule = (ll,lr)
--
-- interface rule = (kl,kr)
--
-- codomain rule (rl,rr)
--
-- nac rule = (nl,nr)
--
-- nacs = set of: domain rule, nac rule, nacL, nacK, nacR
--
-- left = domain rule, interface rule, leftL, leftK, leftR
--
-- right = interface rule, codomain rule, rightL, rightK, rightR
type SndOrderRule n e = Production (TGRuleCat n e) (RuleMorphism n e)
type SndOrderGrammar n e = Grammar (TGRuleCat n e) (RuleMorphism n e)

instance DPO (TGRuleCat n e) (RuleMorphism n e) where
  invertProduction r = do
    shiftedNacs <- concatMapM (shiftNacOverProduction r) (nacs r)
    addMinimalSafetyNacs $ buildProduction (rightMorphism r) (leftMorphism r) shiftedNacs

  -- | Needs the satisfiesNACs extra verification because
  -- not every satisfiesGluingConditions nac can be shifted
  shiftNacOverProduction rule n = do
    safetyNacs <- minimalSafetyNacs rule
    let ruleWithOnlyMinimalSafetyNacs = rule { nacs = safetyNacs }

    canShift <- satisfiesGluingConditions rule n `andM` satisfiesNACs ruleWithOnlyMinimalSafetyNacs n
    if canShift
      then (:[]) <$> calculateComatch n rule
      else return []

---- Minimal Safety NACs

-- | Configuration for the minimalSafetyNACs algorithms, it defines from
-- which side of the leftMorphism (second-order production) is being analyzed
data Side = LeftSide | RightSide

-- | Either: Node_ is only a NodeId | Edge_ is an Edge with: EdgeId plus source and target NodeIds
data NodeOrEdge b = Node_ NodeId | Edge_ (Edge b) deriving (Show)

-- | Adds the minimal safety nacs needed to this production always produce a second-order rule.
-- If the nacs that going to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: SndOrderRule n e -> TGRuleCat n e (SndOrderRule n e)
addMinimalSafetyNacs sndRule = do
  safetyNacs <- filterM (satisfiesNACs sndRule) =<< minimalSafetyNacs sndRule
  return $ sndRule { nacs = nacs sndRule ++ safetyNacs }

-- | Generates the minimal safety NACs of a 2-rule
minimalSafetyNacs :: SndOrderRule n e -> TGRuleCat n e [RuleMorphism n e]
minimalSafetyNacs sndRule = do
  monicMatches <- matchMorphism `isSubclassOf` monic
  let monicSafetyNacs = newNacsProb LeftSide sndRule ++ newNacsProb RightSide sndRule
  if monicMatches
    then return monicSafetyNacs
    else do
      additionalSafetyNacs <- (++) <$> newNacsPair LeftSide sndRule <*> newNacsPair RightSide sndRule
      return (monicSafetyNacs ++ additionalSafetyNacs)

-- | Generate NACs that forbid deleting elements in L or R but not in K,
-- It discovers how situations must have a NAC and function createNacProb creates them.
-- Insert NACs to avoid condition (a) in Thm 70 (rodrigo machado phd thesis, 2012)
newNacsProb :: Side -> SndOrderRule n e -> [RuleMorphism n e]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide  -> (mappingLeft, leftMorphism)
        RightSide -> (mappingRight, rightMorphism)

    (ruleL, ruleK, ruleR) = (leftObject sndRule, interfaceObject sndRule, rightObject sndRule)

    f = mapSide (leftMorphism sndRule)
    g = mapSide (rightMorphism sndRule)

    sa = getSide ruleL
    sb = getSide ruleK
    sc = getSide ruleR

    nodeProb = [applyNodeIdUnsafe f n |
                 n <- nodeIdsFromCodomain sb
               , isOrphanNode sa (applyNodeIdUnsafe f n)
               , isOrphanNode sb n
               , not (isOrphanNode sc (applyNodeIdUnsafe g n))]

    edgeProb = [applyEdgeUnsafe f e |
                 e <- edgesFromCodomain sb
               , isOrphanEdge sa (applyEdgeIdUnsafe f (edgeId e))
               , isOrphanEdge sb (edgeId e)
               , not (isOrphanEdge sc (applyEdgeIdUnsafe g (edgeId e)))]

    nacNodes = map (createNacProb side ruleL . Node_) nodeProb
    nacEdges = map (createNacProb side ruleL . Edge_) edgeProb

-- | Auxiliar function that creates concrectly the NACs for newNacsProb
createNacProb :: Side -> TypedGraphRule n e -> NodeOrEdge (Maybe b) -> RuleMorphism n e
createNacProb sideChoose ruleL x = RuleMorphism ruleL nacRule mapL mapK mapR
  where
    l = leftMorphism ruleL
    r = rightMorphism ruleL

    graphL = codomain l
    graphK = domain l
    graphR = codomain r

    (graphSide, otherSideGraph, side, otherSide) =
      case sideChoose of
        LeftSide  -> (graphR, graphL, l, r)
        RightSide -> (graphL, graphR, r, l)

    tpNode = extractNodeType otherSideGraph
    tpEdge = extractEdgeType otherSideGraph . edgeId

    typeSrc x = extractNodeType otherSideGraph (sourceId x)
    typeTgt x = extractNodeType otherSideGraph (targetId x)

    n' = head (newNodes (domain graphK))
    n'' = head (newNodes (domain graphSide))

    e' = head (newEdges (domain graphK))
    e'' = head (newEdges (domain graphSide))

    newNodesK = newNodes (domain graphK)
    newNodesSide = newNodes (domain graphSide)

    invertSide = invert side

    srcInK x = fromMaybe (head newNodesK)           (applyNodeId invertSide (sourceId x))
    tgtInK x = fromMaybe (head (tail newNodesK))    (applyNodeId invertSide (targetId x))
    srcInR x = fromMaybe (head newNodesSide)        (applyNodeId otherSide (srcInK x))
    tgtInR x = fromMaybe (head (tail newNodesSide)) (applyNodeId otherSide (tgtInK x))

    (updateLeft, updateRight) =
      case x of
        (Node_ n) -> createNodes n n' n'' (tpNode n)
        (Edge_ e) -> createEdges (edgeId e) e' e'' (tpEdge e)
                       (sourceId e) (typeSrc e) (srcInK e) (srcInR e)
                       (targetId e) (typeTgt e) (tgtInK e) (tgtInR e)

    nacRule = buildProduction updateLeft updateRight []
    mapL = idMap graphL (codomain updateLeft)
    mapK = idMap graphK (domain updateLeft)
    mapR = idMap graphR (codomain updateRight)

    createNodes x x' x'' tp =
      case sideChoose of
        LeftSide  -> (updateSide1, updateSide2Map)
        RightSide -> (updateSide2Map, updateSide1)
      where
        updateSide1 = createNodeOnDomain x' tp x side
        updateSide2Cod = createNodeOnCodomain x'' tp otherSide
        updateSide2Map = updateNodeRelation x' x'' tp updateSide2Cod

    createEdges x x' x'' tp
        src typeSrc srcInK srcInR
        tgt typeTgt tgtInK tgtInR =
      case sideChoose of
        LeftSide  -> (updateLeftEdge, updateRightMap)
        RightSide -> (updateRightMap, updateLeftEdge)
      where
        srcRight = createNodeOnCodomain srcInR typeSrc otherSide
        tgtRight = createNodeOnCodomain tgtInR typeTgt srcRight
        updateRight = createNodeOnDomain srcInK typeSrc srcInR tgtRight
        updateRight2 = createNodeOnDomain tgtInK typeTgt tgtInR updateRight
        updateRightCod = createEdgeOnCodomain x'' srcInR tgtInR tp updateRight2
        updateRightMap = createEdgeOnDomain x' srcInK tgtInK tp x'' updateRightCod

        updateLeft = createNodeOnDomain srcInK typeSrc src side
        updateLeft2 = createNodeOnDomain tgtInK typeTgt tgt updateLeft
        updateLeftEdge = createEdgeOnDomain x' srcInK tgtInK tp x updateLeft2

-- | Generate NACs that forbid non monomorphic rule generation.
-- Insert NACs to avoid condition (b) in Thm 70 (rodrigo machado phd thesis, 2012)
newNacsPair :: forall n e. Side -> SndOrderRule n e -> TGRuleCat n e [RuleMorphism n e]
newNacsPair sideChoose sndRule = do
  let
    applyNode = applyNodeIdUnsafe
    applyEdge = applyEdgeIdUnsafe

    (ruleL, ruleK, ruleR) = (leftObject sndRule, interfaceObject sndRule, rightObject sndRule)

    (mapping, getSide) =
      case sideChoose of
        LeftSide  -> (mappingLeft, leftMorphism)
        RightSide -> (mappingRight, rightMorphism)

    fl = mapping (leftMorphism sndRule)
    gl = mapping (rightMorphism sndRule)

    lb = getSide ruleK
    lc = getSide ruleR

    --pairsNodes = pairs applyNode isOrphanNode nodeIds
    findPairs :: Eq a => (TypedGraphMorphism n e -> a -> a) -> (TypedGraphMorphism n e -> a -> Bool) -> (TypedGraph n e -> [a]) -> [(a, a)]
    findPairs apply isOrphan elems = 
      [ (apply fl x, apply fl y) 
          | x <- elems (codomain lb) , y <- elems (codomain lb) , x /= y
          , isOrphan lb x && not (isOrphan lc (apply gl x)) && not (isOrphan lc (apply gl y)) ]
    pairsNodes = findPairs applyNode isOrphanNode (nodeIds . untypedGraph)
    pairsEdges = findPairs applyEdge isOrphanEdge (edgeIds . untypedGraph)

  epis <- liftTGraph $ findAllQuotientsOf (codomain (getSide ruleL))
  let
    filteredNodes = [e | e <- epis, any (\(a,b) -> applyNode e a == applyNode e b) pairsNodes]
    filteredEdges = [e | e <- epis, any (\(a,b) -> applyEdge e a == applyEdge e b) pairsEdges]
  (++) <$> mapMaybeM (createNac ruleL) filteredNodes <*> mapMaybeM (createNac ruleL) filteredEdges
  where
    createNac ruleL e = do
      valid <- isValid n
      return $ if valid then Just n else Nothing
      where
        n = case sideChoose of
              LeftSide  -> nLeft
              RightSide -> nRight

        nLeft = RuleMorphism ruleL ruleNacLeft e mapK mapR
        nRight = RuleMorphism ruleL ruleNacRight mapL mapK e

        ruleNacLeft = buildProduction (e <&> leftMorphism ruleL) (rightMorphism ruleL) []
        ruleNacRight = buildProduction (leftMorphism ruleL) (e <&> rightMorphism ruleL) []

        mapL = idMap (leftObject ruleL) (leftObject ruleL)
        mapK = idMap (interfaceObject ruleL) (interfaceObject ruleL)
        mapR = idMap (rightObject ruleL) (rightObject ruleL)

getRulesFrom2Rule :: SndOrderRule n e -> (TypedGraphRule n e, TypedGraphRule n e, TypedGraphRule n e)
getRulesFrom2Rule sndRule = (leftObject sndRule, interfaceObject sndRule, rightObject sndRule)

isOrphanNode :: TypedGraphMorphism n e -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism n e -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m

-- | Receives a function that works with a second-order and a first-order rule.
-- Apply this function on all possible combinations of rules.
applySecondOrderFunction ::
     ((String, SndOrderRule n e) -> (String, TypedGraphRule n e) -> [t])
  -> [(String, TypedGraphRule n e)] -> [(String, SndOrderRule n e)] -> [t]
applySecondOrderFunction f fstRules = concatMap (\r -> concatMap (f r) fstRules)
