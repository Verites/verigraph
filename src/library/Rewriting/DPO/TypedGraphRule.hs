{-# LANGUAGE TypeFamilies #-}
module Rewriting.DPO.TypedGraphRule where

import           Data.Maybe                                  (fromMaybe, mapMaybe)

import           Abstract.Category.AdhesiveHLR
import           Abstract.Category.FinitaryCategory
import           Abstract.Category.JointlyEpimorphisms
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraphRule
import qualified Category.TypedGraphRule.AdhesiveHLR         as SO ()
import           Category.TypedGraphRule.FindMorphism        ()
import           Category.TypedGraphRule.JointlyEpimorphisms
import           Data.Graphs                                 as G
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph

-- | A second order rule:
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
type SndOrderRule a b = Production (RuleMorphism a b)

instance DPO (RuleMorphism a b) where
  invertProduction conf r = addMinimalSafetyNacs conf newRule
    where
      newRule = buildProduction (getRHS r) (getLHS r)
                 (concatMap (shiftNacOverProduction conf r) (getNACs r))

  -- | Needs the satisfiesNACs extra verification because
  -- not every satisfiesGluingConditions nac can be shifted
  shiftNacOverProduction conf rule n =
    [calculateComatch n rule |
      satisfiesGluingConditions conf rule n &&
      satisfiesNACs conf ruleWithOnlyMinimalSafetyNacs n]
    where
      ruleWithOnlyMinimalSafetyNacs =
        buildProduction (getLHS rule) (getRHS rule) (minimalSafetyNacs conf rule)

  createJointlyEpimorphicPairsFromNAC conf ruleR nac = ret
    where
      createJointly x = createJointlyEpimorphicPairsFromNAC conf (codomain x)

      nL = mappingLeft nac
      nK = mappingInterface nac
      nR = mappingRight nac
      leftR = getLHS ruleR
      rightR = getRHS ruleR
      rK = domain (getLHS ruleR)
      codNac = codomain nac

      interfaceEpiPairs = createJointlyEpimorphicPairsFromNAC conf rK nK

      lefts = concatMap
                (\(kR,kN) ->
                  let ls = createSideRule createJointly kR leftR leftR kN (getLHS codNac) nL
                  in map (\(ll1,ll2,m) -> (kR, kN, ll1, ll2, m)) ls)
                interfaceEpiPairs

      rights = concatMap
                (\(kR,kN,ll1,ll2,l) ->
                  let rs = createSideRule createJointly kR rightR rightR kN (getRHS codNac) nR
                  in map (\(rr1,rr2,r) -> (kR,kN,ll1,ll2,l,rr1,rr2,r)) rs)
                lefts

      transposeNACs l = map (snd . calculatePushout l)

      ret = map (\(k1,k2,ll1,ll2,l,r1,r2,r) ->
                   let rule = buildProduction l r $ transposeNACs ll1 (getNACs ruleR) ++ transposeNACs ll2 (getNACs codNac)
                   in (ruleMorphism ruleR rule ll1 k1 r1,
                       ruleMorphism (codomain nac) rule ll2 k2 r2)) rights

---- Minimal Safety NACs

-- | Configuration for the minimalSafetyNACs algorithms, it defines from
-- which side of the getLHS (second order production) is being analyzed
data Side = LeftSide | RightSide

-- | Either: Node_ is only a NodeId | Edge_ is an Edge with: EdgeId plus source and target NodeIds
data NodeOrEdge b = Node_ NodeId | Edge_ (Edge b) deriving (Show)

-- | Adds the minimal safety nacs needed to this production always produce a second order rule.
-- If the nacs that going to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: MorphismsConfig -> SndOrderRule a b -> SndOrderRule a b
addMinimalSafetyNacs conf sndRule =
  buildProduction
    (getLHS sndRule)
    (getRHS sndRule)
    (getNACs sndRule ++
     filter (satisfiesNACs conf sndRule) (minimalSafetyNacs conf sndRule))

-- | Generates the minimal safety NACs of a 2-rule
minimalSafetyNacs :: MorphismsConfig -> SndOrderRule a b -> [RuleMorphism a b]
minimalSafetyNacs conf sndRule =
  newNacsProb LeftSide sndRule ++
  newNacsProb RightSide sndRule ++
  ( if matchRestriction conf == GenericMorphism then
      newNacsPair LeftSide sndRule ++ newNacsPair RightSide sndRule
    else
      []
  )

-- | Generate NACs that forbid deleting elements in L or R but not in K,
-- It discovers how situations must have a NAC and function createNacProb creates them.
-- Insert NACs to avoid condition (a) in Thm 70 (rodrigo machado phd thesis, 2012)
newNacsProb :: Side -> SndOrderRule a b -> [RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide  -> (mappingLeft, getLHS)
        RightSide -> (mappingRight, getRHS)

    (ruleL, ruleK, ruleR) = getRulesFrom2Rule sndRule

    f = mapSide (getLHS sndRule)
    g = mapSide (getRHS sndRule)

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
createNacProb :: Side -> TypedGraphRule a b -> NodeOrEdge (Maybe b) -> RuleMorphism a b
createNacProb sideChoose ruleL x = ruleMorphism ruleL nacRule mapL mapK mapR
  where
    l = getLHS ruleL
    r = getRHS ruleL

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
newNacsPair :: Side -> SndOrderRule a b -> [RuleMorphism a b]
newNacsPair sideChoose sndRule =
  mapMaybe createNac filteredNodes ++ mapMaybe createNac filteredEdges
  where
    applyNode = applyNodeIdUnsafe
    applyEdge = applyEdgeIdUnsafe

    (ruleL, ruleK, ruleR) = getRulesFrom2Rule sndRule

    (mapping, getSide) =
      case sideChoose of
        LeftSide  -> (mappingLeft, getLHS)
        RightSide -> (mappingRight, getRHS)

    fl = mapping (getLHS sndRule)
    gl = mapping (getRHS sndRule)

    lb = getSide ruleK
    lc = getSide ruleR

    --pairsNodes = pairs applyNode isOrphanNode nodeIds
    pairsNodes =
      [(applyNode fl x, applyNode fl y) |
          x <- nodeIds $ domain $ codomain lb
        , y <- nodeIds $ domain $ codomain lb
        , x /= y
        , isOrphanNode lb x
        , not (isOrphanNode lc (applyNode gl x))
        , not (isOrphanNode lc (applyNode gl y))]


    --pairsEdges = pairs applyEdge isOrphanEdge edgeIds
    pairsEdges =
      [(applyEdge fl x, applyEdge fl y) |
          x <- edgeIds $ domain $ codomain lb
        , y <- edgeIds $ domain $ codomain lb
        , x /= y
        , isOrphanEdge lb x
        , not (isOrphanEdge lc (applyEdge gl x))
        , not (isOrphanEdge lc (applyEdge gl y))]

    epis = calculateAllPartitions (codomain (getSide ruleL))

    filteredNodes = [e | e <- epis, any (\(a,b) -> applyNode e a == applyNode e b) pairsNodes]
    filteredEdges = [e | e <- epis, any (\(a,b) -> applyEdge e a == applyEdge e b) pairsEdges]

    createNac e = if isValid n then Just n else Nothing
      where
        n = case sideChoose of
              LeftSide  -> nLeft
              RightSide -> nRight

        nLeft = ruleMorphism ruleL ruleNacLeft e mapK mapR
        nRight = ruleMorphism ruleL ruleNacRight mapL mapK e

        ruleNacLeft = buildProduction (e <&> getLHS ruleL) (getRHS ruleL) []
        ruleNacRight = buildProduction (getLHS ruleL) (e <&> getRHS ruleL) []

        mapL = idMap (codomain (getLHS ruleL)) (codomain (getLHS ruleL))
        mapK = idMap (domain (getLHS ruleL)) (domain (getLHS ruleL))
        mapR = idMap (codomain (getRHS ruleL)) (codomain (getRHS ruleL))

getRulesFrom2Rule :: SndOrderRule a b -> (TypedGraphRule a b, TypedGraphRule a b, Production (TypedGraphMorphism a b))
getRulesFrom2Rule sndRule = (codomain (getLHS sndRule), domain (getLHS sndRule), codomain (getRHS sndRule))

calculateAllPartitions :: JointlyEpimorphisms morph => Obj morph -> [morph]
calculateAllPartitions = createAllQuotients

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m

-- | Receives a function that works with a second order and a first order rule.
-- Apply this function on all possible combinations of rules.
applySecondOrder ::
     ((String, SndOrderRule a b) -> (String, TypedGraphRule a b) -> [t])
  -> [(String, TypedGraphRule a b)] -> [(String, SndOrderRule a b)] -> [t]
applySecondOrder f fstRules = concatMap (\r -> concatMap (f r) fstRules)

-- | Applies a named second order rule to a named first order rule with all possible matches,
-- and generates named first order rules as result.
applySndOrderRule :: MorphismsConfig -> (String, SndOrderRule a b) -> (String, TypedGraphRule a b) -> [(String, TypedGraphRule a b)]
applySndOrderRule conf (sndName,sndRule) (fstName,fstRule) =
  let
    matches = findApplicableMatches conf sndRule fstRule
    newRules = map (`rewrite` sndRule) matches
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
  in zip newNames newRules
