{-# LANGUAGE TypeFamilies #-}
module Rewriting.DPO.TypedGraphRule
  ( SndOrderRule
  , Production(..)
  , leftObject
  , interfaceObject
  , rightObject

  , MorphismsConfig(..)
  , toFstOrderMorphismsConfig
  , toSndOrderMorphismsConfig

  -- * Minimal Safety NACs
  -- TODO: document why minimal safety NACs exist
  , minimalSafetyNacs
  , addMinimalSafetyNacs
  ) where

import           Data.Maybe                            (fromMaybe, mapMaybe)

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.Finitary
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraphRule
import           Category.TypedGraphRule.Adhesive      (createSideRule)
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

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
type SndOrderRule a b = Production (RuleMorphism a b)

toFstOrderMorphismsConfig :: MorphismsConfig (RuleMorphism a b) -> MorphismsConfig (TypedGraphMorphism a b)
toFstOrderMorphismsConfig (MorphismsConfig cls) = MorphismsConfig (toFstOrderMorphismClass cls)

toSndOrderMorphismsConfig :: MorphismsConfig (TypedGraphMorphism a b) -> MorphismsConfig (RuleMorphism a b)
toSndOrderMorphismsConfig (MorphismsConfig cls) = MorphismsConfig (toSndOrderMorphismClass cls)


instance DPO (RuleMorphism a b) where
  invertProduction conf r = addMinimalSafetyNacs conf newRule
    where
      newRule = Production (rightMorphism r) (leftMorphism r)
                 (concatMap (shiftNacOverProduction conf r) (nacs r))

  -- | Needs the satisfiesNACs extra verification because
  -- not every satisfiesGluingConditions nac can be shifted
  shiftNacOverProduction conf rule n =
    [calculateComatch n rule |
      satisfiesGluingConditions conf rule n &&
      satisfiesNACs conf ruleWithOnlyMinimalSafetyNacs n]
    where
      ruleWithOnlyMinimalSafetyNacs =
        rule { nacs = minimalSafetyNacs conf rule }

  createJointlyEpimorphicPairsFromNAC conf' ruleR nac = ret
    where
      conf = toFstOrderMorphismsConfig conf'
      createJointly x = createJointlyEpimorphicPairsFromNAC conf (codomain x)

      nL = mappingLeft nac
      nK = mappingInterface nac
      nR = mappingRight nac
      leftR = leftMorphism ruleR
      rightR = rightMorphism ruleR
      rK = interfaceObject ruleR
      codNac = codomain nac

      interfaceEpiPairs = createJointlyEpimorphicPairsFromNAC conf rK nK

      lefts = concatMap
                (\(kR,kN) ->
                  let ls = createSideRule createJointly kR leftR leftR kN (leftMorphism codNac) nL
                  in map (\(ll1,ll2,m) -> (kR, kN, ll1, ll2, m)) ls)
                interfaceEpiPairs

      rights = concatMap
                (\(kR,kN,ll1,ll2,l) ->
                  let rs = createSideRule createJointly kR rightR rightR kN (rightMorphism codNac) nR
                  in map (\(rr1,rr2,r) -> (kR,kN,ll1,ll2,l,rr1,rr2,r)) rs)
                lefts

      transposeNACs l = map (snd . calculatePushoutAlongM l)

      ret = map (\(k1,k2,ll1,ll2,l,r1,r2,r) ->
                   let rule = Production l r $ transposeNACs ll1 (nacs ruleR) ++ transposeNACs ll2 (nacs codNac)
                   in (ruleMorphism ruleR rule ll1 k1 r1,
                       ruleMorphism (codomain nac) rule ll2 k2 r2)) rights

---- Minimal Safety NACs

-- | Configuration for the minimalSafetyNACs algorithms, it defines from
-- which side of the leftMorphism (second-order production) is being analyzed
data Side = LeftSide | RightSide

-- | Either: Node_ is only a NodeId | Edge_ is an Edge with: EdgeId plus source and target NodeIds
data NodeOrEdge b = Node_ NodeId | Edge_ (Edge b) deriving (Show)

-- | Adds the minimal safety nacs needed to this production always produce a second-order rule.
-- If the nacs that going to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: MorphismsConfig (RuleMorphism a b) -> SndOrderRule a b -> SndOrderRule a b
addMinimalSafetyNacs conf sndRule =
  sndRule { nacs = nacs sndRule ++ filter (satisfiesNACs conf sndRule) (minimalSafetyNacs conf sndRule) }

-- | Generates the minimal safety NACs of a 2-rule
minimalSafetyNacs :: MorphismsConfig (RuleMorphism a b) -> SndOrderRule a b -> [RuleMorphism a b]
minimalSafetyNacs conf sndRule =
  newNacsProb LeftSide sndRule ++
  newNacsProb RightSide sndRule ++
  ( if matchRestriction conf `isSubclassOf` monic then
      []
    else
      newNacsPair LeftSide sndRule ++ newNacsPair RightSide sndRule
  )

-- | Generate NACs that forbid deleting elements in L or R but not in K,
-- It discovers how situations must have a NAC and function createNacProb creates them.
-- Insert NACs to avoid condition (a) in Thm 70 (rodrigo machado phd thesis, 2012)
newNacsProb :: Side -> SndOrderRule a b -> [RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide  -> (mappingLeft, leftMorphism)
        RightSide -> (mappingRight, rightMorphism)

    (ruleL, ruleK, ruleR) = getRulesFrom2Rule sndRule

    f = mapSide (leftMorphism sndRule)
    g = mapSide (rightMorphism sndRule)

    sa = getSide ruleL
    sb = getSide ruleK
    sc = getSide ruleR

    nodeProb = [applyNodeIdUnsafe f n |
                 n <- nodeIds (codomain sb)
               , isOrphanNode sa (applyNodeIdUnsafe f n)
               , isOrphanNode sb n
               , not (isOrphanNode sc (applyNodeIdUnsafe g n))]

    edgeProb = [applyEdgeUnsafe f e |
                 (e, _) <- edges (codomain sb)
               , isOrphanEdge sa (applyEdgeIdUnsafe f (edgeId e))
               , isOrphanEdge sb (edgeId e)
               , not (isOrphanEdge sc (applyEdgeIdUnsafe g (edgeId e)))]

    nacNodes = map (createNacProb side ruleL . Node_) nodeProb
    nacEdges = map (createNacProb side ruleL . Edge_) edgeProb

-- | Auxiliar function that creates concrectly the NACs for newNacsProb
createNacProb :: Side -> TypedGraphRule a b -> NodeOrEdge (Maybe b) -> RuleMorphism a b
createNacProb sideChoose ruleL x = ruleMorphism ruleL nacRule mapL mapK mapR
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

    n' = head (newNodes graphK)
    n'' = head (newNodes graphSide)

    e' = head (newEdges graphK)
    e'' = head (newEdges graphSide)

    newNodesK = newNodes graphK
    newNodesSide = newNodes graphSide

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

    nacRule = Production updateLeft updateRight []
    mapL = makeInclusion graphL (codomain updateLeft)
    mapK = makeInclusion graphK (domain updateLeft)
    mapR = makeInclusion graphR (codomain updateRight)

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
        LeftSide  -> (mappingLeft, leftMorphism)
        RightSide -> (mappingRight, rightMorphism)

    fl = mapping (leftMorphism sndRule)
    gl = mapping (rightMorphism sndRule)

    lb = getSide ruleK
    lc = getSide ruleR

    --pairsNodes = pairs applyNode isOrphanNode nodeIds
    pairsNodes =
      [(applyNode fl x, applyNode fl y) |
          x <- nodeIds $ codomain lb
        , y <- nodeIds $ codomain lb
        , x /= y
        , isOrphanNode lb x
        , not (isOrphanNode lc (applyNode gl x))
        , not (isOrphanNode lc (applyNode gl y))]


    --pairsEdges = pairs applyEdge isOrphanEdge edgeIds
    pairsEdges =
      [(applyEdge fl x, applyEdge fl y) |
          x <- edgeIds $ codomain lb
        , y <- edgeIds $ codomain lb
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

        ruleNacLeft = Production (e <&> leftMorphism ruleL) (rightMorphism ruleL) []
        ruleNacRight = Production (leftMorphism ruleL) (e <&> rightMorphism ruleL) []

        mapL = makeInclusion (leftObject ruleL) (leftObject ruleL)
        mapK = makeInclusion (interfaceObject ruleL) (interfaceObject ruleL)
        mapR = makeInclusion (rightObject ruleL) (rightObject ruleL)

getRulesFrom2Rule :: SndOrderRule a b -> (TypedGraphRule a b, TypedGraphRule a b, Production (TypedGraphMorphism a b))
getRulesFrom2Rule sndRule = (leftObject sndRule, interfaceObject sndRule, rightObject sndRule)

calculateAllPartitions :: ECofinitary morph => Obj morph -> [morph]
calculateAllPartitions = findAllQuotientsOf

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m
