{-# OPTIONS_GHC -fno-warn-orphans #-}

module SndOrder.Rule (
    SndOrderRule
  , addMinimalSafetyNacs
  , applySndOrderRule
  , applySecondOrder
  ) where

import           Data.Maybe           (fromMaybe)

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Graph.Graph          as G
import qualified Graph.GraphMorphism  as GM
import           SndOrder.Morphism    as SO
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

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

-- | Receives a function that works with a second order and a first order rule.
-- Apply this function on all possible combinations of rules.
applySecondOrder ::
     ((String, SndOrderRule a b) -> (String, GraphRule a b) -> [t])
  -> [(String, GraphRule a b)] -> [(String, SndOrderRule a b)] -> [t]
applySecondOrder f fstRules = concatMap (\r -> applySecondOrderListRules f r fstRules)

applySecondOrderListRules ::
    ((String, SndOrderRule a b) -> (String, GraphRule a b) -> [t])
 -> (String, SndOrderRule a b) -> [(String, GraphRule a b)] -> [t]
applySecondOrderListRules f sndRule = concatMap (f sndRule)

instance DPO (RuleMorphism a b) where
  invertProduction config r = addMinimalSafetyNacs config newRule
    where
      newRule = constructProduction (getRHS r) (getLHS r) (concatMap (shiftNacOverProduction config r) (getNACs r))

  -- | Needs the satisfiesNACs extra verification because not every satisfiesGluingConditions nac can be shifted
  shiftNacOverProduction config rule n =
    [calculateComatch n rule |
      satisfiesGluingConditions config rule n &&
      satisfiesNACs config ruleWithOnlyMinimalSafetyNacs n]

    where
      ruleWithOnlyMinimalSafetyNacs = constructProduction (getLHS rule) (getRHS rule) (minimalSafetyNacs rule)

  partiallyMonomorphic m l =
    partiallyMonomorphic (mappingLeft m)      (mappingLeft l)      &&
    partiallyMonomorphic (mappingInterface m) (mappingInterface l) &&
    partiallyMonomorphic (mappingRight m)     (mappingRight l)

applySndOrderRule :: DPOConfig -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule config (sndName,sndRule) (fstName,fstRule) =
  let
    matches =
      findApplicableMatches config sndRule fstRule

    newRules =
      map (`rewrite` sndRule) matches

    newNames =
      map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
  in
    zip newNames newRules

-- *** Minimal Safety Nacs

-- | Adds the minimal safety nacs needed to this production always produce a second order rule.
-- If the nacs to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: DPOConfig -> SndOrderRule a b -> SndOrderRule a b
addMinimalSafetyNacs nacInj sndRule =
  constructProduction
    (getLHS sndRule)
    (getRHS sndRule)
    (getNACs sndRule ++
     filter (satisfiesNACs nacInj sndRule) (minimalSafetyNacs sndRule))

data Side = LeftSide | RightSide

-- | Generates the minimal safety NACs of a 2-rule.
-- probL and probR done, pairL and pairR to do.
minimalSafetyNacs :: SndOrderRule a b -> [RuleMorphism a b]
minimalSafetyNacs sndRule = newNacsProb LeftSide sndRule ++ newNacsProb RightSide sndRule

newNacsProb :: Side -> SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide -> (SO.mappingLeft, getLHS)
        RightSide -> (SO.mappingRight, getRHS)

    applyNode = applyNodeTGMUnsafe
    applyEdge = applyEdgeTGMUnsafe

    ruleL = codomain (getLHS sndRule)
    ruleK = domain (getLHS sndRule)
    ruleR = codomain (getRHS sndRule)

    f = mapSide (getLHS sndRule)
    g = mapSide (getRHS sndRule)

    sa = getSide ruleL
    sb = getSide ruleK
    sc = getSide ruleR

    nodeProb = [applyNode f n |
                 n <- nodesCodomain sb
               , orphanNode sa (applyNode f n)
               , orphanNode sb n
               , not (orphanNode sc (applyNode g n))]

    edgeProb = [applyEdge f n |
                 n <- edgesCodomain sb
               , orphanEdge sa (applyEdge f n)
               , orphanEdge sb n
               , not (orphanEdge sc (applyEdge g n))]

    nacNodes = map (createNacProb side ruleL . Left) nodeProb
    nacEdges = map (createNacProb side ruleL . Right) edgeProb

createNacProb :: Side -> GraphRule a b -> Either NodeId EdgeId -> SO.RuleMorphism a b
createNacProb sideChoose ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where
    l = getLHS ruleL
    r = getRHS ruleL

    graphL = codomain l
    graphK = domain l
    graphR = codomain r

    src = G.sourceOfUnsafe (domain graphL)
    tgt = G.targetOfUnsafe (domain graphL)

    tpNode = GM.applyNodeUnsafe graphL
    tpEdge = GM.applyEdgeUnsafe graphL

    (graphSide, side, otherSide) =
      case sideChoose of
        LeftSide -> (graphR, l, r)
        RightSide -> (graphL, r, l)

    typeSrc x = GM.applyNodeUnsafe graphL (src x)
    typeTgt x = GM.applyNodeUnsafe graphL (tgt x)

    n' = head (newNodes (domain graphK))
    n'' = head (newNodes (domain graphSide))

    e' = head (newEdges (domain graphK))
    e'' = head (newEdges (domain graphSide))

    newNodesK = newNodes (domain graphK)
    newNodesSide = newNodes (domain graphSide)

    invertSide = invertTGM side

    srcInK x = fromMaybe (newNodesK !! 0) (applyNodeTGM invertSide (src x))
    tgtInK x = fromMaybe (newNodesK !! 1) (applyNodeTGM invertSide (tgt x))
    srcInR x = fromMaybe (newNodesSide !! 0) (applyNodeTGM otherSide (srcInK x))
    tgtInR x = fromMaybe (newNodesSide !! 1) (applyNodeTGM otherSide (tgtInK x))

    (updateLeft, updateRight) =
      (case x of
         (Left n) -> createNodes n n' n'' (tpNode n)
         (Right e) -> createEdges e e' e'' (tpEdge e)
                        (src e) (typeSrc e) (srcInK e) (srcInR e)
                        (tgt e) (typeTgt e) (tgtInK e) (tgtInR e))
        side otherSide

    nacRule = constructProduction updateLeft updateRight []
    mapL = idMap graphL (codomain updateLeft)
    mapK = idMap graphK (domain updateLeft)
    mapR = idMap graphR (codomain updateRight)

    createNodes x x' x'' tp side otherSide = (updateSide1, updateSide2Map)
      where
        updateSide1 = createNodeDomTGM x' tp x side
        updateSide2Cod = createNodeCodTGM x'' tp otherSide
        updateSide2Map = updateNodeRelationTGM x' x'' tp updateSide2Cod

    createEdges x x' x'' tp
        src typeSrc srcInK srcInR
        tgt typeTgt tgtInK tgtInR
        side otherSide = (updateLeftEdge, updateRightMap)
      where
        srcRight = createNodeCodTGM srcInR typeSrc otherSide
        tgtRight = createNodeCodTGM tgtInR typeTgt srcRight
        updateRight = createNodeDomTGM srcInK typeSrc srcInR tgtRight
        updateRight2 = createNodeDomTGM tgtInK typeTgt tgtInR updateRight
        updateRightCod = createEdgeCodTGM x'' srcInR tgtInR tp updateRight2
        updateRightMap = createEdgeDomTGM x' srcInK tgtInK tp x'' updateRightCod

        updateLeft = createNodeDomTGM srcInK typeSrc src side
        updateLeft2 = createNodeDomTGM tgtInK typeTgt tgt updateLeft
        updateLeftEdge = createEdgeDomTGM x' srcInK tgtInK tp x updateLeft2

orphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
orphanNode m n = n `elem` orphanNodesTyped m

orphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
orphanEdge m n = n `elem` orphanEdgesTyped m
