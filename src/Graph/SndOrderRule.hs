{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graph.SndOrderRule (
    SndOrderRule
  , applySndOrderRules
  , addMinimalSafetyNacs
  , minimalSafetyNacs
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.EpiPairs ()
import           Graph.Graph as G
import           Graph.GraphRule
import qualified Graph.GraphMorphism as GM
import           Graph.RuleMorphism as SO
import           Graph.TypedGraphMorphism

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
--
type SndOrderRule a b = Production (RuleMorphism a b)

data Side = LeftSide | RightSide

instance DPO (RuleMorphism a b) where
  satsGluing inj m l =
    satsGluing inj (mappingLeft m)      (mappingLeft l)      &&
    satsGluing inj (mappingInterface m) (mappingInterface l) &&
    satsGluing inj (mappingRight m)     (mappingRight l)     &&
    danglingSpan (left (codomain m)) (mappingLeft m) (mappingInterface m) (mappingLeft l) (mappingInterface l) &&
    danglingSpan (right (codomain m)) (mappingRight m) (mappingInterface m) (mappingRight l) (mappingInterface l)
  
  -- CHECK
  freeDanglingEdges _ _ = True
  
  inverse nacInj inj r = addMinimalSafetyNacs newRule
    where
      newRule = production (right r) (left r) (concatMap (shiftLeftNac nacInj inj r) (nacs r))
  
  -- | Needs the satsNacs extra verification because not every satsGluing nac can be shifted
  shiftLeftNac nacInj inj rule n = [comatch n rule |
                               satsGluing inj n (left rule) &&
                               satsNacs nacInj inj ruleWithOnlyMinimalSafetyNacs n]
    where
      ruleWithOnlyMinimalSafetyNacs = production (left rule) (right rule) (minimalSafetyNacs rule)
  
  partiallyMonomorphic m l =
    partiallyMonomorphic (mappingLeft m)      (mappingLeft l)      &&
    partiallyMonomorphic (mappingInterface m) (mappingInterface l) &&
    partiallyMonomorphic (mappingRight m)     (mappingRight l)

-- | A gluing condition for second order rules application 
danglingSpan :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
danglingSpan matchRuleSide matchMorp matchK l k = deletedNodesInK && deletedEdgesInK
  where
    deletedNodes = filter (ruleDeletes l matchMorp applyNodeTGM nodesDomain) (nodesCodomain matchMorp)
    nodesInK = [a | a <- nodesDomain matchRuleSide, (applyNodeTGMUnsafe matchRuleSide a) `elem` deletedNodes]
    deletedNodesInK = all (ruleDeletes k matchK applyNodeTGM nodesDomain) nodesInK
    
    deletedEdges = filter (ruleDeletes l matchMorp applyEdgeTGM edgesDomain) (edgesCodomain matchMorp)
    edgesInK = [a | a <- edgesDomain matchRuleSide, (applyEdgeTGMUnsafe matchRuleSide a) `elem` deletedEdges]
    deletedEdgesInK = all (ruleDeletes k matchK applyEdgeTGM edgesDomain) edgesInK

applySndOrderRules :: [(String, GraphRule a b)] -> [(String, SndOrderRule a b)] -> [(String, GraphRule a b)]
applySndOrderRules fstRules = concatMap (\r -> applySndOrderRuleListRules r fstRules)

applySndOrderRuleListRules :: (String, SndOrderRule a b) -> [(String, GraphRule a b)] -> [(String, GraphRule a b)]
applySndOrderRuleListRules sndRule = concatMap (applySndOrderRule sndRule)

applySndOrderRule :: (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule (sndName,sndRule) (fstName,fstRule) = zip newNames newRules
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    leftRule = left sndRule
    rightRule = right sndRule
    mats = matches MONO (codomain leftRule) fstRule
    gluing = filter (\m -> satsGluing False m leftRule) mats
    nacs = filter (satsNacs True False sndRule) gluing
    newRules = map
                 (\match ->
                   let (k,_)  = poc match leftRule
                       (m',_) = po k rightRule in
                       codomain m'
                   ) nacs

-- | Adds the minimal safety nacs needed to this production always produce a second order rule.
-- If the nacs to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: SndOrderRule a b -> SndOrderRule a b
addMinimalSafetyNacs sndRule =
  production
    (left sndRule)
    (right sndRule)
    ((nacs sndRule) ++
     (filter (satsNacs True True sndRule) (minimalSafetyNacs sndRule)))

-- | Generates the minimal safety NACs of a 2-rule
-- probL and probR done, pairL and pairR to do.
minimalSafetyNacs :: SndOrderRule a b -> [RuleMorphism a b]
minimalSafetyNacs sndRule = (newNacsProb LeftSide sndRule) ++ (newNacsProb RightSide sndRule)

newNacsProb :: Side -> SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide -> (SO.mappingLeft, left)
        RightSide -> (SO.mappingRight, right)
    
    applyNode = applyNodeTGMUnsafe
    applyEdge = applyEdgeTGMUnsafe
    
    ruleL = codomain (left sndRule)
    ruleK = domain (left sndRule)
    ruleR = codomain (right sndRule)
    
    f = mapSide (left sndRule)
    g = mapSide (right sndRule)
    
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
    
    nacNodes = map (\x -> createNacProb side ruleL (Left x)) nodeProb
    nacEdges = map (\x -> createNacProb side ruleL (Right x)) edgeProb

createNacProb :: Side -> GraphRule a b -> Either NodeId EdgeId -> SO.RuleMorphism a b
createNacProb sideChoose ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where    
    l = left ruleL
    r = right ruleL
    
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
    
    srcInK x = case applyNodeTGM invertSide (src x) of {Just y -> y; Nothing -> newNodesK !! 0}
    tgtInK x = case applyNodeTGM invertSide (tgt x) of {Just y -> y; Nothing -> newNodesK !! 1}
    srcInR x = case applyNodeTGM otherSide (srcInK x) of {Just y -> y; Nothing -> newNodesSide !! 0}
    tgtInR x = case applyNodeTGM otherSide (tgtInK x) of {Just y -> y; Nothing -> newNodesSide !! 1}
    
    (updateLeft, updateRight) =
      (case x of 
         (Left n) -> createNodes n n' n'' (tpNode n)
         (Right e) -> createEdges e e' e'' (tpEdge e)
                        (src e) (typeSrc e) (srcInK e) (srcInR e)
                        (tgt e) (typeTgt e) (tgtInK e) (tgtInR e))
        side otherSide
    
    nacRule = production updateLeft updateRight []
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

{-newNacsPairL :: SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsPairL sndRule = map createNac ret
  where
    apply = applyNodeTGMUnsafe
    
    ruleL = codomain (left sndRule)
    ruleK = domain (left sndRule)
    ruleR = codomain (right sndRule)
    
    fl = SO.mappingLeft (left sndRule)
    gl = SO.mappingLeft (right sndRule)
    
    lb = left ruleK
    lc = left ruleR
    
    pairL = [(apply fl x, apply fl y) |
                     x <- nodesCodomain lb
                   , y <- nodesCodomain lb
                   , x /= y
                   , orphanNode lb x
                   , not (orphanNode lc (apply gl x))
                   , not (orphanNode lc (apply gl y))]
    
    epis = calculateAllPartitions (codomain (left ruleL))
    
    ret = [e | e <- epis, any (\(a,b) -> (apply e) a == (apply e) b) pairL]
    
    createNac e = SO.ruleMorphism ruleL ruleNac e mapK mapR
      where
        ruleNac = production (compose (left ruleL) e) (right ruleL) []
        mapK = idMap (domain (left ruleL)) (domain (left ruleL))
        mapR = idMap (codomain (right ruleL)) (codomain (right ruleL))

calculateAllPartitions :: GM.TypedGraph a b -> [TypedGraphMorphism a b]
calculateAllPartitions graph = map fst (createPairs inj graph graphNull)
  where
    inj = False
    graphNull = GM.empty G.empty G.empty-}

orphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
orphanNode m n = n `elem` (orphanNodesTyped m)

orphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
orphanEdge m n = n `elem` (orphanEdgesTyped m)
