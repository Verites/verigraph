{-# LANGUAGE TypeFamilies #-}

module Graph.SndOrderRule (
    SndOrderRule
  , minimalSafetyNacs
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
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

-- | Generates the minimal safety NACs of a 2-rule
-- probL and probR done, pairL and pairR to do.
minimalSafetyNacs :: SndOrderRule a b -> SndOrderRule a b
minimalSafetyNacs sndRule =
  production
    (left sndRule)
    (right sndRule)
    ((nacs sndRule) ++
     (newNacsProb LeftSide sndRule) ++
     (newNacsProb RightSide sndRule){- ++
     (newNacsPairL sndRule)-})

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
    
    nacNodes = map (\x -> createNacNodeProb side ruleL x) nodeProb
    nacEdges = map (\x -> createNacEdgeProb side ruleL x) edgeProb

createNacNodeProb :: Side -> GraphRule a b -> NodeId -> SO.RuleMorphism a b
createNacNodeProb side ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where    
    l = left ruleL
    r = right ruleL
    
    graphL = codomain l
    graphK = domain l
    graphR = codomain r
    
    tp = GM.applyNodeUnsafe graphL x
    
    (graphSide, side1, side2) =
      case side of
        LeftSide -> (graphR, l, r)
        RightSide -> (graphL, r, l)
    
    x' = head (newNodes (domain graphK))
    x'' = head (newNodes (domain graphSide))
    
    updateSide1 = createNodeDomTGM x' tp x side1
    updateSide2Cod = createNodeCodTGM x'' tp side2
    updateSide2Map = updateNodeRelationTGM x' x'' tp updateSide2Cod
    
    nacRule = production updateSide1 updateSide2Map []
    mapL = idMap graphL (codomain updateSide1)
    mapK = idMap graphK (domain updateSide1)
    mapR = idMap graphR (codomain updateSide2Map)

createNacEdgeProb :: Side -> GraphRule a b -> EdgeId -> SO.RuleMorphism a b
createNacEdgeProb side ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where
    l = left ruleL
    r = right ruleL
    
    graphL = codomain l
    graphK = domain l
    graphR = codomain r
    
    src = G.sourceOfUnsafe (domain graphL) x
    tgt = G.targetOfUnsafe (domain graphL) x
    
    tp = GM.applyEdgeUnsafe graphL x
    
    (graphSide, side1, side2) =
      case side of
        LeftSide -> (graphR, l, r)
        RightSide -> (graphL, r, l)
    
    typeSrc = GM.applyNodeUnsafe graphL src
    typeTgt = GM.applyNodeUnsafe graphL tgt
    
    x' = head (newEdges (domain graphK))
    x'' = head (newEdges (domain graphSide))
    
    newNodesK = newNodes (domain graphK)
    newNodesSide = newNodes (domain graphSide)
    
    invertSide1 = invertTGM side1
    
    srcInK = case applyNodeTGM invertSide1 src of
                    Just x -> x
                    Nothing -> newNodesK !! 0
    tgtInK = case applyNodeTGM invertSide1 tgt of
                    Just x -> x
                    Nothing -> newNodesK !! 1
    
    srcInR = case applyNodeTGM side2 srcInK of
                    Just x -> x
                    Nothing -> newNodesSide !! 0
    tgtInR = case applyNodeTGM side2 tgtInK of
                    Just x -> x
                    Nothing -> newNodesSide !! 1
    
    srcRight = createNodeCodTGM srcInR typeSrc side2
    tgtRight = createNodeCodTGM tgtInR typeTgt srcRight
    updateRight = createNodeDomTGM srcInK typeSrc srcInR tgtRight
    updateRight2 = createNodeDomTGM tgtInK typeTgt tgtInR updateRight
    updateRightCod = createEdgeCodTGM x'' srcInR tgtInR tp updateRight2
    updateRightMap = createEdgeDomTGM x' srcInK tgtInK tp x'' updateRightCod
    
    updateLeft = createNodeDomTGM srcInK typeSrc src side1
    updateLeft2 = createNodeDomTGM tgtInK typeTgt tgt updateLeft    
    updateLeftEdge = createEdgeDomTGM x' srcInK tgtInK tp x updateLeft2
    
    nacRule = production updateLeftEdge updateRightMap []
    mapL = idMap graphL (codomain updateLeft)
    mapK = idMap graphK (domain updateLeft)
    mapR = idMap graphR (codomain updateRightMap)

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
