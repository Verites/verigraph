{-# OPTIONS_GHC -fno-warn-orphans #-}

module SndOrder.Rule.DPO where

import           Data.Maybe           (mapMaybe,fromMaybe)

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Valid
import           Graph.Graph          as G
import           SndOrder.Morphism    as SO
import           TypedGraph.Graph
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

import           SndOrder.Rule.Core

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

  isPartiallyMonomorphic m l =
    isPartiallyMonomorphic (mappingLeft m)      (mappingLeft l)      &&
    isPartiallyMonomorphic (mappingInterface m) (mappingInterface l) &&
    isPartiallyMonomorphic (mappingRight m)     (mappingRight l)



---- Minimal Safety NACs

-- | Configuration for the minimalSafetyNACs algorithms, it defines from
-- which side of the getLHS (second order production) is being analyzed
data Side = LeftSide | RightSide

-- | Either NodeId EdgeId
data NodeOrEdge = Node NodeId | Edge EdgeId deriving (Show)

-- | Adds the minimal safety nacs needed to this production always produce a second order rule.
-- If the nacs that going to be added not satisfies the others nacs, then it do not need to be added.
addMinimalSafetyNacs :: DPOConfig -> SndOrderRule a b -> SndOrderRule a b
addMinimalSafetyNacs conf sndRule =
  buildProduction 
    (getLHS sndRule)
    (getRHS sndRule)
    (getNACs sndRule ++
     filter (satisfiesNACs conf sndRule) (minimalSafetyNacs conf sndRule))

-- | Generates the minimal safety NACs of a 2-rule
minimalSafetyNacs :: DPOConfig -> SndOrderRule a b -> [RuleMorphism a b]
minimalSafetyNacs conf sndRule =
  newNacsProb LeftSide sndRule ++
  newNacsProb RightSide sndRule ++
  (if (matchRestriction conf) == AnyMatches
    then
      (newNacsPair LeftSide sndRule ++
       newNacsPair RightSide sndRule)
    else [])

-- | Generate NACs that forbid deleting elements in L or R but not in K,
-- It discovers how situations must have a NAC and function createNacProb creates them.
-- Insert NACs to avoid condition (a) in Thm 70 (rodrigo machado phd thesis, 2012)
newNacsProb :: Side -> SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide  -> (SO.mappingLeft, getLHS)
        RightSide -> (SO.mappingRight, getRHS)

    applyNode = applyNodeUnsafe
    applyEdge = applyEdgeUnsafe

    ruleL = codomain (getLHS sndRule)
    ruleK = domain (getLHS sndRule)
    ruleR = codomain (getRHS sndRule)

    f = mapSide (getLHS sndRule)
    g = mapSide (getRHS sndRule)

    sa = getSide ruleL
    sb = getSide ruleK
    sc = getSide ruleR
    
    nodeProb = [applyNode f n |
                 n <- nodesFromCodomain sb
               , isOrphanNode sa (applyNode f n)
               , isOrphanNode sb n
               , not (isOrphanNode sc (applyNode g n))]

    edgeProb = [applyEdge f n |
                 n <- edgesFromCodomain sb
               , isOrphanEdge sa (applyEdge f n)
               , isOrphanEdge sb n
               , not (isOrphanEdge sc (applyEdge g n))]

    nacNodes = map (createNacProb side ruleL . Node) nodeProb
    nacEdges = map (createNacProb side ruleL . Edge) edgeProb

-- | Auxiliar function that creates concrectly the NACs for newNacsProb
createNacProb :: Side -> GraphRule a b -> NodeOrEdge -> SO.RuleMorphism a b
createNacProb sideChoose ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
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

    src = G.sourceOfUnsafe (domain otherSideGraph)
    tgt = G.targetOfUnsafe (domain otherSideGraph)

    tpNode = getNodeType otherSideGraph
    tpEdge = getEdgeType otherSideGraph

    typeSrc x = getNodeType otherSideGraph (src x)
    typeTgt x = getNodeType otherSideGraph (tgt x)

    n' = head (newNodes (domain graphK))
    n'' = head (newNodes (domain graphSide))

    e' = head (newEdges (domain graphK))
    e'' = head (newEdges (domain graphSide))

    newNodesK = newNodes (domain graphK)
    newNodesSide = newNodes (domain graphSide)

    invertSide = invert side

    srcInK x = fromMaybe (newNodesK !! 0) (applyNode invertSide (src x))
    tgtInK x = fromMaybe (newNodesK !! 1) (applyNode invertSide (tgt x))
    srcInR x = fromMaybe (newNodesSide !! 0) (applyNode otherSide (srcInK x))
    tgtInR x = fromMaybe (newNodesSide !! 1) (applyNode otherSide (tgtInK x))

    (updateLeft, updateRight) =
      (case x of
         (Node n) -> createNodes n n' n'' (tpNode n)
         (Edge e) -> createEdges e e' e'' (tpEdge e)
                        (src e) (typeSrc e) (srcInK e) (srcInR e)
                        (tgt e) (typeTgt e) (tgtInK e) (tgtInR e))
    
    nacRule = buildProduction updateLeft updateRight []
    mapL = idMap graphL (codomain updateLeft)
    mapK = idMap graphK (domain updateLeft)
    mapR = idMap graphR (codomain updateRight)

    createNodes x x' x'' tp = 
      case sideChoose of
        LeftSide -> (updateSide1, updateSide2Map)
        RightSide -> (updateSide2Map, updateSide1)
      where
        updateSide1 = createNodeOnDomain x' tp x side
        updateSide2Cod = createNodeOnCodomain x'' tp otherSide
        updateSide2Map = updateNodeRelation x' x'' tp updateSide2Cod

    createEdges x x' x'' tp
        src typeSrc srcInK srcInR
        tgt typeTgt tgtInK tgtInR =
      case sideChoose of
        LeftSide -> (updateLeftEdge, updateRightMap)
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
newNacsPair :: Side -> SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsPair sideChoose sndRule =
  mapMaybe createNac retNodes ++ mapMaybe createNac retEdges
  where
    applyNode = applyNodeUnsafe
    applyEdge = applyEdgeUnsafe
    
    ruleL = codomain (getLHS sndRule)
    ruleK = domain (getLHS sndRule)
    ruleR = codomain (getRHS sndRule)
    
    (mapping, getSide) =
      case sideChoose of
        LeftSide -> (SO.mappingLeft, getLHS)
        RightSide -> (SO.mappingRight, getRHS)
    
    fl = mapping (getLHS sndRule)
    gl = mapping (getRHS sndRule)
    
    lb = getSide ruleK
    lc = getSide ruleR
    
    pairs apply isOrphan list =
      [(apply fl x, apply fl y) |
          x <- list $ domain $ codomain lb
        , y <- list $ domain $ codomain lb
        , x /= y
        , isOrphan lb x
        , not (isOrphan lc (apply gl x))
        , not (isOrphan lc (apply gl y))]
    
    pairsNodes = pairs applyNode isOrphanNode nodes
    pairsEdges = pairs applyEdge isOrphanEdge edges
    
    epis = calculateAllPartitions (codomain (getSide ruleL))
    
    filtered apply pairs = [e | e <- epis, any (\(a,b) -> (apply e) a == (apply e) b) pairs]
    
    retNodes = filtered applyNode pairsNodes
    retEdges = filtered applyEdge pairsEdges
    
    createNac e = if isValid n then Just n else Nothing
      where
        n = case sideChoose of
              LeftSide -> nLeft
              RightSide -> nRight
        
        nLeft = SO.ruleMorphism ruleL ruleNacLeft e mapK mapR
        nRight = SO.ruleMorphism ruleL ruleNacRight mapL mapK e
        
        ruleNacLeft = buildProduction (compose (getLHS ruleL) e) (getRHS ruleL) []
        ruleNacRight = buildProduction (getLHS ruleL) (compose (getRHS ruleL) e) []
        
        mapL = idMap (codomain (getLHS ruleL)) (codomain (getLHS ruleL))
        mapK = idMap (domain (getLHS ruleL)) (domain (getLHS ruleL))
        mapR = idMap (codomain (getRHS ruleL)) (codomain (getRHS ruleL))


calculateAllPartitions :: EpiPairs m => Obj m -> [m]
calculateAllPartitions graph = createAllSubobjects False graph

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodes m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdges m
