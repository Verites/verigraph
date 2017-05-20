module SndOrder.Rule.DPO where

import           Data.Maybe               (fromMaybe, mapMaybe)

import           Abstract.Valid
import           Abstract.Category.AdhesiveHLR
import           Abstract.Category.DPO
import           Object.Graph              as G
import           SndOrder.Morphism        as SO
import           SndOrder.Rule.Core
import           TypedGraph.DPO.GraphRule
import           Object.TypedGraph
import           TypedGraph.Morphism

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
  ( if matchRestriction conf == AnyMatches then
      newNacsPair LeftSide sndRule ++ newNacsPair RightSide sndRule
    else
      []
  )

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
createNacProb :: Side -> GraphRule a b -> NodeOrEdge (Maybe b) -> SO.RuleMorphism a b
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
newNacsPair :: Side -> SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsPair sideChoose sndRule =
  mapMaybe createNac retNodes ++ mapMaybe createNac retEdges
  where
    applyNode = applyNodeIdUnsafe
    applyEdge = applyEdgeIdUnsafe

    (ruleL, ruleK, ruleR) = getRulesFrom2Rule sndRule

    (mapping, getSide) =
      case sideChoose of
        LeftSide  -> (SO.mappingLeft, getLHS)
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

    pairsNodes = pairs applyNode isOrphanNode nodeIds
    pairsEdges = pairs applyEdge isOrphanEdge edgeIds

    epis = calculateAllPartitions (codomain (getSide ruleL))

    filtered apply pairs = [e | e <- epis, any (\(a,b) -> apply e a == apply e b) pairs]

    retNodes = filtered applyNode pairsNodes
    retEdges = filtered applyEdge pairsEdges

    createNac e = if isValid n then Just n else Nothing
      where
        n = case sideChoose of
              LeftSide  -> nLeft
              RightSide -> nRight

        nLeft = SO.ruleMorphism ruleL ruleNacLeft e mapK mapR
        nRight = SO.ruleMorphism ruleL ruleNacRight mapL mapK e

        ruleNacLeft = buildProduction (e <&> getLHS ruleL) (getRHS ruleL) []
        ruleNacRight = buildProduction (getLHS ruleL) (e <&> getRHS ruleL) []

        mapL = idMap (codomain (getLHS ruleL)) (codomain (getLHS ruleL))
        mapK = idMap (domain (getLHS ruleL)) (domain (getLHS ruleL))
        mapR = idMap (codomain (getRHS ruleL)) (codomain (getRHS ruleL))

getRulesFrom2Rule :: SndOrderRule a b -> (Production (TypedGraphMorphism a b), Production (TypedGraphMorphism a b), Production (TypedGraphMorphism a b))
getRulesFrom2Rule sndRule = (codomain (getLHS sndRule), domain (getLHS sndRule), codomain (getRHS sndRule))

calculateAllPartitions :: EpiPairs morph => Obj morph -> [morph]
calculateAllPartitions = createAllSubobjects False

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m
