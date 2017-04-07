{-|
Module      : InterLevelCP
Description : Implements the inter-level critical pairs
Stability   : development
-}

module Analysis.Interlevel.InterLevelCP (interLevelCP, InterLevelCP(..)) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Data.List                (nubBy)
import           Graph.Graph
import           Graph.GraphMorphism      hiding (createEdgeOnCodomain,
                                           createNodeOnCodomain)
import           SndOrder.Morphism
import           SndOrder.Rule
import           TypedGraph.DPO.GraphRule
import           TypedGraph.Graph
import           TypedGraph.Morphism
import           TypedGraph.Subgraph


data InterLevelCP a b = InterLevelCP {
  sndOrderMatch :: RuleMorphism a b,
  fstOrderMatch :: TypedGraphMorphism a b
  } deriving (Eq,Show)

-- | Matches the second order rule with the first order, and calls theirs critical pairs
interLevelCP :: MorphismsConfig -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String,String,Int,InterLevelCP a b)]
interLevelCP conf (sndName, sndRule) (fstName, fstRule) =
  map (\((x,y,z),w) -> (x,y,z,w)) unformattedConflicts

  where
    newNames = map (\number -> (fstName, sndName, number)) ([0..] :: [Int])
    unformattedConflicts = zip newNames (concatMap conflictsForMatch validMatches)

    validMatches = findApplicableMatches conf sndRule fstRule

    conflictsForMatch match =
      do
        conflicts <- interLevelConflictOneMatch conf sndRule match
        return $ InterLevelCP match conflicts

-- | Calculates the second order rewriting,
-- defines the dangling extension for L and L'',
-- gets all relevant graphs from L
interLevelConflictOneMatch :: MorphismsConfig -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch conf sndRule match = m0s
  where
    sndOrderL = getLHS sndRule
    sndOrderR = getRHS sndRule

    (k,l') = calculatePushoutComplement match sndOrderL
    (m',r') = calculatePushout k sndOrderR

    p = codomain match
    p'' = codomain m'

    bigL = getLHS p
    bigL'' = getLHS p''

    fl = mappingLeft l'
    gl = mappingLeft r'

    danglingExtFl = compose fl (danglingExtension fl bigL)
    danglingExtGl = compose gl (danglingExtension gl bigL'')

    axs = relevantMatches conf danglingExtFl danglingExtGl
    relevantGraphs = map codomain axs

    defineMatches = allILCP conf p p'' fl gl

    m0s = concatMap defineMatches (removeDuplicated relevantGraphs)

removeDuplicated :: [GraphMorphism (Maybe a) (Maybe b)] -> [GraphMorphism (Maybe a) (Maybe b)]
removeDuplicated = nubBy (\x y -> not $ Prelude.null $ find x y)
  where
    find :: GraphMorphism (Maybe a) (Maybe b) -> GraphMorphism (Maybe a) (Maybe b) -> [TypedGraphMorphism a b]
    find = findMorphisms Isomorphism

-- | For a relevant graph, gets all matches and check conflict
allILCP :: DPO m => MorphismsConfig -> Production m -> Production m -> m -> m -> Obj m -> [m]
allILCP conf p p'' fl gl ax = filter conflicts validMatches
  where
    validMatches = findApplicableMatches conf p ax
    conflicts = ilCP conf fl gl p''

-- | For a m0, checks if exists a conflicting m''0
ilCP :: DPO m => MorphismsConfig -> m -> m -> Production m -> m -> Bool
ilCP conf fl gl p'' m0 = Prelude.null validM0''-- or all (==False) (map (\m'' -> satsGluing inj bigL'' m'') validM0'') --thesis def
  where
    matchesM0'' = findApplicableMatches conf p'' (codomain m0)
    validMatch = satisfiesRewritingConditions conf p''

    commutes m0'' = compose fl m0 == compose gl m0''

    --paper definition
    --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
    validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantMatches :: MorphismsConfig -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
--relevantMatches inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantMatches conf dangFl dangGl = concatMap (createAllSubobjects matchInjective) axs
  where
    matchInjective = matchRestriction conf == MonoMatches
    (_,al) = calculatePushout dangFl dangGl
    --axs = induzedSubgraphs al
    axs = subgraphs (codomain al)

danglingExtension :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
danglingExtension gl l = tlUpdated
  where
    ld = orphanTypedNodes l
    tl = codomain l
    t = codomain tl
    tlx n' = any (\n -> extractNodeType tl n == n') ld
    dangT = filter (\e -> tlx (sourceOfUnsafe t e) || tlx (targetOfUnsafe t e)) (edgeIds t)

    edgesToAdd = concatMap (\n -> map (\e -> (n,e)) dangT) ld

    tlUpdated = foldl addEdge gl edgesToAdd

    addEdge tgm (n,e) =
      case (isSrc,isTgt) of
        (True,True) -> createSrcTgt tgm e n
        (True,False) -> createSrc tgm e n
        (False,True) -> createTgt tgm e n
        (False,False) -> error "danglingExtension: it's not supposed to be here"
      where
        typeNode = extractNodeType (codomain tgm) n
        isSrc = typeNode == sourceOfUnsafe t e
        isTgt = typeNode == targetOfUnsafe t e

        createEdge tgm e s t = createEdgeOnCodomain edgeId s t e tgm
          where
            edgeId = head (newTypedEdges (codomain tgm))

        createSrcTgt tgm e n = createTgt (createSrc tgm e n) e n

        createSrc tgm e n = createEdge newGraph e n nodeId
          where
            nodeId = head (newTypedNodes (codomain tgm))
            typeNewNode = targetOfUnsafe (codomain (codomain tgm)) e
            newGraph = createNodeOnCodomain nodeId typeNewNode tgm

        createTgt tgm e n = createEdge newGraph e nodeId n
          where
            nodeId = head (newTypedNodes (codomain tgm))
            typeNewNode = sourceOfUnsafe (codomain (codomain tgm)) e
            newGraph = createNodeOnCodomain nodeId typeNewNode tgm
