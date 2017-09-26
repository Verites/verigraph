{-|
Module      : InterLevelCP
Description : Implements the inter-level critical pairs
-}
module Analysis.Interlevel.InterLevelCP
  ( interLevelCP,
    InterLevelCP(..),
    danglingExtension
  ) where

import           Data.List                             (nubBy)

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Limit
import           Abstract.Category.Finitary
import           Abstract.Rewriting.DPO
import           Category.TypedGraphRule
import qualified Data.Graphs as G
import           Data.Graphs.Morphism                  hiding (createEdgeOnCodomain,
                                                        createNodeOnCodomain)
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Data.TypedGraph.Subgraph
import           Rewriting.DPO.TypedGraphRule


data InterLevelCP a b = InterLevelCP {
  sndOrderMatch :: RuleMorphism a b,
  fstOrderMatch :: TypedGraphMorphism a b
  } deriving (Eq,Show)

-- | Matches the second-order rule with the first-order, and calls theirs critical pairs
interLevelCP :: MorphismsConfig (RuleMorphism a b) -> (String, SndOrderRule a b) -> (String, TypedGraphRule a b) -> [(String,String,Int,InterLevelCP a b)]
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

-- | Calculates the second-order rewriting,
-- defines the dangling extension for L and L'',
-- gets all relevant graphs from L
interLevelConflictOneMatch :: MorphismsConfig (RuleMorphism a b) -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch conf' sndRule match = m0s
  where
    sndOrderL = leftMorphism sndRule
    sndOrderR = rightMorphism sndRule

    (k,l') = calculatePushoutComplementAlongM sndOrderL match
    (r',m') = calculatePushoutAlongM sndOrderR k 

    p = codomain match
    p'' = codomain m'

    bigL = leftMorphism p
    bigL'' = leftMorphism p''

    fl = mappingLeft l'
    gl = mappingLeft r'

    danglingExtFl = danglingExtension bigL <&> fl
    danglingExtGl = danglingExtension bigL'' <&> gl

    conf = toFstOrderMorphismsConfig conf'
    relevantGraphs = map codomain axs
    axs = relevantMatches conf danglingExtFl danglingExtGl

    m0s = concatMap defineMatches (removeDuplicated relevantGraphs)
    defineMatches = allILCP conf p p'' fl gl

removeDuplicated :: [GraphMorphism (Maybe a) (Maybe b)] -> [GraphMorphism (Maybe a) (Maybe b)]
removeDuplicated = nubBy (\x y -> not $ Prelude.null $ find x y)
  where
    find :: GraphMorphism (Maybe a) (Maybe b) -> GraphMorphism (Maybe a) (Maybe b) -> [TypedGraphMorphism a b]
    find = findMorphisms iso

-- | For a relevant graph, gets all matches and check conflict
allILCP :: DPO morph => MorphismsConfig morph -> Production morph -> Production morph -> morph -> morph -> Obj morph -> [morph]
allILCP conf p p'' fl gl ax = filter conflicts validMatches
  where
    validMatches = findApplicableMatches conf p ax
    conflicts = ilCP conf fl gl p''

-- | For a m0, checks if exists a conflicting m''0
ilCP :: DPO morph => MorphismsConfig morph -> morph -> morph -> Production morph -> morph -> Bool
ilCP conf fl gl p'' m0 = Prelude.null validM0''-- or all (==False) (map (\m'' -> satsGluing inj bigL'' m'') validM0'') --thesis def
  where
    matchesM0'' = findApplicableMatches conf p'' (codomain m0)
    validMatch = satisfiesRewritingConditions conf p''

    commutes m0'' = m0 <&> fl == m0'' <&> gl

    --paper definition
    --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
    validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantMatches :: MorphismsConfig (TypedGraphMorphism a b) -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
--relevantMatches inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantMatches conf dangFl dangGl = concatMap createQuotients allSubgraphs
  where
    createQuotients
      | matchRestriction conf `isSubclassOf` monic = \g -> [identity g]
      | otherwise = findAllQuotientsOf
    (_,al) = calculatePushout dangFl dangGl
    --axs = induzedSubgraphs al
    allSubgraphs = subgraphs (codomain al)

-- Algorithm proposed in (MACHADO, 2012) to extend a TGM.
-- For each orphan node in the received morphism l, it must add all
-- possible edges that connects in this node according to the type graph.
danglingExtension :: TypedGraphMorphism a b -> TypedGraphMorphism a b
danglingExtension l = tlUpdated
  where
    initObject = makeInclusion (codomain l) (codomain l)

    orphanNodes = orphanTypedNodeIds l
    typesOfOrphanNodes = map (extractNodeType typingMorphism) orphanNodes

    typingMorphism = codomain l
    typeGraph = codomain typingMorphism

    -- Select edges to be added
    dangT = [e | e <- G.edges typeGraph,
                 sourceId e `elem` typesOfOrphanNodes ||
                 targetId e `elem` typesOfOrphanNodes]

    -- Merge edges to be added with their nodes
    edgesToAdd = concatMap (\n -> map (\e -> (n,e)) dangT) orphanNodes

    tlUpdated = foldl addEdge initObject edgesToAdd

    addEdge tgm (n,e) =
      case (isSrc,isTgt) of
        (True,True) -> createSrcTgt tgm
        (True,False) -> createSrc tgm
        (False,True) -> createTgt tgm
        (False,False) -> error "danglingExtension: it's not supposed to be here"
      where
        typeNode = extractNodeType (codomain tgm) n
        isSrc = typeNode == sourceId e
        isTgt = typeNode == targetId e

        createEdge src tgt newGraph = createEdgeOnCodomain newEdgeId src tgt (edgeId e) newGraph
          where
            newEdgeId = head (newEdges (codomain newGraph))

        createSrcTgt morp = createTgt (createSrc morp)

        createSrc morp = createEdge n nodeId newGraph
          where
            nodeId = head (newNodes (codomain morp))
            typeNewNode = targetId e
            newGraph = createNodeOnCodomain nodeId typeNewNode morp

        createTgt morp = createEdge nodeId n newGraph
          where
            nodeId = head (newNodes (codomain morp))
            typeNewNode = sourceId e
            newGraph = createNodeOnCodomain nodeId typeNewNode morp
