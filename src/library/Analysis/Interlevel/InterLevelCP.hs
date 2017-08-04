{-|
Module      : InterLevelCP
Description : Implements the inter-level critical pairs
-}
module Analysis.Interlevel.InterLevelCP
  ( interLevelCP,
    InterLevelCP(..),
    danglingExtension
  ) where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import qualified Category.TypedGraph as TGraph
import Category.TypedGraphRule (liftFstOrder, TypedGraphRule, Production(..), RuleMorphism, mappingLeft)
import qualified Category.TypedGraphRule as TGRule
import           Data.Graphs
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraphRule
import Util.Monad


data InterLevelCP n e = InterLevelCP {
  sndOrderMatch :: RuleMorphism n e,
  fstOrderMatch :: TypedGraphMorphism n e
  } deriving (Eq,Show)

-- | Matches the second-order rule with the first-order, and calls theirs critical pairs
interLevelCP :: (String, SndOrderRule n e) -> (String, TypedGraphRule n e) -> TGRule.CatM n e [(String,String,Int,InterLevelCP n e)]
interLevelCP (sndName, sndRule) (fstName, fstRule) = addNames <$> do
  matches <- findApplicableMatches sndRule fstRule
  concatMapM conflictsForMatch matches
  where
    addNames = zipWith (\number conflict -> (fstName, sndName, number, conflict)) [0..]
    conflictsForMatch match = map (InterLevelCP match) <$> interLevelConflictOneMatch sndRule match

-- | Calculates the second-order rewriting,
-- defines the dangling extension for L and L'',
-- gets all relevant graphs from L
interLevelConflictOneMatch :: SndOrderRule n e -> RuleMorphism n e -> TGRule.CatM n e [TypedGraphMorphism n e]
interLevelConflictOneMatch sndRule match = do
  (k,l') <- calculatePushoutComplementAlongMono (leftMorphism sndRule) match
  (m',r') <- calculatePushout k (rightMorphism sndRule)
  let fl = mappingLeft l'
      gl = mappingLeft r'

      p = codomain match
      p'' = codomain m'

      bigL = leftMorphism p
      bigL'' = leftMorphism p''

      danglingExtFl = danglingExtension bigL <&> fl
      danglingExtGl = danglingExtension bigL'' <&> gl
  liftFstOrder $ do
    axs <- relevantMatches danglingExtFl danglingExtGl
    relevantGraphs <- removeDuplicated (map codomain axs)
    let defineMatches = allILCP p p'' fl gl
    concatMapM defineMatches relevantGraphs


removeDuplicated :: [TypedGraph a b] -> TGraph.CatM a b [TypedGraph a b]
removeDuplicated = nubByM (\x y -> not . Prelude.null <$> findMorphisms iso x y)

-- | For a relevant graph, gets all matches and check conflict
allILCP :: DPO cat morph => Production cat morph -> Production cat morph -> morph -> morph -> Obj cat -> cat [morph]
allILCP p p'' fl gl ax = filterM conflicts =<< findApplicableMatches p ax
  where conflicts = ilCP fl gl p''

-- | For a m0, checks if exists a conflicting m''0
ilCP :: DPO cat morph => morph -> morph -> Production cat morph -> morph -> cat Bool
ilCP fl gl p'' m0 = do
  matchesM0'' <- findApplicableMatches p'' (codomain m0)
  --paper definition
  --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
  validM0'' <- filterM (\m0'' -> commutes m0'' `andM` validMatch m0'') matchesM0''
  return (Prelude.null validM0'')-- or all (==False) (map (\m'' -> satsGluing inj bigL'' m'') validM0'') --thesis def
  where
    validMatch = satisfiesRewritingConditions p''
    commutes m0'' = return (m0 <&> fl == m0'' <&> gl)

relevantMatches :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TGraph.CatM n e [TypedGraphMorphism n e]
--relevantMatches inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantMatches dangFl dangGl = do
  (_, al) <- calculatePushout dangFl dangGl
  subgraphs <- map domain <$> findAllSubobjectsOf (codomain al)
  -- FIXME: does the following refer to matches of first- or second-order rules?
  monicMatches <- matchMorphism `isSubclassOf` monic
  if monicMatches
    then return (map identity subgraphs)
    else concatMapM findAllQuotientsOf subgraphs

-- Algorithm proposed in (MACHADO, 2012) to extend a TGM.
-- For each orphan node in the received morphism l, it must add all
-- possible edges that connects in this node according to the type graph.
danglingExtension :: TypedGraphMorphism n e -> TypedGraphMorphism n e
danglingExtension l = tlUpdated
  where
    initObject = idMap (codomain l) (codomain l)

    orphanNodes = orphanTypedNodeIds l
    typesOfOrphanNodes = map (extractNodeType typingMorphism) orphanNodes

    typingMorphism = codomain l
    typeGraph = codomain typingMorphism
    edgesOfTypeGraph = edges typeGraph

    -- Select edges to be added
    dangT = [e | e <- edgesOfTypeGraph,
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
            newEdgeId = head (newTypedEdges (codomain newGraph))

        createSrcTgt morp = createTgt (createSrc morp)

        createSrc morp = createEdge n nodeId newGraph
          where
            nodeId = head (newTypedNodes (codomain morp))
            typeNewNode = targetId e
            newGraph = createNodeOnCodomain nodeId typeNewNode morp

        createTgt morp = createEdge nodeId n newGraph
          where
            nodeId = head (newTypedNodes (codomain morp))
            typeNewNode = sourceId e
            newGraph = createNodeOnCodomain nodeId typeNewNode morp
