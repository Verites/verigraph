{-|
Module      : InterLevelCP
Description : Implements the inter-level conflict
Stability   : development
-}

module Analysis.InterLevelCP (interLevelConflict, evo) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Valid
import           Analysis.DiagramAlgorithms
import           Graph.Graph
import           Graph.GraphMorphism
import           SndOrder.Morphism
import           SndOrder.Rule
import           TypedGraph.Graph
import           TypedGraph.GraphRule
import           TypedGraph.Morphism
import           TypedGraph.Subgraph

-- TODO: Decent names, please
-- TODO: Follow naming convention for haskell: CamelCase
data CPE = FOL_FOL | DUSE_DUSE | FOL_DUSE | DUSE_FOL deriving(Show)

classify :: DPOConfig -> SndOrderRule a b -> SndOrderRule a b -> (RuleMorphism a b, RuleMorphism a b) -> CPE
classify config r1 r2 (m1,m2) =
  case (deleteUseFlGl, deleteUseFlGl'') of
    (True,True) -> DUSE_DUSE
    (True,False) -> DUSE_FOL
    (False,True) -> FOL_DUSE
    (False,False) -> FOL_FOL
  where
    r1Left = codomain (getLHS r1)
    r2Left = codomain (getLHS r2)
    r1Right = codomain (getRHS r1)
    r2Right = codomain (getRHS r2)

    deleteUseFlGl =
      isDeleteUse config r1Left (mappingLeft m1, mappingLeft m2) ||
      isDeleteUse config r2Left (mappingLeft m2, mappingLeft m1)
    deleteUseFlGl'' =
      isDeleteUse config r1Right (mappingRight m1, mappingRight m2) ||
      isDeleteUse config r2Right (mappingRight m2, mappingRight m1)

-- TODO: Decent names, please
-- TODO: Remove duplication (as per hlint)
evo :: DPOConfig -> (String, SndOrderRule a b) -> (String, SndOrderRule a b) -> (String, [CPE])
evo config (n1,r1) (n2,r2) = (n1 ++ "_" ++ n2, map (classify config r1 r2) xs'')
  where
    r1Left = codomain (getLHS r1)
    r2Left = codomain (getLHS r2)
    r1Right = codomain (getRHS r1)
    r2Right = codomain (getRHS r2)

    leftR1 = constructProduction (mappingLeft (getLHS r1)) (mappingLeft (getRHS r1)) []
    leftR2 = constructProduction (mappingLeft (getLHS r2)) (mappingLeft (getRHS r2)) []

    pairs = createJointlyEpimorphicPairs (matchRestriction config == MonoMatches) leftR1 leftR2

    xs = filter (\(m1,_) -> valid (codomain m1)) pairs
    xs' = filter (\(m1,m2) -> satisfyRewritingConditions config (r1Left, mappingLeft m1) (r2Left, mappingLeft m2)) xs
    xs'' = filter (\(m1,m2) -> satisfyRewritingConditions config (r1Right, mappingLeft m1) (r2Right, mappingLeft m2)) xs'

danglingExtension :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
danglingExtension gl l = tlUpdated
  where
    ld = orphanNodesTyped l
    tl = codomain l
    t = codomain tl
    tlx n' = any (\n -> applyNodeUnsafe tl n == n') ld
    dangT = filter (\e -> tlx (sourceOfUnsafe t e) || tlx (targetOfUnsafe t e)) (edges t)

    edgesToAdd = concatMap (\n -> map (\e -> (n,e)) dangT) ld

    tlUpdated = foldl addEdge gl edgesToAdd

    addEdge tgm (n,e) =
      case (isSrc,isTgt) of
        (True,True) -> createSrcTgt tgm e n
        (True,False) -> createSrc tgm e n
        (False,True) -> createTgt tgm e n
        (False,False) -> error "danglingExtension: it's not supposed to be here"
      where
        typeNode = applyNodeUnsafe (codomain tgm) n
        isSrc = typeNode == sourceOfUnsafe t e
        isTgt = typeNode == targetOfUnsafe t e

        createEdge tgm e s t = createEdgeCodTGM edgeId s t e tgm
          where
            edgeId = head (newEdgesTyped (codomain tgm))

        createSrcTgt tgm e n = createTgt (createSrc tgm e n) e n

        createSrc tgm e n = createEdge newGraph e n nodeId
          where
            nodeId = head (newNodesTyped (codomain tgm))
            typeNewNode = targetOfUnsafe (codomain (codomain tgm)) e
            newGraph = createNodeCodTGM nodeId typeNewNode tgm

        createTgt tgm e n = createEdge newGraph e nodeId n
          where
            nodeId = head (newNodesTyped (codomain tgm))
            typeNewNode = sourceOfUnsafe (codomain (codomain tgm)) e
            newGraph = createNodeCodTGM nodeId typeNewNode tgm

interLevelConflict :: DPOConfig -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String,(RuleMorphism a b, TypedGraphMorphism a b))]
interLevelConflict config (sndName, sndRule) (fstName, fstRule) =
  zip newNames (concatMap conflictsForMatch validMatches)

  where
    validMatches =
      findApplicableMatches config sndRule fstRule

    conflictsForMatch match =
      do
        conflicts <- interLevelConflictOneMatch config sndRule match
        return (match, conflicts)

    newNames =
      map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])


interLevelConflictOneMatch :: DPOConfig -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch config sndRule match = m0s
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

    axs = relevantGraphs config danglingExtFl danglingExtGl

    m0s = concatMap defineMatches axs

    defineMatches ax = filter conflicts validMatches
      where
        validMatches = findApplicableMatches config p (codomain ax)

        conflicts m0 = Prelude.null validM0''-- or all (==False) (map (\m'' -> satisfiesGluingConditions inj bigL'' m'') validM0'') --thesis def
          where
            matchesM0'' = findAllMatches config p'' (codomain m0)
            validMatch = satisfiesRewritingConditions config p''

            commutes m0'' = compose fl m0 == compose gl m0''

            --paper definition
            --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
            validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantGraphs :: DPOConfig -> TypedGraphMorphism a b -> TypedGraphMorphism a b
               -> [TypedGraphMorphism a b]
--relevantGraphs inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantGraphs config dangFl dangGl = concatMap (createAllSubobjects matchInjective) axs
  where
    matchInjective = matchRestriction config == MonoMatches
    (_,al) = calculatePushout dangFl dangGl
    --axs = induzedSubgraphs al
    axs = subgraphs (codomain al)
