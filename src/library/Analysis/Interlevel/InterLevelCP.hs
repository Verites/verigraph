{-|
Module      : InterLevelCP
Description : Implements the inter-level critical pairs
Stability   : development
-}

module Analysis.Interlevel.InterLevelCP (interLevelConflict) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
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

    r1Left = codomain (left r1)
    r2Left = codomain (left r2)
    r1Right = codomain (right r1)
    r2Right = codomain (right r2)
      isDeleteUse config r1Left (mappingLeft m1, mappingLeft m2) ||
      isDeleteUse config r2Left (mappingLeft m2, mappingLeft m1)
      isDeleteUse config r1Right (mappingRight m1, mappingRight m2) ||
      isDeleteUse config r2Right (mappingRight m2, mappingRight m1)
    r1Left = codomain (getLHS r1)
    r2Left = codomain (getLHS r2)
    r1Right = codomain (getRHS r1)
    r2Right = codomain (getRHS r2)
    leftR1 = constructProduction (mappingLeft (getLHS r1)) (mappingLeft (getRHS r1)) []
    leftR2 = constructProduction (mappingLeft (getLHS r2)) (mappingLeft (getRHS r2)) []
    pairs = createJointlyEpimorphicPairs (matchRestriction config == MonoMatches) leftR1 leftR2
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
      map (\number -> fstName ++ ";" ++ sndName ++ ";" ++ show number) ([0..] :: [Int])


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

    m0s = concatMap defineMatches (removeDuplicatedGM axs [])

    defineMatches = allILCP config p p'' fl gl

allILCP :: DPO m => DPOConfig -> Production m -> Production m -> m -> m -> Obj m -> [m]
allILCP config p p'' fl gl ax = filter conflicts validMatches
  where
        validMatches = findApplicableMatches config p ax
    conflicts = ilCP config fl gl p''
ilCP :: DPO m => DPOConfig -> m -> m -> Production m -> m -> Bool
ilCP config fl gl p'' m0 = Prelude.null validM0''-- or all (==False) (map (\m'' -> satsGluing inj bigL'' m'') validM0'') --thesis def
  where
    matchesM0'' = allMatches config p'' (codomain m0)
    validMatch = satsGluingAndNacs config p''

    commutes m0'' = compose fl m0 == compose gl m0''

    --paper definition
    --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
    validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantGraphs :: DPOConfig -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
--relevantGraphs inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantGraphs config dangFl dangGl = concatMap (createAllSubobjects matchInjective) axs
  where
    matchInjective = matchRestriction config == MonoMatches
    (_,al) = calculatePushout dangFl dangGl
    --axs = induzedSubgraphs al
    axs = subgraphs (codomain al)

removeDuplicatedGM :: [TypedGraphMorphism a b] -> [GraphMorphism a b] -> [GraphMorphism a b]
removeDuplicatedGM [] act = act
removeDuplicatedGM (x:xs) act = removeDuplicatedGM xs add
  where
    isos = concatMap (find (codomain x)) act
    add = if Prelude.null isos then (codomain x):act else act

find :: GraphMorphism a b -> GraphMorphism a b -> [TypedGraphMorphism a b]
find = findMorphisms IsoMorphisms
