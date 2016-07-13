{-|
Module      : InterLevelCP
Description : Implements the inter-level conflict
Stability   : development
-}

--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.InterLevelCP (interLevelConflict, evo) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import           Analysis.DiagramAlgorithms
import           Graph.Graph
import           Graph.GraphMorphism
import           TypedGraph.GraphRule
import           TypedGraph.Graph
import           TypedGraph.Morphism
import           TypedGraph.Subgraph
import           SndOrder.Morphism
import           SndOrder.Rule

-- TODO: Decent names, please
-- TODO: Follow naming convention for haskell: CamelCase
data CPE = FOL_FOL | DUSE_DUSE | FOL_DUSE | DUSE_FOL deriving(Show)

classify :: MatchRestriction -> SndOrderRule a b -> SndOrderRule a b -> (RuleMorphism a b, RuleMorphism a b) -> CPE
classify inj r1 r2 (m1,m2) =
  case (deleteUseFlGl, deleteUseFlGl'') of
    (True,True) -> DUSE_DUSE
    (True,False) -> DUSE_FOL
    (False,True) -> FOL_DUSE
    (False,False) -> FOL_FOL
  where
    r1Left = codomain (left r1)
    r2Left = codomain (left r2)
    r1Right = codomain (right r1)
    r2Right = codomain (right r2)

    deleteUseFlGl =
      deleteUse inj r1Left (mappingLeft m1, mappingLeft m2) ||
      deleteUse inj r2Left (mappingLeft m2, mappingLeft m1)
    deleteUseFlGl'' =
      deleteUse inj r1Right (mappingRight m1, mappingRight m2) ||
      deleteUse inj r2Right (mappingRight m2, mappingRight m1)

-- TODO: Decent names, please
-- TODO: Remove duplication (as per hlint)
evo :: NacSatisfaction -> MatchRestriction -> (String, SndOrderRule a b) -> (String, SndOrderRule a b) -> (String, [CPE])
evo nacInj inj (n1,r1) (n2,r2) = (n1 ++ "_" ++ n2, map (classify inj r1 r2) xs'')
  where
    r1Left = codomain (left r1)
    r2Left = codomain (left r2)
    r1Right = codomain (right r1)
    r2Right = codomain (right r2)

    leftR1 = production (mappingLeft (left r1)) (mappingLeft (right r1)) []
    leftR2 = production (mappingLeft (left r2)) (mappingLeft (right r2)) []

    pairs = createPairs (inj == MonoMatches) leftR1 leftR2

    xs = filter (\(m1,_) -> valid (codomain m1)) pairs
    xs' = filter (\(m1,m2) -> satsGluingNacsBoth nacInj inj (r1Left, mappingLeft m1) (r2Left, mappingLeft m2)) xs
    xs'' = filter (\(m1,m2) -> satsGluingNacsBoth nacInj inj (r1Right, mappingLeft m1) (r2Right, mappingLeft m2)) xs'

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

interLevelConflict :: NacSatisfaction -> MatchRestriction -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String,(RuleMorphism a b, TypedGraphMorphism a b))]
interLevelConflict nacInj inj (sndName,sndRule) (fstName,fstRule) = zip newNames (concatMap conflicts validMatches)
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    sndOrderL = left sndRule
    leftRule = codomain sndOrderL

    mats = matches (matchRestrictionToProp inj) leftRule fstRule
    validMatches = filter (satsGluingAndNacs nacInj inj sndRule) mats

    conflicts m =
      do
        a <- interLevelConflictOneMatch nacInj inj sndRule m
        return (m,a)

interLevelConflictOneMatch :: NacSatisfaction -> MatchRestriction -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch nacInj inj sndRule match = m0s
  where
    sndOrderL = left sndRule
    sndOrderR = right sndRule

    (k,l') = pushoutComplement match sndOrderL
    (m',r') = pushout k sndOrderR

    p = codomain match
    p'' = codomain m'

    bigL = left p
    bigL'' = left p''

    fl = mappingLeft l'
    gl = mappingLeft r'

    danglingExtFl = compose fl (danglingExtension fl bigL)
    danglingExtGl = compose gl (danglingExtension gl bigL'')

    axs = relevantGraphs inj danglingExtFl danglingExtGl

    m0s = concatMap defineMatches axs

    defineMatches ax = filter conflicts validMatches
      where
        mats = matches (matchRestrictionToProp inj) (codomain bigL) (codomain ax)
        validMatches = filter (satsGluingAndNacs nacInj inj p) mats

        conflicts m0 = Prelude.null validM0''-- or all (==False) (map (\m'' -> satsGluing inj bigL'' m'') validM0'') --thesis def
          where
            matchesM0'' = matches (matchRestrictionToProp inj) (codomain bigL'') (codomain m0)
            validMatch = satsGluingAndNacs nacInj inj p''

            commutes m0'' = compose fl m0 == compose gl m0''

            --paper definition
            --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
            validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantGraphs :: MatchRestriction -> TypedGraphMorphism a b -> TypedGraphMorphism a b
               -> [TypedGraphMorphism a b]
--relevantGraphs inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
relevantGraphs inj dangFl dangGl = concatMap (partitions (inj == MonoMatches)) axs
  where
    (_,al) = pushout dangFl dangGl
    --axs = induzedSubgraphs al
    axs = subgraphs (codomain al)
