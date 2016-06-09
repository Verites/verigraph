module Analysis.InterLevelCP (interLevelConflict) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.EpiPairs ()
import           Graph.Graph
import           Graph.GraphMorphism
import           Graph.GraphRule
import           Graph.TypedGraphMorphism
import           Graph.RuleMorphism
import           Graph.SndOrderRule

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

interLevelConflict :: Bool -> Bool -> (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String,(RuleMorphism a b, TypedGraphMorphism a b))]
interLevelConflict nacInj inj (sndName,sndRule) (fstName,fstRule) = zip newNames (concatMap conflicts validMatches)
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    sndOrderL = left sndRule
    leftRule = codomain sndOrderL
    
    mats = matches (injectiveBoolToProp inj) leftRule fstRule
    validMatches = filter (satsGluingAndNacs nacInj inj sndRule) mats
    
    conflicts m = 
      do
        a <- interLevelConflictOneMatch nacInj inj sndRule m
        return (m,a)

interLevelConflictOneMatch :: Bool -> Bool -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch nacInj inj sndRule match = m0s
  where
    sndOrderL = left sndRule
    sndOrderR = right sndRule
    
    (k,l') = poc match sndOrderL
    (m',r') = po k sndOrderR
    
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
        mats = matches (injectiveBoolToProp inj) (codomain bigL) (codomain ax)
        validMatches = filter (satsGluingAndNacs nacInj inj p) mats
        
        conflicts m0 = Prelude.null validM0''-- || all (==False) (map (\m'' -> satsGluing inj m'' bigL'') validM0'') --thesis def
          where
            matchesM0'' = matches (injectiveBoolToProp inj) (codomain bigL'') (codomain m0)
            validMatch = satsGluingAndNacs nacInj inj p''
            
            commutes m0'' = compose fl m0 == compose gl m0''
            
            --paper definition
            --validM0'' = filter (\m0'' -> not ((validMatch m0'') && (commutes m0''))) matchesM0''
            validM0'' = filter (\m0'' -> commutes m0'' && validMatch m0'') matchesM0''

relevantGraphs :: Bool -> TypedGraphMorphism a b -> TypedGraphMorphism a b
               -> [TypedGraphMorphism a b]
relevantGraphs inj dangFl dangGl = concatMap (\ax -> partitions inj (codomain ax)) axs
  where
    (_,al) = po dangFl dangGl
    axs = induzedSubgraphs al
