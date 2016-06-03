module Analysis.InterLevelCP (interLevelConflict) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.EpiPairs ()
import           Data.Maybe (mapMaybe)
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
    
    -- when two edges from the same source (or domain) in the typegraph 
    -- are created, it creates two differents nodes. verify this
    addEdge tgm (n,e) =
      case (isSrc,isTgt) of
        (True,True) -> createEdge tgm e n n
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
interLevelConflict nacInj inj (sndName,sndRule) (fstName,fstRule) = zip newNames (concatMap conflicts nacs)
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    sndOrderL = left sndRule
    leftRule = codomain sndOrderL
    mats = matches (injectiveBoolToProp inj) leftRule fstRule
    gluing = filter (\m -> satsGluing inj m sndOrderL) mats
    nacs = filter (satsNacs nacInj inj sndRule) gluing
    conflicts m = 
      do
        a <- interLevelConflictOneMatch inj sndRule m
        return (m,a)

interLevelConflictOneMatch :: Bool -> SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch inj sndRule match = m0s
  where
    sndOrderL = left sndRule
    sndOrderR = right sndRule
    
    -- step 2a
    (k,l') = poc match sndOrderL
    (m',r') = po k sndOrderR
    p = codomain match
    p'' = codomain m'
    
    bigL = left p
    bigL'' = left p''
    
    fl = mappingLeft l'
    gl = mappingLeft r'
    
    -- step 2b
    (_,al) = po fl (compose gl (danglingExtension gl bigL''))
    
    -- step 2c
    axs = induzedSubgraphs al
    
    m0s = concatMap calc axs
    
    calc ax = mapMaybe
                (\(m0,fact) -> if test fact then Just m0 else Nothing)
                (zip mm facts)
      where
        -- step 2d
        part = partitions inj (codomain ax)
        
        -- step 2e
        axeps = map (\ep -> compose ax ep) part
        mm = filter (\axep -> satsGluing inj axep bigL) axeps
        
        -- step3
        facts = map (\m0 -> let mts = matches (injectiveBoolToProp inj) (codomain bigL'') (codomain m0)
                             in filter (\m'' -> compose fl m0 == compose gl m'') mts) mm
        
        test x = Prelude.null x || all (==False) (map (\m'' -> satsGluing inj m'' bigL'') x)
