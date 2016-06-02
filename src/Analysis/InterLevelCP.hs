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

danglingExtension :: TypedGraphMorphism a b -> TypedGraphMorphism a b
danglingExtension l = error (show x)
  where
    ld = orphanNodesTyped l
    tl = codomain l
    t = codomain tl
    tlx n' = any (\n -> applyNodeUnsafe tl n == n') ld
    dangT = filter (\e -> tlx (sourceOfUnsafe t e) || tlx (targetOfUnsafe t e)) (edges t)
    
    --init = idTGM (codomain l)
    
    x = map (\n -> map (\e -> (n,e)) dangT) ld

--idTGM :: GM.GraphMorphism a b -> TypedGraphMorphism a b
--idTGM = Abstract.Morphism.id

interLevelConflict :: (String, SndOrderRule a b) -> (String, GraphRule a b) -> [(String,(RuleMorphism a b, TypedGraphMorphism a b))]
interLevelConflict (sndName,sndRule) (fstName,fstRule) = zip newNames (concatMap conflicts nacs)
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    nacInj = False
    inj = False
    sndOrderL = left sndRule
    leftRule = codomain sndOrderL
    mats = matches MONO leftRule fstRule
    gluing = filter (\m -> satsGluing inj m sndOrderL) mats
    nacs = filter (satsNacs nacInj inj sndRule) gluing
    conflicts m = 
      do
        a <- interLevelConflictOneMatch sndRule m
        return (m,a)

interLevelConflictOneMatch :: SndOrderRule a b -> RuleMorphism a b -> [TypedGraphMorphism a b]
interLevelConflictOneMatch sndRule match = m0s
  where
    --nacInj = False
    inj = False
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
    (_,al) = po fl (compose gl (danglingExtension bigL''))
    
    -- step 2c
    axs = induzedSubgraphs al
    
    m0s = concatMap calc axs
    
    calc ax = mapMaybe
                (\(m0,fact) -> if test fact then Just m0 else Nothing)
                (zip mm facts)
      where
        -- step 2d
        part = partitions False (codomain ax)
        
        -- step 2e
        axeps = map (\ep -> compose ax ep) part
        mm = filter (\axep -> satsGluing inj axep bigL) axeps
        
        -- step3
        facts = map (\m0 -> let mts = matches MONO (codomain bigL'') (codomain m0)
                             in filter (\m'' -> compose fl m0 == compose gl m'') mts) mm
        
        test x = Prelude.null x || all (==False) (map (\m'' -> satsGluing inj m'' bigL'') x)
