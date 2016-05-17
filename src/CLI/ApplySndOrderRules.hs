{-# LANGUAGE TypeFamilies #-}

module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Abstract.Valid
--import           Data.Maybe
import           Graph.Graph               as G
import qualified Graph.GraphMorphism       as GM
--import           Graph.EpiPairs
import           Graph.TypedGraphMorphism

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import qualified Graph.GraphGrammar        as GG
import           Graph.GraphRule
import qualified Graph.RuleMorphism        as SO
import qualified Graph.SndOrderRule        as SO
import           Options.Applicative
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data Options = Options
  { outputFile :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("GGX file that will be written, adding the new rules to the original graph grammar"))

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    let --nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = if arbitraryMatches globalOpts then ALL else MONO
        newRules = applySndOrderRules onlyInj (GG.rules gg) (GG.sndOrderRules gg)
        testSndOrder = map (\(n,r) -> (n,newNacs r)) (GG.sndOrderRules gg)
        rule = snd (head (GG.sndOrderRules gg))
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ newRules) testSndOrder--(GG.sndOrderRules gg)
        --rul = snd (head (GG.sndOrderRules gg))
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
    
    print (GG.rules gg)
    print (head (nacs rule))
    print (newNacsPairL rule)
    
    putStrLn "Done!"
    putStrLn ""

-- | Generates the minimal safety NACs of a 2-rule
newNacs :: SO.SndOrderRule a b -> SO.SndOrderRule a b
newNacs sndRule =
  production
    (SO.left sndRule)
    (SO.right sndRule)
    ((nacs sndRule) ++
     (newNacsProbL sndRule) ++
     (newNacsProbR sndRule){- ++
     (newNacsPairL sndRule)-})

newNacsProbL :: SO.SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProbL sndRule = map (\x -> createNacProbL ruleL x (tp x)) probL
  where
    apply = applyNodeTGMUnsafe
    
    ruleL = codomain (SO.left sndRule)
    ruleK = domain (SO.left sndRule)
    ruleR = codomain (SO.right sndRule)
    
    fl = SO.mappingLeft (SO.left sndRule)
    gl = SO.mappingLeft (SO.right sndRule)
    
    la = left ruleL
    lb = left ruleK
    lc = left ruleR
    
    probL = [apply fl n |
                 n <- nodesCodomain lb
               , orphanNode la (apply fl n)
               , orphanNode lb n
               , not (orphanNode lc (apply gl n))]
    
    tp = GM.applyNodeUnsafe (codomain la)

createNacProbL :: GraphRule a b -> NodeId -> NodeId -> SO.RuleMorphism a b
createNacProbL ruleL x tp = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where
    x' = head (newNodes (domain (domain (left ruleL))))
    x'' = head (newNodes (domain (codomain (right ruleL))))
    updateLeft = createNodeDomTGM x' tp x (left ruleL)
    updateRightCod = createNodeCodTGM x'' tp (right ruleL)
    updateRightMap = updateNodeRelationTGM x' x'' tp updateRightCod
    nacRule = production updateLeft updateRightMap []
    mapL = idMap (codomain (left ruleL)) (codomain updateLeft)
    mapK = idMap (domain (left ruleL)) (domain updateLeft)
    mapR = idMap (codomain (right ruleL)) (codomain updateRightMap)

newNacsProbR :: SO.SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProbR sndRule = map (\x -> createNacProbR ruleL x (tp x)) probR
  where
    apply = applyNodeTGMUnsafe
    
    ruleL = codomain (SO.left sndRule)
    ruleK = domain (SO.left sndRule)
    ruleR = codomain (SO.right sndRule)
    
    fr = SO.mappingRight (SO.left sndRule)
    gr = SO.mappingRight (SO.right sndRule)
    
    ra = right ruleL
    rb = right ruleK
    rc = right ruleR
    
    probR = [apply fr n |
                 n <- nodesCodomain rb
               , orphanNode ra (apply fr n)
               , orphanNode rb n
               , not (orphanNode rc (apply gr n))]
    
    tp = GM.applyNodeUnsafe (codomain ra)

createNacProbR :: GraphRule a b -> NodeId -> NodeId -> SO.RuleMorphism a b
createNacProbR ruleL x tp = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where
    x' = head (newNodes (domain (domain (left ruleL))))
    x'' = head (newNodes (domain (codomain (left ruleL))))
    updateRight = createNodeDomTGM x' tp x (right ruleL)
    updateLeftCod = createNodeCodTGM x'' tp (left ruleL)
    updateLeftMap = updateNodeRelationTGM x' x'' tp updateLeftCod
    nacRule = production updateLeftMap updateRight []
    mapL = idMap (codomain (left ruleL)) (codomain updateLeftMap)
    mapK = idMap (domain (left ruleL)) (domain updateLeftMap)
    mapR = idMap (codomain (right ruleL)) (codomain updateRight)

newNacsPairL :: SO.SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsPairL sndRule = ret
  where
    apply = applyNodeTGMUnsafe
    
    ruleL = codomain (SO.left sndRule)
    ruleK = domain (SO.left sndRule)
    ruleR = codomain (SO.right sndRule)
    
    fl = SO.mappingLeft (SO.left sndRule)
    gl = SO.mappingLeft (SO.right sndRule)
    
    lb = left ruleK
    lc = left ruleR
    
    pairL = [(apply fl x, apply fl y) |
                     x <- nodesCodomain lb
                   , y <- nodesCodomain lb
                   , x /= y
                   , orphanNode lb x
                   , not (orphanNode lc (apply gl x))
                   , not (orphanNode lc (apply gl y))]
    
    epis = calculateAllPartitions ruleL
    el e = apply (SO.mappingLeft e)
    
    ret = [e | e <- epis, any (\(a,b) -> (el e) a == (el e) b) pairL]
    
    createNac e = SO.ruleMorphism ruleL ruleNac e mapK mapR
      where
        ruleNac = production (compose (left ruleL) e) (right ruleL) []
        mapK = idMap (domain (left ruleL)) (domain (left ruleL))
        mapR = idMap (codomain (right ruleL)) (codomain (right ruleL))

calculateAllPartitions :: GraphRule a b -> [SO.RuleMorphism a b]
calculateAllPartitions r = ret
  where
    inj = True
    l = left r
    rr = right r
    graphL = codomain (left r)
    graphK = domain (left r)
    graphR = codomain (right r)
    graphNull = GM.empty G.empty G.empty
    partL = map fst (createPairs inj graphL graphNull)
    partK = map fst (createPairs inj graphK graphNull)
    partR = map fst (createPairs inj graphR graphNull)
    allPart = [(a,b,c) | a <- partL, b <- partK, c <- partR]
    allRules = map (\(a,b,c) -> (a,b,c,production (compose (compose (invertTGM b) l) a) (compose (compose (invertTGM b) rr) c) [])) allPart
    valids = filter (\(_,_,_,x) -> valid x) allRules
    ret = map (\(a,b,c,rule) -> SO.ruleMorphism r rule a b c) valids

orphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
orphanNode m n = n `elem` (orphanNodesTyped m)

applySndOrderRules :: PROP -> [(String, GraphRule a b)] -> [(String, SO.SndOrderRule a b)] -> [(String, GraphRule a b)]
applySndOrderRules prop fstRules = concatMap (\r -> applySndOrderRuleListRules prop r fstRules)

applySndOrderRuleListRules :: PROP -> (String, SO.SndOrderRule a b) -> [(String, GraphRule a b)] -> [(String, GraphRule a b)]
applySndOrderRuleListRules prop sndRule = concatMap (applySndOrderRule prop sndRule)

applySndOrderRule :: PROP -> (String, SO.SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule prop (sndName,sndRule) (fstName,fstRule) = zip newNames newRules
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    leftRule = SO.left sndRule
    rightRule = SO.right sndRule
    matches = SO.matchesSndOrder prop (codomain leftRule) fstRule
    gluing = filter (\m -> satsGluing False m leftRule) matches
    nacs = filter (satsNacs True False sndRule) gluing
    newRules = map
                 (\match ->
                   let (k,_)  = poc match leftRule
                       (m',_) = po k rightRule in
                       codomain m'
                   ) nacs
