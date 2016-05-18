{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

--import           Abstract.Valid
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

data Side = LeftSide | RightSide

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
        --rule = snd (head (GG.sndOrderRules gg))
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ newRules) testSndOrder--(GG.sndOrderRules gg)
        --rul = snd (head (GG.sndOrderRules gg))
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
    
    --print (GG.rules gg)
    --print (head (nacs rule))
    --print (map (\x -> codomain (left (codomain x))) (newNacsPairL rule))
    --print (length (newNacsPairL rule))
    
    putStrLn "Done!"
    putStrLn ""

-- | Generates the minimal safety NACs of a 2-rule
newNacs :: SO.SndOrderRule a b -> SO.SndOrderRule a b
newNacs sndRule =
  production
    (SO.left sndRule)
    (SO.right sndRule)
    ((nacs sndRule) ++
     (newNacsProb LeftSide sndRule) ++
     (newNacsProb RightSide sndRule){- ++
     (newNacsPairL sndRule)-})

newNacsProb :: Side -> SO.SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsProb side sndRule = nacNodes ++ nacEdges
  where
    (mapSide, getSide) =
      case side of
        LeftSide -> (SO.mappingLeft, left)
        RightSide -> (SO.mappingRight, right)
    
    applyNode = applyNodeTGMUnsafe
    applyEdge = applyEdgeTGMUnsafe
    
    ruleL = codomain (SO.left sndRule)
    ruleK = domain (SO.left sndRule)
    ruleR = codomain (SO.right sndRule)
    
    f = mapSide (SO.left sndRule)
    g = mapSide (SO.right sndRule)
    
    sa = getSide ruleL
    sb = getSide ruleK
    sc = getSide ruleR
    
    nodeProb = [applyNode f n |
                 n <- nodesCodomain sb
               , orphanNode sa (applyNode f n)
               , orphanNode sb n
               , not (orphanNode sc (applyNode g n))]
    
    edgeProb = [applyEdge f n |
                 n <- edgesCodomain sb
               , orphanEdge sa (applyEdge f n)
               , orphanEdge sb n
               , not (orphanEdge sc (applyEdge g n))]
    
    nacNodes = map (\x -> createNacNodeProb side ruleL x) nodeProb
    nacEdges = map (\x -> createNacEdgeProb side ruleL x) edgeProb

createNacNodeProb :: Side -> GraphRule a b -> NodeId -> SO.RuleMorphism a b
createNacNodeProb side ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where    
    l = left ruleL
    r = right ruleL
    
    graphL = codomain l
    graphK = domain l
    graphR = codomain r
    
    tp = GM.applyNodeUnsafe graphL x
    
    (graphSide, side1, side2) =
      case side of
        LeftSide -> (graphR, l, r)
        RightSide -> (graphL, r, l)
    
    x' = head (newNodes (domain graphK))
    x'' = head (newNodes (domain graphSide))
    
    updateSide1 = createNodeDomTGM x' tp x side1
    updateSide2Cod = createNodeCodTGM x'' tp side2
    updateSide2Map = updateNodeRelationTGM x' x'' tp updateSide2Cod
    
    nacRule = production updateSide1 updateSide2Map []
    mapL = idMap graphL (codomain updateSide1)
    mapK = idMap graphK (domain updateSide1)
    mapR = idMap graphR (codomain updateSide2Map)

createNacEdgeProb :: Side -> GraphRule a b -> EdgeId -> SO.RuleMorphism a b
createNacEdgeProb side ruleL x = SO.ruleMorphism ruleL nacRule mapL mapK mapR
  where
    l = left ruleL
    r = right ruleL
    
    graphL = codomain l
    graphK = domain l
    graphR = codomain r
    
    src = G.sourceOfUnsafe (domain graphL) x
    tgt = G.targetOfUnsafe (domain graphL) x
    
    tp = GM.applyEdgeUnsafe graphL x
    
    (graphSide, side1, side2) =
      case side of
        LeftSide -> (graphR, l, r)
        RightSide -> (graphL, r, l)
    
    typeSrc = GM.applyNodeUnsafe graphL src
    typeTgt = GM.applyNodeUnsafe graphL tgt
    
    x' = head (newEdges (domain graphK))
    x'' = head (newEdges (domain graphSide))
    
    newNodesK = newNodes (domain graphK)
    newNodesSide = newNodes (domain graphSide)
    
    invertSide1 = invertTGM side1
    
    srcInK = case applyNodeTGM invertSide1 src of
                    Just x -> x
                    Nothing -> newNodesK !! 0
    tgtInK = case applyNodeTGM invertSide1 tgt of
                    Just x -> x
                    Nothing -> newNodesK !! 1
    
    srcInR = case applyNodeTGM side2 srcInK of
                    Just x -> x
                    Nothing -> newNodesSide !! 0
    tgtInR = case applyNodeTGM side2 tgtInK of
                    Just x -> x
                    Nothing -> newNodesSide !! 1
    
    srcRight = createNodeCodTGM srcInR typeSrc side2
    tgtRight = createNodeCodTGM tgtInR typeTgt srcRight
    updateRight = createNodeDomTGM srcInK typeSrc srcInR tgtRight
    updateRight2 = createNodeDomTGM tgtInK typeTgt tgtInR updateRight
    updateRightCod = createEdgeCodTGM x'' srcInR tgtInR tp updateRight2
    updateRightMap = createEdgeDomTGM x' srcInK tgtInK tp x'' updateRightCod
    
    updateLeft = createNodeDomTGM srcInK typeSrc src side1
    updateLeft2 = createNodeDomTGM tgtInK typeTgt tgt updateLeft    
    updateLeftEdge = createEdgeDomTGM x' srcInK tgtInK tp x updateLeft2
    
    nacRule = production updateLeftEdge updateRightMap []
    mapL = idMap graphL (codomain updateLeft)
    mapK = idMap graphK (domain updateLeft)
    mapR = idMap graphR (codomain updateRightMap)

newNacsPairL :: SO.SndOrderRule a b -> [SO.RuleMorphism a b]
newNacsPairL sndRule = map createNac ret
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
    
    epis = calculateAllPartitions (codomain (left ruleL))
    
    ret = [e | e <- epis, any (\(a,b) -> (apply e) a == (apply e) b) pairL]
    
    createNac e = SO.ruleMorphism ruleL ruleNac e mapK mapR
      where
        ruleNac = production (compose (left ruleL) e) (right ruleL) []
        mapK = idMap (domain (left ruleL)) (domain (left ruleL))
        mapR = idMap (codomain (right ruleL)) (codomain (right ruleL))

calculateAllPartitions :: GM.TypedGraph a b -> [TypedGraphMorphism a b]
calculateAllPartitions graph = map fst (createPairs inj graph graphNull)
  where
    inj = False
    graphNull = GM.empty G.empty G.empty

orphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
orphanNode m n = n `elem` (orphanNodesTyped m)

orphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
orphanEdge m n = n `elem` (orphanEdgesTyped m)

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
