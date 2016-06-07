module XML.GGXWriter
 ( writeConfDepFile,
   writeConflictsFile,
   writeDependenciesFile,
   writeSndOderConfDepFile,
   writeSndOderConflictsFile,
   writeSndOderDependenciesFile,
   writeGrammarFile
 ) where

import           Abstract.DPO
import           Abstract.Morphism         (codomain)
import qualified Analysis.CriticalPairs    as CP
import qualified Analysis.CriticalSequence as CS
import           Data.List.Utils           (startswith)
import           Data.Maybe
import qualified Graph.Graph               as G
import           Graph.GraphGrammar
import qualified Graph.GraphRule           as GR
import qualified Graph.SndOrderRule        as SO
import           Graph.RuleMorphism
import           Graph.TypedGraphMorphism
import           Text.XML.HXT.Core         hiding (left,right)
import           XML.GGXParseOut
import           XML.ParsedTypes
import           XML.ParseSndOrderRule
import           XML.Utilities

appendSndOrderConflicts :: Bool -> Bool -> GraphGrammar a b -> GraphGrammar a b
appendSndOrderConflicts nacInj inj gg = newGG
  where
    conflicts = CP.namedCriticalPairs nacInj inj (sndOrderRules gg)
    matches = concatMap (\(n1,n2,c) -> map (\ol -> (n1, n2, CP.getCP ol, codomain (fst (CP.getMatch ol)))) c) conflicts
    conflictRules = map (\(idx,(n1,n2,tp,rule)) -> ("conflict_"++(show tp)++"_"++n1++"_"++n2++"_"++(show idx), rule)) (zip ([0..]::[Int]) matches)
    newGG = graphGrammar (initialGraph gg) ((rules gg) ++ conflictRules) (sndOrderRules gg)

appendSndOrderDependencies :: Bool -> Bool -> GraphGrammar a b -> GraphGrammar a b
appendSndOrderDependencies nacInj inj gg = newGG
  where
    conflicts = CS.namedCriticalSequences nacInj inj (sndOrderRules gg)
    matches = concatMap (\(n1,n2,c) -> map (\ol -> (n1, n2, CS.getCS ol, codomain (fst (CS.getComatch ol)))) c) conflicts
    conflictRules = map (\(idx,(n1,n2,tp,rule)) -> ("dependency_"++(show tp)++"_"++n1++"_"++n2++"_"++(show idx), rule)) (zip ([0..]::[Int]) matches)
    newGG = graphGrammar (initialGraph gg) ((rules gg) ++ conflictRules) (sndOrderRules gg)

-- | Writes grammar, second order conflicts and dependencies (.ggx)
writeSndOderConfDepFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeSndOderConfDepFile nacInj inj gg name names fileName =
  do
    let newGG = ((appendSndOrderDependencies nacInj inj) . (appendSndOrderConflicts nacInj inj)) gg
    runX $ writeConfDep nacInj inj newGG name names fileName
    putStrLn $ "Saved in " ++ fileName
    return ()

-- | Writes the grammar and the second order conflicts (.ggx)
writeSndOderConflictsFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeSndOderConflictsFile nacInj inj gg name names fileName =
  do
    let newGG = appendSndOrderConflicts nacInj inj gg
    runX $ writeConf nacInj inj newGG name names fileName
    putStrLn $ "Saved in " ++ fileName
    return ()

-- | Writes the grammar and the second order dependencies (.ggx)
writeSndOderDependenciesFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeSndOderDependenciesFile nacInj inj gg name names fileName =
  do
    let newGG = appendSndOrderDependencies nacInj inj gg
    runX $ writeDep nacInj inj newGG name names fileName
    putStrLn $ "Saved in " ++ fileName
    return ()

-- | Writes grammar, conflicts and dependencies (.cpx)
writeConfDepFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeConfDepFile nacInj inj gg name names fileName = do
  runX $ writeConfDep nacInj inj gg name names fileName
  putStrLn $ "Saved in " ++ fileName
  return ()

-- | Writes the grammar and the conflicts (.cpx)
writeConflictsFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeConflictsFile nacInj inj gg name names fileName = do
  runX $ writeConf nacInj inj gg name names fileName
  putStrLn $ "Saved in " ++ fileName
  return ()

-- | Writes the grammar and the dependencies (.cpx)
writeDependenciesFile :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeDependenciesFile nacInj inj gg name names fileName = do
  runX $ writeDep nacInj inj gg name names fileName
  putStrLn $ "Saved in " ++ fileName
  return ()

-- | Writes only the grammar (.ggx)
writeGrammarFile :: GraphGrammar a b -> String -> [(String,String)] -> String -> IO ()
writeGrammarFile gg name names fileName = do
  runX $ root [] [writeRoot gg name names] >>> writeDocument [withIndent yes] fileName
  putStrLn $ "Saved in " ++ fileName
  return ()

writeConfDep :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IOSLA (XIOState s) XmlTree XmlTree
writeConfDep nacInj inj gg name names fileName = root [] [writeCpx gg cps css name names] >>> writeDocument [withIndent yes] fileName
  where
    cps = CP.namedCriticalPairs nacInj inj (rules gg)
    css = CS.namedCriticalSequences nacInj inj (rules gg)

writeConf :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IOSLA (XIOState s) XmlTree XmlTree
writeConf nacInj inj gg name names fileName = root [] [writeCpx gg cps [] name names] >>> writeDocument [withIndent yes] fileName
  where
    cps = CP.namedCriticalPairs nacInj inj (rules gg)

writeDep :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IOSLA (XIOState s) XmlTree XmlTree
writeDep nacInj inj gg name names fileName = root [] [writeCpx gg [] cps name names] >>> writeDocument [withIndent yes] fileName
  where
    cps = CS.namedCriticalSequences nacInj inj (rules gg)

--Functions to deal with ggx format specificities
writeRoot :: ArrowXml a => GraphGrammar b c -> String -> [(String,String)] -> a XmlTree XmlTree
writeRoot gg name names = mkelem "Document" [sattr "version" "1.0"] [writeGts gg name names]

writeCpx :: ArrowXml a => GraphGrammar b c
         -> [(String,String,[CP.CriticalPair (TypedGraphMorphism b c)])]
         -> [(String,String,[CS.CriticalSequence (TypedGraphMorphism b c)])] -> String
         -> [(String,String)] -> a XmlTree XmlTree
writeCpx gg cp cs name names = mkelem "Document"
                        [sattr "version" "1.0"]
                        [mkelem "CriticalPairs"
                        [sattr "ID" "I0"]
                          (writeGts gg name names : writeCriticalPairAnalysis names (rules gg) parsedCP parsedCS)]
  where
    parsedCP = map parseCPGraph cp
    parsedCS = map parseCSGraph cs

writeGts :: ArrowXml a => GraphGrammar b c -> String -> [(String,String)] -> a XmlTree XmlTree
writeGts grammar name names = mkelem "GraphTransformationSystem" (sattr "name" name : defaultGtsAttributes) $ writeGrammar grammar names

writeCpaOptions :: ArrowXml a => a XmlTree XmlTree
writeCpaOptions = mkelem "cpaOptions" cpaAttributes []

writeCriticalPairAnalysis :: ArrowXml a => [(String,String)] -> [(String,GR.GraphRule b c)] -> [Overlappings] -> [Overlappings] -> [a XmlTree XmlTree]
writeCriticalPairAnalysis names rules cpOL csOL = writeCpaOptions : conflictContainer ++ dependenceContainer
  where
    conflictContainer = if null cpOL then [] else
                          [writeConflictContainer "exclude" nacNames rules cpOL,
                           writeConflictFreeContainer rules cpOL]
    dependenceContainer = if null csOL then [] else
                           [writeConflictContainer "trigger_dependency" nacNames rules csOL,
                            writeConflictFreeContainer rules csOL]
    nacNames = filter (\(x,_) -> startswith "NAC" x) names

writeConflictContainer :: ArrowXml a => String -> [(String,String)] -> [(String,GR.GraphRule b c)] -> [Overlappings] ->  a XmlTree XmlTree
writeConflictContainer kind nacNames rules overlappings =
  mkelem elem [sattr "kind" kind] (writeRuleSets rules ++ writeConflictMatrix nacNames rules overlappings)
    where
      elem = case kind of
               "exclude"            -> "conflictContainer"
               "trigger_dependency" -> "dependencyContainer"
               _ -> error $ "Unexpected kind of conflict/dependency: " ++ kind

writeConflictMatrix :: ArrowXml a => [(String,String)] -> [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictMatrix nacNames rules overlappings =
  map (\(name,_) ->
         mkelem "Rule"
           [sattr "R1" name]
           (map (getCPs nacNames) (overlappingsR2 name)))
      rules
    where
      overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings

getCPs :: ArrowXml a => [(String,String)] -> Overlappings -> a XmlTree XmlTree
getCPs nacNames (n1,n2,overlappings) = if null overlappings then false else true
  where
    r2 = sattr "R2" n2
    attribs = [sattr "caIndx" "-1:", sattr "duIndx" "-1:", sattr "pfIndx" "-1:-1:"]
    false = mkelem "Rule" (r2 : sattr "bool" "false" : attribs) []
    true  = mkelem "Rule" (r2 : sattr "bool" "true"  : attribs) $ writeOverlappings nacNames (n1,n2,overlappings)

writeConflictFreeContainer :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> a XmlTree XmlTree
writeConflictFreeContainer rules overlappings = mkelem "conflictFreeContainer" [] $ writeConflictFreeMatrix rules overlappings

writeConflictFreeMatrix :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictFreeMatrix rules overlappings =
  map (\(name,_) -> mkelem "Rule"
                            [sattr "R1" name]
                            (map getCPs (overlappingsR2 name))) rules
    where
      overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings
      getCPs (_,n2,list) = if null list
                             then mkelem "Rule" [sattr "R2" n2, sattr "bool" "true"] []
                             else mkelem "Rule" [sattr "R2" n2, sattr "bool" "false"] []

writeRuleSets :: ArrowXml a => [(String,GR.GraphRule b c)] -> [a XmlTree XmlTree]
writeRuleSets rules =
  [mkelem "RuleSet" (somethingRules ++ rulesL) [], mkelem "RuleSet2" (somethingRules ++ rulesL) []]
    where
      somethingRules :: ArrowXml a => [a XmlTree XmlTree]
      somethingRules = map (\(x,(ruleName,_)) -> sattr ("i" ++ show x) ruleName) (zip [0::Int ..] rules)
      rulesL :: ArrowXml a => [a XmlTree XmlTree]
      rulesL = [sattr "size" (show $ length rules)]

writeGrammar :: ArrowXml a => GraphGrammar b c -> [(String,String)] -> [a XmlTree XmlTree]
writeGrammar grammar names = writeAggProperties ++
                             [writeTypes (typeGraph grammar) names] ++
                             [writeHostGraph] ++
                             (writeRules grammar nacNames) ++
                             (writeSndOrderRules grammar)
  where
    nacNames = filter (\(x,_) -> startswith "NAC" x) names

writeTypes :: ArrowXml a => G.Graph b c -> [(String,String)] -> a XmlTree XmlTree
writeTypes graph names = mkelem "Types" []
  $ writeNodeTypes names nodeTypeList ++ writeEdgeTypes names edgeTypeList
  ++ [writeTypeGraph graph]
    where
      nodeTypeList = map (\n -> ("N" ++ show n, show n)) (G.nodes graph)
      edgeTypeList = map (\e -> ("E" ++ show e, show e)) (G.edges graph)

writeTypeGraph :: ArrowXml a => G.Graph b c -> a XmlTree XmlTree
writeTypeGraph graph = writeGraph "TypeGraph" "TG" "TypeGraph" nodeList edgeList
  where
    src = G.sourceOfUnsafe
    tgt = G.targetOfUnsafe
    nodeList = map (\n -> ("n" ++ show n, Nothing, "N" ++ show n)) (G.nodes graph)
    edgeList = map (\e -> ("e" ++ show e, Nothing, "E" ++ show e, "n" ++ show (src graph e), "n" ++ show (tgt graph e))) (G.edges graph)

writeNodeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeNodeTypes names = map (writeNodeType names)

writeNodeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeNodeType names (nodeId,nodeType) =
  mkelem "NodeType"
    [sattr "ID" nodeId, sattr "abstract" "false", sattr "name" name] []
  where
    adjNames = map (\(x,y) -> (clearId x,y)) names
    name = fromMaybe nodeType (lookup (clearId nodeType) adjNames)

writeEdgeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeEdgeTypes names = map (writeEdgeType names)

writeEdgeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeEdgeType names (edgeId,edgeType) =
  mkelem "EdgeType"
    [sattr "ID" edgeId, sattr "abstract" "false", sattr "name" name] []
  where
    adjNames = map (\(x,y) -> (clearId x,y)) names
    name = fromMaybe edgeType (lookup (clearId edgeType) adjNames)

writeGraph :: ArrowXml a => String -> String -> String
              -> [ParsedTypedNode] -> [ParsedTypedEdge] -> a XmlTree XmlTree
writeGraph graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodes graphId nodes ++ writeEdges graphId edges

writeGraphOverlaping :: ArrowXml a => String -> String -> String -> String
              -> [ParsedTypedNode] -> [ParsedTypedEdge] -> a XmlTree XmlTree
writeGraphOverlaping graphId info kind name nodes edges =
  mkelem "Graph"
    (sattr "ID" graphId : attrInfo ++ [sattr "kind" kind, sattr "name" name])
    (writeNodesConflict graphId nodes ++ writeEdgesConflict graphId edges)
  where
    attrInfo = if info == "" then [] else [sattr "info" ("NAC:"++info)]

writeOverlappings :: ArrowXml a => [(String,String)] -> Overlappings -> [a XmlTree XmlTree]
writeOverlappings nacNames (n1, n2, overlaps) = map (\(x,y) -> writeOverlapping nacNames (n1,n2,x,y)) list
  where
    list = zip overlaps [1..]

writeOverlapping :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeOverlapping nacNames overlap@(_,_,(_,_,_,_,t),_) =
  (case t of
    "DeleteUse"       -> writeDeleteUse
    "ProduceDangling" -> writeProdDangling
    "ProduceForbid"   -> writeProdForbid nacNames
    "ProduceUse"      -> writeProdUse
    "DeliverDelete"   -> writeDelDel nacNames
    _ -> error $ "Unexpected type of overlapping: " ++ t)
  overlap

writeProdForbid :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeProdForbid nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "NAC+LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produce-forbid-conflict (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ show idx ++ "_proforcon"
    nacCorrectName = fromMaybe nacName (lookup nacName nacNames)

writeProdDangling :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdDangling (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produceEdge-deleteNode-conflict"
    graphId idx = n1 ++ n2 ++ show idx ++ "_prodelcon"

writeDeleteUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeDeleteUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "delete-use-conflict"
    graphId idx = n1 ++ n2 ++ show idx ++ "_delusecon"

writeProdUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produce-use-dependency"
    graphId idx = n1 ++ n2 ++ show idx ++ "_prousedep"

writeDelDel :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeDelDel nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "NAC+LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "delete-forbid-dependency (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ show idx ++ "_delfordep"
    nacCorrectName = fromMaybe nacName (lookup nacName nacNames)

writeHostGraph :: ArrowXml a => a XmlTree XmlTree
writeHostGraph = writeGraph "Graph" "HOST" "Graph" [] []

writeNodes :: ArrowXml a => String -> [ParsedTypedNode] -> [a XmlTree XmlTree]
writeNodes graphId = map (writeNode graphId)

writeNode :: ArrowXml a => String -> ParsedTypedNode -> a XmlTree XmlTree
writeNode graphId (nodeId, objName, nodeType) =
  mkelem "Node"
   ([ sattr "ID" (graphId++"_"++nodeId) ] ++
    (writeObjName objName) ++
    [ sattr "type" nodeType ])
    [ writeDefaultNodeLayout, writeAdditionalNodeLayout ]

writeNodesConflict :: ArrowXml a => String -> [ParsedTypedNode] -> [a XmlTree XmlTree]
writeNodesConflict graphId = map (writeNodeConflict graphId)

writeNodeConflict :: ArrowXml a => String -> ParsedTypedNode -> a XmlTree XmlTree
writeNodeConflict graphId (nodeId, objName, nodeType) =
  mkelem "Node"
   ([sattr "ID" (graphId++"_"++nodeId) ] ++
    (writeObjName objName) ++
    [ sattr "type" nodeType ])
   []

writeEdges :: ArrowXml a => String -> [ParsedTypedEdge] -> [a XmlTree XmlTree]
writeEdges prefix = map (writeEdge prefix)

writeEdge :: ArrowXml a => String -> ParsedTypedEdge -> a XmlTree XmlTree
writeEdge prefix (edgeId, objName, edgeType, source, target) =
  mkelem "Edge"
   ([ sattr "ID" (prefix++"_"++edgeId) ] ++
    (writeObjName objName) ++
    [ sattr "source" (prefix++"_"++source),
      sattr "target" (prefix++"_"++target),
      sattr "type" edgeType])
    [ writeDefaultEdgeLayout, writeAdditionalEdgeLayout ]

writeObjName :: ArrowXml a => Maybe String -> [a XmlTree XmlTree]
writeObjName (Just n) = [sattr "name" n]
writeObjName _ = []

writeEdgesConflict :: ArrowXml a => String -> [ParsedTypedEdge] -> [a XmlTree XmlTree]
writeEdgesConflict graphId = map (writeEdgeConflict graphId)

writeEdgeConflict :: ArrowXml a => String -> ParsedTypedEdge -> a XmlTree XmlTree
writeEdgeConflict graphId (edgeId, objName, edgeType, source, target) =
  mkelem "Edge"
   ([ sattr "ID" (graphId++"_"++edgeId) ] ++
      (writeObjName objName) ++
    [ sattr "source" (graphId++"_"++source),
      sattr "target" (graphId++"_"++target),
      sattr "type" edgeType])
    []

writeSndOrderRules :: ArrowXml a => GraphGrammar b c -> [a XmlTree XmlTree]
writeSndOrderRules grammar = concatMap writeSndOrderRule (sndOrderRules grammar)

writeSndOrderRule :: ArrowXml a => (String, SO.SndOrderRule b c) -> [a XmlTree XmlTree]
writeSndOrderRule (name, sndOrderRule) =
 ([writeSndOrderRuleSide
    ("2rule_left_" ++ name)
    objNameMapLeftLeft
    objNameMapLeftRight
    (left sndOrderRule)] ++
  [writeSndOrderRuleSide
    ("2rule_right_" ++ name)
    objNameMapRightLeft
    objNameMapRightRight
    (right sndOrderRule)] ++
  (map (\(n,idx) ->
          writeSndOrderRuleSide
            ("2rule_nac" ++ show idx ++ "_" ++ name)
            (objNameMapNacLeft n)
            (objNameMapNacRight n)
            n)
       (zip (nacs sndOrderRule) ([0..] :: [Int]))))
    where
      objNameMapNacLeft n = getObjectNacNameMorphism (mapping (mappingLeft n))
      objNameMapNacRight n = getObjectNacNameMorphism (mapping (mappingRight n))
      objNameMapRightLeft = getObjectNameMorphism (mappingLeft (left sndOrderRule)) (mappingLeft (right sndOrderRule))
      objNameMapRightRight = getObjectNameMorphism (mappingRight (left sndOrderRule)) (mappingRight (right sndOrderRule))
      graphLRuleL = codomain (mappingLeft (left sndOrderRule))
      graphRRuleL = codomain (mappingRight (left sndOrderRule))
      twice f x = f x x
      objNameMapLeftLeft = twice getObjectNameMorphism (idMap graphLRuleL graphLRuleL)
      objNameMapLeftRight = twice getObjectNameMorphism (idMap graphRRuleL graphRRuleL)

writeSndOrderRuleSide :: ArrowXml a => String -> [Mapping] -> [Mapping] -> RuleMorphism b c -> a XmlTree XmlTree
writeSndOrderRuleSide name objLeft objRight ruleMorphism = writeRule objLeft objRight [] (name, codomain ruleMorphism)

writeRules :: ArrowXml a => GraphGrammar b c -> [(String,String)] -> [a XmlTree XmlTree]
writeRules grammar nacNames = map (writeRule [] [] nacNames) (rules grammar)

writeRule :: ArrowXml a => [Mapping] -> [Mapping] -> [(String,String)] -> (String, GR.GraphRule b c) -> a XmlTree XmlTree
writeRule objNameLeft objNameRight nacNames (ruleName, rule) =
  mkelem "Rule"
    [sattr "ID" ruleName, sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++
      [writeMorphism ("RightOf_"++ruleName, "LeftOf_"++ruleName) ruleName "" morphism] ++
      [writeConditions nacNames ruleName rule]
  where
    lhs = getLHS objNameLeft rule
    rhs = getRHS objNameRight rule
    morphism = getMappings rule

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (_, nodes, edges) = writeGraph ("LeftOf_" ++ ruleName) "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (_, nodes, edges)= writeGraph ("RightOf_" ++ ruleName) "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => (String, String) -> String -> String -> [Mapping] -> a XmlTree XmlTree
writeMorphism (tgtPrefix, srcPrefix) name source mappings =
  mkelem "Morphism" (sattr "name" name : [sattr "source" source | source /= ""])
    $ writeMappings (map (\(tgt,elemSrcPrefix,src) ->
                           (tgtPrefix ++"_"++tgt,
                            (chooseSrcPrefix elemSrcPrefix)++"_"++src)) mappings)
  where
    chooseSrcPrefix = fromMaybe srcPrefix

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeConditions :: ArrowXml a => [(String, String)] -> String -> GR.GraphRule b c -> a XmlTree XmlTree
writeConditions nacNames ruleName rule =
  mkelem "ApplCondition" [] $ map (writeNac ruleName) (zip (getNacs ruleName rule) (map snd nacsRule++nacsNoName))
    where
      -- filter the name of the nacs of this rule
      nacsRule = filter (\(x,_) -> startswith ("NAC_"++ruleName) x) nacNames
      -- in the case of do not find, writes Nac_0,Nac_1,...
      nacsNoName = [a++b | a <- ["Nac_"], b <- map show [0::Int ..]]

writeNac :: ArrowXml a => String -> ((ParsedTypedGraph, [Mapping]),String) -> a XmlTree XmlTree
writeNac ruleName ((nacGraph@(nacId,_,_), nacMorphism),nacName) = mkelem "NAC" [] [writeNacGraph nacGraph, writeNacMorphism nacMorphism]
  where
    writeNacGraph (nacId, nodes, edges) = writeGraph nacId "NAC" nacName nodes edges
    writeNacMorphism = writeMorphism (nacId, "LeftOf_"++ruleName) nacName ""

defaultGtsAttributes :: ArrowXml a => [a n XmlTree]
defaultGtsAttributes = [ sattr "ID" "I1", sattr "directed" "true", sattr "parallel" "true" ]

writeAggProperties :: ArrowXml a => [a XmlTree XmlTree]
writeAggProperties = writeAttrHandler : writeTaggedValues defaultProperties

defaultProperties :: [(String, String)]
defaultProperties = [("CSP","true"),("dangling","true"),("identification","true"),
  ("NACs","true"),("PACs","true"),("GACs","true"),("breakAllLayer","true"),
  ("showGraphAfterStep","true"),("TypeGraphLevel","ENABLED")]

writeTaggedValues :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeTaggedValues = map writeTaggedValue

writeTaggedValue :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeTaggedValue (tag, value) = mkelem "TaggedValue" [sattr "Tag" tag,
                              sattr "TagValue" value] []

writeAttrHandler :: ArrowXml a => a XmlTree XmlTree
writeAttrHandler =
  mkelem "TaggedValue" [sattr "Tag" "AttrHandler", sattr "TagValue" "Java Expr"]
  $ writeTaggedValues [("Package", "java.lang"), ("Package", "java.util")]

cpaAttributes :: ArrowXml a => [a n XmlTree]
cpaAttributes = [sattr "complete" "true", sattr "consistent" "false", sattr "directlyStrictConfluent" "false",
                sattr "directlyStrictConfluentUpToIso" "false", sattr "essential" "false",
                sattr "ignoreSameMatch" "false", sattr "ignoreSameRule" "false", sattr "maxBoundOfCriticCause" "0",
                sattr "namedObject" "false", sattr "strongAttrCheck" "false"]

writeDefaultNodeLayout :: ArrowXml a => a XmlTree XmlTree
writeDefaultNodeLayout =
  mkelem "NodeLayout" [sattr "X" "50", sattr "Y" "50"] []

writeAdditionalNodeLayout :: ArrowXml a => a XmlTree XmlTree
writeAdditionalNodeLayout = mkelem "additionalLayout"
                              [ sattr "age" "0", sattr "force" "10"
                              , sattr "frozen" "true", sattr "zone" "50"] []

writeDefaultEdgeLayout :: ArrowXml a => a XmlTree XmlTree
writeDefaultEdgeLayout =
  mkelem "EdgeLayout"
    [ sattr "bendX" "0", sattr "bendY" "0"
    , sattr "sourceMultiplicityOffsetX" "-6"
    , sattr "sourceMultiplicityOffsetY" "15"
    , sattr "targetMultiplicityOffsetX" "-6"
    , sattr "targetMultiplicityOffsetY" "7"
    , sattr "textOffsetX" "0", sattr "textOffsetY" "-22" ] []

writeAdditionalEdgeLayout :: ArrowXml a => a XmlTree XmlTree
writeAdditionalEdgeLayout =
  mkelem "additionalLayout"
  [ sattr "aktlength" "200", sattr "force" "10", sattr "preflength" "200" ] []
