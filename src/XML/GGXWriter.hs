module XML.GGXWriter 
 ( writeConfDepFile,
   writeConflictsFile,
   writeDependenciesFile,
   writeGrammarFile
 ) where

import qualified Abstract.Morphism as M
import           Data.List.Utils (startswith)
import           Data.Maybe
import           Text.XML.HXT.Core
import qualified Analysis.CriticalPairs as CP
import qualified Analysis.CriticalSequence as CS
import qualified Graph.Graph as G
import           Graph.GraphGrammar
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphRule as GR
import           Graph.TypedGraphMorphism
import           XML.GGXParseOut
import           XML.ParsedTypes

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
    css = CS.namedCriticalSequence nacInj inj (rules gg)

writeConf :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IOSLA (XIOState s) XmlTree XmlTree
writeConf nacInj inj gg name names fileName = root [] [writeCpx gg cps [] name names] >>> writeDocument [withIndent yes] fileName
  where
    cps = CP.namedCriticalPairs nacInj inj (rules gg)

writeDep :: Bool -> Bool -> GraphGrammar a b -> String -> [(String,String)] -> String -> IOSLA (XIOState s) XmlTree XmlTree
writeDep nacInj inj gg name names fileName = root [] [writeCpx gg [] cps name names] >>> writeDocument [withIndent yes] fileName
  where
    cps = CS.namedCriticalSequence nacInj inj (rules gg)

--Functions to deal with ggx format specificities
writeRoot :: ArrowXml a => GraphGrammar b c -> String -> [(String,String)] -> a XmlTree XmlTree
writeRoot gg name names = mkelem "Document" [sattr "version" "1.0"] [writeGts gg name names]

writeCpx :: ArrowXml a => GraphGrammar b c -> [(String,String,[CP.CriticalPair b c])] -> [(String,String,[CS.CriticalSequence b c])] -> String -> [(String,String)] -> a XmlTree XmlTree
writeCpx gg cp cs name names = mkelem "Document"
                        [sattr "version" "1.0"]
                        [mkelem "CriticalPairs"
                        [sattr "ID" "I0"]
                          ((writeGts gg name names) : (writeCriticalPairAnalysis names (rules gg) parsedCP parsedCS))]
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
    nacNames = filter (\(x,y) -> startswith "NAC" x) names

writeConflictContainer :: ArrowXml a => String -> [(String,String)] -> [(String,GR.GraphRule b c)] -> [Overlappings] ->  a XmlTree XmlTree
writeConflictContainer kind nacNames rules overlappings = mkelem elem [sattr "kind" kind] $ (writeRuleSets rules ++ writeConflictMatrix nacNames rules overlappings)
  where
    elem = case kind of 
             "exclude"            -> "conflictContainer"
             "trigger_dependency" -> "dependencyContainer"

writeConflictMatrix :: ArrowXml a => [(String,String)] -> [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictMatrix nacNames rules overlappings =
  map (\r1@(name,rule) ->
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
    false = mkelem "Rule" (r2 : (sattr "bool" "false") : attribs) []
    true  = mkelem "Rule" (r2 : (sattr "bool" "true")  : attribs) $ writeOverlappings nacNames (n1,n2,overlappings)

writeConflictFreeContainer :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> a XmlTree XmlTree
writeConflictFreeContainer rules overlappings = mkelem "conflictFreeContainer" [] $ writeConflictFreeMatrix rules overlappings

writeConflictFreeMatrix :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictFreeMatrix rules overlappings =
  map (\r1@(name,rule) -> mkelem "Rule"
                            [sattr "R1" name]
                            (map getCPs (overlappingsR2 name))) rules
    where
      overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings
      getCPs (_,n2,list) = if null list
                             then mkelem "Rule" [sattr "R2" n2, sattr "bool" "true"] []
                             else mkelem "Rule" [sattr "R2" n2, sattr "bool" "false"] []

writeRuleSets :: ArrowXml a => [(String,GR.GraphRule b c)] -> [a XmlTree XmlTree]
writeRuleSets rules = (mkelem "RuleSet" (somethingRules ++ rulesL) []) : (mkelem "RuleSet2" (somethingRules ++ rulesL) []) : []
  where
    somethingRules :: ArrowXml a => [a XmlTree XmlTree]
    somethingRules = map (\(x,(ruleName,_)) -> sattr ("i" ++(show x)) ruleName) (zip [0..] rules)
    rulesL :: ArrowXml a => [a XmlTree XmlTree]
    rulesL = [sattr "size" (show $ length rules)]

writeGrammar :: ArrowXml a => GraphGrammar b c -> [(String,String)] -> [a XmlTree XmlTree]
writeGrammar grammar names = writeAggProperties ++ [writeTypes (typeGraph grammar) names] ++ [writeHostGraph] ++ (writeRules grammar nacNames)
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
    nodeList = map (\n -> ("n" ++ show n, "N" ++ show n)) (G.nodes graph)
    edgeList = map (\e -> ("e" ++ show e, "E" ++ show e, "n" ++ show (src graph e), "n" ++ show (tgt graph e))) (G.edges graph)

writeNodeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeNodeTypes names = map (writeNodeType names)

writeNodeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeNodeType names (nodeId,nodeType) =
  mkelem "NodeType"
    [sattr "ID" nodeId, sattr "abstract" "false", sattr "name" name] []
  where
    name = case lookup ("I"++nodeType) names of
             Just n -> n
             Nothing ->nodeType 

writeEdgeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeEdgeTypes names = map (writeEdgeType names)

writeEdgeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeEdgeType names (edgeId,edgeType) =
  mkelem "EdgeType"
    [sattr "ID" edgeId, sattr "abstract" "false", sattr "name" name] []
  where
    name = case lookup ("I"++edgeType) names of
             Just n -> n
             Nothing -> edgeType 

writeGraph :: ArrowXml a => String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraph graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodes nodes ++ writeEdges edges

writeGraphOverlaping :: ArrowXml a => String -> String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraphOverlaping graphId info kind name nodes edges =
  mkelem "Graph"
    ((sattr "ID" graphId) : attrInfo ++ [sattr "kind" kind, sattr "name" name])
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
    "DeleteUse"             -> writeDeleteUse
    "ProduceEdgeDeleteNode" -> writeProdNode
    "ProduceForbid"         -> writeProdForbid nacNames
    "ProduceUse"            -> writeProdUse
    "DeliverDelete"         -> writeDelDel nacNames)
  overlap

writeProdForbid :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeProdForbid nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism ("MorphOf_" ++ n1) "RHS" (mapAdjusted (graphId idx) map1),
     writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
  where
    msg = "( "++show idx++ " ) " ++ "produce-forbid-conflict (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ (show idx) ++ "_proforcon"
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))
    nacCorrectName = case lookup nacName nacNames of
                       Just n -> n
                       Nothing -> nacName

writeProdNode :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdNode (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism ("MorphOf_" ++ n1) "LHS" (mapAdjusted (graphId idx) map1),
     writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
  where
    msg = "( "++show idx++ " ) " ++ "produceEdge-deleteNode-conflict"
    graphId idx = n1 ++ n2 ++ (show idx) ++ "_prodelcon"
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))

writeDeleteUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeDeleteUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism ("MorphOf_" ++ n1) "LHS" (mapAdjusted (graphId idx) map1),
     writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
  where
    msg = "( "++show idx++ " ) " ++ "delete-use-conflict"
    graphId idx = n1 ++ n2 ++ (show idx) ++ "_delusecon"
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))

writeProdUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism ("MorphOf_" ++ n1) "RHS" (mapAdjusted (graphId idx) map1),
     writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
  where
    msg = "( "++show idx++ " ) " ++ "produce-use-dependency"
    graphId idx = n1 ++ n2 ++ (show idx) ++ "_prousedep"
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))

writeDelDel :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeDelDel nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism ("MorphOf_" ++ n1) "RHS" (mapAdjusted (graphId idx) map1),
     writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
  where
    msg = "( "++show idx++ " ) " ++ "delete-forbid-conflict (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ (show idx) ++ "_delfordep"
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))
    nacCorrectName = case lookup nacName nacNames of
                       Just n -> n
                       Nothing -> nacName

writeHostGraph :: ArrowXml a => a XmlTree XmlTree
writeHostGraph = writeGraph "Graph" "HOST" "Graph" [] []

writeNodes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeNodes = map writeNode

writeNode :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeNode (nodeId, nodeType) =
  mkelem "Node"
    [ sattr "ID" nodeId, sattr "type" nodeType ]
    [ writeDefaultNodeLayout, writeAdditionalNodeLayout ]

writeNodesConflict :: ArrowXml a => String -> [(String,String)] -> [a XmlTree XmlTree]
writeNodesConflict graphId = map (writeNodeConflict graphId)

writeNodeConflict :: ArrowXml a => String -> (String,String) -> a XmlTree XmlTree
writeNodeConflict graphId (nodeId, nodeType) =
  mkelem "Node"
    [sattr "ID" (graphId++"_"++nodeId), sattr "type" nodeType] []

writeEdges :: ArrowXml a => [(String,String,String,String)] -> [a XmlTree XmlTree]
writeEdges = map writeEdge

writeEdge :: ArrowXml a => (String,String,String,String) -> a XmlTree XmlTree
writeEdge (edgeId, edgeType, source, target) =
  mkelem "Edge"
    [ sattr "ID" edgeId,
      sattr "source" source,
      sattr "target" target,
      sattr "type" edgeType]
    [ writeDefaultEdgeLayout, writeAdditionalEdgeLayout ]

writeEdgesConflict :: ArrowXml a => String -> [(String,String,String,String)] -> [a XmlTree XmlTree]
writeEdgesConflict graphId = map (writeEdgeConflict graphId)

writeEdgeConflict :: ArrowXml a => String -> (String,String,String,String) -> a XmlTree XmlTree
writeEdgeConflict graphId (edgeId, edgeType, source, target) =
  mkelem "Edge"
    [ sattr "ID" (graphId++"_"++edgeId),
      sattr "source" (graphId++"_"++source),
      sattr "target" (graphId++"_"++target),
      sattr "type" edgeType] []

writeRules :: ArrowXml a => GraphGrammar b c -> [(String,String)] -> [a XmlTree XmlTree]
writeRules grammar nacNames = map (writeRule nacNames) (rules grammar)

writeRule :: ArrowXml a => [(String,String)] -> (String, GR.GraphRule b c) -> a XmlTree XmlTree
writeRule nacNames (ruleName, rule) =
  mkelem "Rule"
    [sattr "ID" ruleName, sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++ [writeMorphism ruleName "" morphism] ++ [writeConditions nacNames ruleName rule]
  where
    lhs = getLHS rule
    rhs = getRHS rule
    morphism = getMappings rule

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (_, nodes, edges) = writeGraph ("LeftOf_" ++ ruleName) "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (_, nodes, edges)= writeGraph ("RightOf_" ++ ruleName) "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => String -> String -> [Mapping] -> a XmlTree XmlTree
writeMorphism name source mappings =
  mkelem "Morphism"
  ([sattr "name" name] ++
  (if source /= "" then [sattr "source" source] else []))
  $ writeMappings mappings

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeConditions :: ArrowXml a => [(String, String)] -> String -> GR.GraphRule b c -> a XmlTree XmlTree
writeConditions nacNames ruleName rule = mkelem "ApplCondition" [] $ map writeNac (zip (getNacs ruleName rule) (map snd nacsRule))
  where
    nacsRule = filter (\(x,_) -> startswith ("NAC_"++ruleName) x) nacNames

writeNac :: ArrowXml a => ((ParsedTypedGraph, [Mapping]),String) -> a XmlTree XmlTree
writeNac ((nacGraph, nacMorphism),nacName) = mkelem "NAC" [] [writeNacGraph nacGraph, writeNacMorphism nacMorphism]
  where
    writeNacGraph (nacId, nodes, edges) = writeGraph nacId "NAC" nacName nodes edges
    writeNacMorphism = writeMorphism nacName ""

src :: G.Graph a b -> G.EdgeId -> G.NodeId
src g e = fromJust $ G.sourceOf g e

tgt :: G.Graph a b -> G.EdgeId -> G.NodeId
tgt g e = fromJust $ G.targetOf g e

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

