module XML.GGXWriter
  ( Grammars
  , writeGrammarFile
  , writeCpx
  ) where

import           Data.Maybe
import           Text.XML.HXT.Core
import           Data.List

import           Abstract.Category
import           Abstract.Rewriting.DPO
import qualified Analysis.CriticalPairs                as CP
import qualified Analysis.CriticalSequence             as CS
import           Category.TypedGraphRule
import qualified Data.Graphs                           as G
import           Data.TypedGraph                       (TypedGraph)
import           Data.TypedGraph.Morphism
import qualified Rewriting.DPO.TypedGraph              as GR
import           Rewriting.DPO.TypedGraph.GraphProcess ()
import qualified Rewriting.DPO.TypedGraphRule          as SO
import           XML.GGXParseOut
import           XML.ParsedTypes
import           XML.ParseSndOrderRule
import           XML.Utilities

type Grammars a b = (Grammar (TypedGraphMorphism a b), Grammar (RuleMorphism a b))

-- | Writes only the grammar (.ggx)
writeGrammarFile :: Grammars a b -> String -> [(String,String)] -> String -> IO ()
writeGrammarFile ggs name names fileName = do
  runX $ root [] [writeRoot ggs name names] >>> writeDocument [withIndent yes] fileName
  putStrLn $ "Saved in " ++ fileName
  return ()

--Functions to deal with ggx format specificities
writeRoot :: ArrowXml a => Grammars b c -> String -> [(String,String)] -> a XmlTree XmlTree
writeRoot ggs name names = mkelem "Document" [sattr "version" "1.0"] [writeGts ggs name names]

-- | Given a grammar, lists of conflicts and dependencies for each pair of
-- rules, a name for the grammar and a naming environment for the elements of
-- the grammar, return the structure of a @.cpx@ file.
writeCpx :: ArrowXml a => Grammars b c
         -> [(String,String,[CP.CriticalPair (TypedGraphMorphism b c)])]
         -> [(String,String,[CS.CriticalSequence (TypedGraphMorphism b c)])] -> String
         -> [(String,String)] -> a XmlTree XmlTree
writeCpx ggs@(gg1,_) cp cs name names = mkelem "Document"
                        [sattr "version" "1.0"]
                        [mkelem "CriticalPairs"
                        [sattr "ID" "I0"]
                          (writeGts ggs name names : writeCriticalPairAnalysis names (productions gg1) parsedCP parsedCS)]
  where
    parsedCP = map parseCPGraph cp
    parsedCS = map parseCSGraph cs

writeGts :: ArrowXml a => Grammars b c -> String -> [(String,String)] -> a XmlTree XmlTree
writeGts grammars name names = mkelem "GraphTransformationSystem" (sattr "name" name : defaultGtsAttributes) $ writeGrammar grammars names

writeCpaOptions :: ArrowXml a => a XmlTree XmlTree
writeCpaOptions = mkelem "cpaOptions" cpaAttributes []

writeCriticalPairAnalysis :: ArrowXml a => [(String,String)] -> [(String,GR.TypedGraphRule b c)] -> [Overlappings] -> [Overlappings] -> [a XmlTree XmlTree]
writeCriticalPairAnalysis names productions cpOL csOL = writeCpaOptions : conflictContainer ++ dependenceContainer
  where
    conflictContainer = if null cpOL then [] else
                          [writeConflictContainer "exclude" nacNames productions cpOL,
                           writeConflictFreeContainer productions cpOL]
    dependenceContainer = if null csOL then [] else
                           [writeConflictContainer "trigger_switch_dependency" nacNames productions csOL,
                            writeConflictFreeContainer productions csOL]
    nacNames = filter (\(x,_) -> isPrefixOf "NAC" x) names

writeConflictContainer :: ArrowXml a => String -> [(String,String)] -> [(String,GR.TypedGraphRule b c)] -> [Overlappings] ->  a XmlTree XmlTree
writeConflictContainer kind nacNames productions overlappings =
  mkelem elem [sattr "kind" kind] (writeRuleSets productions ++ writeConflictMatrix nacNames productions overlappings)
    where
      elem = case kind of
               "exclude"            -> "conflictContainer"
               "trigger_switch_dependency" -> "dependencyContainer"
               _ -> error $ "Unexpected kind of conflict/dependency: " ++ kind

writeConflictMatrix :: ArrowXml a => [(String,String)] -> [(String,GR.TypedGraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictMatrix nacNames productions overlappings =
  map (\(name,_) ->
         mkelem "Rule"
           [sattr "R1" name]
           (map (getCPs nacNames) (overlappingsR2 name)))
      productions
    where
      overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings

getCPs :: ArrowXml a => [(String,String)] -> Overlappings -> a XmlTree XmlTree
getCPs nacNames (n1,n2,overlappings) = if null overlappings then false else true
  where
    r2 = sattr "R2" n2
    attribs = [sattr "caIndx" "-1:", sattr "duIndx" "-1:", sattr "pfIndx" "-1:-1:"]
    false = mkelem "Rule" (r2 : sattr "bool" "false" : attribs) []
    true  = mkelem "Rule" (r2 : sattr "bool" "true"  : attribs) $ writeOverlappings nacNames (n1,n2,overlappings)

writeConflictFreeContainer :: ArrowXml a => [(String,GR.TypedGraphRule b c)] -> [Overlappings] -> a XmlTree XmlTree
writeConflictFreeContainer productions overlappings = mkelem "conflictFreeContainer" [] $ writeConflictFreeMatrix productions overlappings

writeConflictFreeMatrix :: ArrowXml a => [(String,GR.TypedGraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictFreeMatrix productions overlappings =
  map (\(name,_) -> mkelem "Rule"
                            [sattr "R1" name]
                            (map getCPs (overlappingsR2 name))) productions
    where
      overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings
      getCPs (_,n2,list) = if null list
                             then mkelem "Rule" [sattr "R2" n2, sattr "bool" "true"] []
                             else mkelem "Rule" [sattr "R2" n2, sattr "bool" "false"] []

writeRuleSets :: ArrowXml a => [(String,GR.TypedGraphRule b c)] -> [a XmlTree XmlTree]
writeRuleSets productions =
  [mkelem "RuleSet" (somethingRules ++ rulesL) [], mkelem "RuleSet2" (somethingRules ++ rulesL) []]
    where
      somethingRules :: ArrowXml a => [a XmlTree XmlTree]
      somethingRules = map (\(x,(ruleName,_)) -> sattr ("i" ++ show x) ruleName) (zip [0::Int ..] productions)
      rulesL :: ArrowXml a => [a XmlTree XmlTree]
      rulesL = [sattr "size" (show $ length productions)]

writeGrammar :: ArrowXml a => Grammars b c -> [(String,String)] -> [a XmlTree XmlTree]
writeGrammar (gg1,gg2) names = writeAggProperties ++
                             [writeTypes (XML.GGXWriter.typeGraph gg1) names] ++
                             [writeInitialGraph (start gg1)] ++
                             writeReachableGraphs gg1 ++
                             writeRules gg1 nacNames ++
                             writeSndOrderRules gg2
  where
    nacNames = filter (\(x,_) -> isPrefixOf "NAC" x) names

writeInitialGraph :: ArrowXml a => TypedGraph b c -> a XmlTree XmlTree
writeInitialGraph initial = writeHostGraph ("Init", initial)

writeReachableGraphs :: ArrowXml a => Grammar (TypedGraphMorphism b c) -> [a XmlTree XmlTree]
writeReachableGraphs gg = map writeHostGraph (reachableGraphs gg)
--    write (name,graph) writeGraph "initial_graph" "HOST" "Init" nodes edges
--    -- Reuses the serialize for productions to serialize the initial graph
--    tgm = makeInclusion initial initial
--    (_, nodes, edges) = serializeGraph [] [] tgm

writeHostGraph :: ArrowXml a => (String,TypedGraph b c) -> a XmlTree XmlTree
writeHostGraph (name,graph) = writeGraph ("graph_" ++ name) "HOST" name nodes edges
  where
    -- Reuses the serialize for productions to serialize the initial graph
    tgm = makeInclusion graph graph
    (_, nodes, edges) = serializeGraph [] [] tgm

writeTypes :: ArrowXml a => G.Graph b c -> [(String,String)] -> a XmlTree XmlTree
writeTypes graph names = mkelem "Types" []
  $ writeNodeTypes names nodeTypeList ++ writeEdgeTypes names edgeTypeList
  ++ [writeTypeGraph graph]
    where
      nodeTypeList = map (\n -> ("N" ++ show n, show n)) (G.nodeIds graph)
      edgeTypeList = map (\e -> ("E" ++ show e, show e)) (G.edgeIds graph)

writeTypeGraph :: ArrowXml a => G.Graph b c -> a XmlTree XmlTree
writeTypeGraph graph = writeGraph "TypeGraph" "TG" "TypeGraph" nodeList edgeList
  where
    nodeList = map (\n -> ("n" ++ show n, Nothing, "N" ++ show n)) (G.nodeIds graph)
    edgeList = map (\e -> ("e" ++ show (G.edgeId e), Nothing, "E" ++ show (G.edgeId e), "n" ++ show (G.sourceId e), "n" ++ show (G.targetId e))) (G.edges graph)

writeNodeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeNodeTypes names = map (writeNodeType names)

writeNodeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeNodeType names (nodeId,nodeType) =
  mkelem "NodeType"
    [sattr "ID" nodeId, sattr "abstract" "false", sattr "name" name] []
  where
    adjNames = map (first clearId) names
    name = if null searchName || length searchName > 1 then nodeType else head searchName
    searchName = [lbl | (id,lbl) <- adjNames, clearId nodeType == id, ":[NODE]:" `isInfixOf` lbl]

writeEdgeTypes :: ArrowXml a => [(String,String)] -> [(String,String)] -> [a XmlTree XmlTree]
writeEdgeTypes names = map (writeEdgeType names)

writeEdgeType :: ArrowXml a => [(String,String)] -> (String,String) -> a XmlTree XmlTree
writeEdgeType names (edgeId,edgeType) =
  mkelem "EdgeType"
    [sattr "ID" edgeId, sattr "abstract" "false", sattr "name" name] []
  where
    adjNames = map (first clearId) names
    name = if null searchName || length searchName > 1 then edgeType else head searchName
    searchName = [lbl | (id,lbl) <- adjNames, clearId edgeType == id, ":[EDGE]:" `isInfixOf` lbl]

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
    "RemoveDangling"  -> writeRemDangling
    "DeleteForbid"    -> writeDelFor nacNames
    "DeliverDelete"   -> writeDelDel
    "DeliverDangling" -> writeDelDangling
    "ForbidProduce"   -> writeForbProd nacNames
    _                 -> error $ "Unexpected type of overlapping: " ++ t)
  overlap

writeProdForbid :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeProdForbid nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "NAC+LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produce-forbid-verigraph-conflict (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ show idx ++ "_proforcon"
    nacCorrectName = fromMaybe nacName (lookup nacName nacNames)

writeProdDangling :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdDangling (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produceDangling-verigraph-conflict"
    graphId idx = n1 ++ n2 ++ show idx ++ "_prodelcon"

writeDeleteUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeDeleteUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "delete-use-verigraph-conflict"
    graphId idx = n1 ++ n2 ++ show idx ++ "_delusecon"

writeProdUse :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeProdUse (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "produce-use-verigraph-dependency"
    graphId idx = n1 ++ n2 ++ show idx ++ "_prousedep"

writeDelDel :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeDelDel (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "deliver-delete-verigraph-dependency"
    graphId idx = n1 ++ n2 ++ show idx ++ "_deldeldep"

writeRemDangling :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeRemDangling (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "remove-dangling-verigraph-dependency"
    graphId idx = n1 ++ n2 ++ show idx ++ "_remdandep"

writeDelDangling :: ArrowXml a => Overlapping -> a XmlTree XmlTree
writeDelDangling (n1, n2, ((_, nodes, edges), map1, map2, _, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) "" "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "RightOf_"++n1) ("MorphOf_" ++ n1) "RHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "delete-dangling-verigraph-dependency"
    graphId idx = n1 ++ n2 ++ show idx ++ "_deldandep"

writeDelFor :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeDelFor nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "NAC+LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "delete-forbid-verigraph-dependency (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ show idx ++ "_delfordep"
    nacCorrectName = fromMaybe nacName (lookup nacName nacNames)

writeForbProd :: ArrowXml a => [(String,String)] -> Overlapping -> a XmlTree XmlTree
writeForbProd nacNames (n1, n2, ((_, nodes, edges), map1, map2, nacName, _), idx) =
  mkelem "Overlapping_Pair" []
    [writeGraphOverlaping (graphId idx) nacCorrectName "GRAPH" msg nodes edges,
     writeMorphism (graphId idx, "LeftOf_"++n1) ("MorphOf_" ++ n1) "LHS" map1,
     writeMorphism (graphId idx, "LeftOf_"++n2) ("MorphOf_" ++ n2) "NAC+LHS" map2]
  where
    msg = "( "++show idx++ " ) " ++ "forbid-produce-verigraph-dependency (NAC: "++nacCorrectName++")"
    graphId idx = n1 ++ n2 ++ show idx ++ "_forprodep"
    nacCorrectName = fromMaybe nacName (lookup nacName nacNames)

writeNodes :: ArrowXml a => String -> [ParsedTypedNode] -> [a XmlTree XmlTree]
writeNodes graphId = map (writeNode graphId)

writeNode :: ArrowXml a => String -> ParsedTypedNode -> a XmlTree XmlTree
writeNode graphId (nodeId, objName, nodeType) =
  mkelem "Node"
   ([ sattr "ID" (graphId++"_"++nodeId) ] ++
    writeObjName objName ++
    [ sattr "type" nodeType ])
    [ writeDefaultNodeLayout, writeAdditionalNodeLayout ]

writeNodesConflict :: ArrowXml a => String -> [ParsedTypedNode] -> [a XmlTree XmlTree]
writeNodesConflict graphId = map (writeNodeConflict graphId)

writeNodeConflict :: ArrowXml a => String -> ParsedTypedNode -> a XmlTree XmlTree
writeNodeConflict graphId (nodeId, objName, nodeType) =
  mkelem "Node"
   ([sattr "ID" (graphId++"_"++nodeId) ] ++
    writeObjName objName ++
    [ sattr "type" nodeType ])
   []

writeEdges :: ArrowXml a => String -> [ParsedTypedEdge] -> [a XmlTree XmlTree]
writeEdges prefix = map (writeEdge prefix)

writeEdge :: ArrowXml a => String -> ParsedTypedEdge -> a XmlTree XmlTree
writeEdge prefix (edgeId, objName, edgeType, source, target) =
  mkelem "Edge"
   ([ sattr "ID" (prefix++"_"++edgeId) ] ++
    writeObjName objName ++
    [ sattr "source" (prefix++"_"++source),
      sattr "target" (prefix++"_"++target),
      sattr "type" edgeType])
    [ writeDefaultEdgeLayout, writeAdditionalEdgeLayout ]

writeObjName :: ArrowXml a => Maybe String -> [a XmlTree XmlTree]
writeObjName (Just n) = [sattr "name" n]
writeObjName _        = []

writeEdgesConflict :: ArrowXml a => String -> [ParsedTypedEdge] -> [a XmlTree XmlTree]
writeEdgesConflict graphId = map (writeEdgeConflict graphId)

writeEdgeConflict :: ArrowXml a => String -> ParsedTypedEdge -> a XmlTree XmlTree
writeEdgeConflict graphId (edgeId, objName, edgeType, source, target) =
  mkelem "Edge"
   ([ sattr "ID" (graphId++"_"++edgeId) ] ++
      writeObjName objName ++
    [ sattr "source" (graphId++"_"++source),
      sattr "target" (graphId++"_"++target),
      sattr "type" edgeType])
    []

writeSndOrderRules :: ArrowXml a => Grammar (RuleMorphism b c) -> [a XmlTree XmlTree]
writeSndOrderRules grammar = concatMap writeSndOrderRule (productions grammar)

writeSndOrderRule :: ArrowXml a => (String, SO.SndOrderRule b c) -> [a XmlTree XmlTree]
writeSndOrderRule (name, sndOrderRule) =
  [writeSndOrderRuleSide
    ("2rule_left_" ++ name)
    objNameMapLeftLeft objNameMapLeftLeft
    objNameMapLeftRight objNameMapLeftRight
    (leftMorphism sndOrderRule)] ++
  [writeSndOrderRuleSide
    ("2rule_right_" ++ name)
    objNameMapRightLeft objNameMapRightLeft
    objNameMapRightRight objNameMapRightRight
    (rightMorphism sndOrderRule)] ++
    map (\(n,idx) ->
          writeSndOrderRuleSide
            ("2rule_nac" ++ show idx ++ "_" ++ name)
            (objNameMapNacLeftN n)
            (objNameMapNacLeftE n)
            (objNameMapNacRightN n)
            (objNameMapNacRightE n)
            n)
       (zip (nacs sndOrderRule) ([0..] :: [Int]))
    where
      objNameMapNacLeftN n = getObjectNacNameMorphismNodes (mapping (mappingLeft n))
      objNameMapNacLeftE n = getObjectNacNameMorphismEdges (mapping (mappingLeft n))
      objNameMapNacRightN n = getObjectNacNameMorphismNodes (mapping (mappingRight n))
      objNameMapNacRightE n = getObjectNacNameMorphismEdges (mapping (mappingRight n))
      objNameMapRightLeft = getObjectNameMorphism (mappingLeft (leftMorphism sndOrderRule)) (mappingLeft (rightMorphism sndOrderRule))
      objNameMapRightRight = getObjectNameMorphism (mappingRight (leftMorphism sndOrderRule)) (mappingRight (rightMorphism sndOrderRule))
      graphLRuleL = codomain (mappingLeft (leftMorphism sndOrderRule))
      graphRRuleL = codomain (mappingRight (leftMorphism sndOrderRule))
      twice f x = f x x
      objNameMapLeftLeft = twice getObjectNameMorphism (makeInclusion graphLRuleL graphLRuleL)
      objNameMapLeftRight = twice getObjectNameMorphism (makeInclusion graphRRuleL graphRRuleL)

writeSndOrderRuleSide :: ArrowXml a => String -> [Mapping] -> [Mapping] -> [Mapping] -> [Mapping] -> RuleMorphism b c -> a XmlTree XmlTree
writeSndOrderRuleSide name objLeftN objLeftE objRightN objRightE ruleMorphism = writeRule objLeftN objLeftE objRightN objRightE [] (name, codomain ruleMorphism)

writeRules :: ArrowXml a => Grammar (TypedGraphMorphism b c) -> [(String,String)] -> [a XmlTree XmlTree]
writeRules grammar nacNames = map (writeRule [] [] [] [] nacNames) (productions grammar)

writeRule :: ArrowXml a => [Mapping] -> [Mapping] -> [Mapping] -> [Mapping] -> [(String,String)] -> (String, GR.TypedGraphRule b c) -> a XmlTree XmlTree
writeRule objNameLeftN objNameLeftE objNameRightN objNameRightE nacNames (ruleName, rule) =
  mkelem "Rule"
    [sattr "ID" ruleName, sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++
      [writeMorphism ("RightOf_"++ruleName, "LeftOf_"++ruleName) ruleName "" morphism] ++
      [writeConditions nacNames ruleName rule]
  where
    lhs = XML.GGXParseOut.getLHS objNameLeftN objNameLeftE rule
    rhs = XML.GGXParseOut.getRHS objNameRightN objNameRightE rule
    morphism = getMappings rule

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (_, nodes, edges) = writeGraph ("LeftOf_" ++ ruleName) "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (_, nodes, edges)= writeGraph ("RightOf_" ++ ruleName) "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => (String, String) -> String -> String -> [Mapping] -> a XmlTree XmlTree
writeMorphism (tgtPrefix, srcPrefix) name source mappings =
  mkelem "Morphism" (sattr "name" name : [sattr "source" source | source /= ""])
    $ writeMappings (map (\(tgt, elemSrcPrefix, src) ->
                           (tgtPrefix ++ "_" ++ tgt,
                            chooseSrcPrefix elemSrcPrefix ++ "_" ++ src)) mappings)
  where
    chooseSrcPrefix = fromMaybe srcPrefix

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeConditions :: ArrowXml a => [(String, String)] -> String -> GR.TypedGraphRule b c -> a XmlTree XmlTree
writeConditions nacNames ruleName rule =
  mkelem "ApplCondition" [] $ map (writeNac ruleName) (zip (getNacs ruleName rule) (map snd nacsRule++nacsNoName))
    where
      -- filter the name of the nacs of this rule
      nacsRule = filter (\(x,_) -> isPrefixOf ("NAC_"++ruleName) x) nacNames
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

typeGraph :: Grammar (TypedGraphMorphism a b) -> G.Graph (Maybe a) (Maybe b)
typeGraph = codomain . start
