module XML.GGXWriter where

import           Data.Maybe
import           Text.XML.HXT.Core
import           XML.ParsedTypes
import qualified CriticalPairs.CriticalPairs as CP
import qualified Graph.Graph         as G
import qualified Graph.GraphGrammar  as GG
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphRule     as GR
import qualified Abstract.Morphism   as M
import qualified Graph.TypedGraphMorphism as TGM

writeGts :: ArrowXml a => GG.GraphGrammar b c -> a XmlTree XmlTree
writeGts grammar = mkelem "GraphTransformationSystem" defaultGtsAttributes $ writeGrammar grammar

writeCpaOptions :: ArrowXml a => a XmlTree XmlTree
writeCpaOptions = mkelem "cpaOptions" cpaAttributes []

writeCriticalPairAnalysis :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeCriticalPairAnalysis rules overlappings = [writeCpaOptions, writeConflictContainer rules overlappings, writeConflictFreeContainer rules overlappings]

writeConflictContainer :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] ->  a XmlTree XmlTree
writeConflictContainer rules overlappings = mkelem "conflictContainer" [sattr "kind" "exclude"] $ (writeRuleSets rules ++ writeConflictMatrix rules overlappings)

writeConflictMatrix :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictMatrix rules overlappings = map (\r1@(name,rule) -> mkelem "Rule" [sattr "R1" name] (map calculateCP (overlappingsR2 name))) rules
  where
    overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings

calculateCP :: ArrowXml a => Overlappings -> a XmlTree XmlTree
calculateCP (n1,n2,overlappings) = if null overlappings then false else true
  where
    r2 = sattr "R2" n2
    attribs = [sattr "caIndx" "-1:", sattr "duIndx" "-1:", sattr "pfIndx" "-1:-1:"]
    false = mkelem "Rule" (r2 : (sattr "bool" "false") : attribs) []
    true  = mkelem "Rule" (r2 : (sattr "bool" "true")  : attribs) $ writeOverlappings (n1,n2,overlappings)

writeConflictFreeContainer :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> a XmlTree XmlTree
writeConflictFreeContainer rules overlappings = mkelem "conflictFreeContainer" [] $ writeConflictFreeMatrix rules overlappings

writeConflictFreeMatrix :: ArrowXml a => [(String,GR.GraphRule b c)] -> [Overlappings] -> [a XmlTree XmlTree]
writeConflictFreeMatrix rules overlappings = map (\r1@(name,rule) -> mkelem "Rule" [sattr "R1" name] (map calculateCP (overlappingsR2 name))) rules
  where
    overlappingsR2 r1 = filter (\(n1,_,_) -> n1 == r1) overlappings
    calculateCP (n1,n2,list) = if null list
                                 then mkelem "Rule" [sattr "R2" n2, sattr "bool" "true"] []
                                 else mkelem "Rule" [sattr "R2" n2, sattr "bool" "false"] []

writeRuleSets :: ArrowXml a => [(String,GR.GraphRule b c)] -> [a XmlTree XmlTree]
writeRuleSets rules = (mkelem "RuleSet" (somethingRules ++ rulesL) []) : (mkelem "RuleSet2" (somethingRules ++ rulesL) []) : []
  where
    somethingRules :: ArrowXml a => [a XmlTree XmlTree]
    somethingRules = map (\(x,(ruleName,_)) -> sattr ("i" ++(show x)) ruleName) (zip [0..] rules)
    rulesL :: ArrowXml a => [a XmlTree XmlTree]
    rulesL = [sattr "size" (show $ length rules)]

writeGrammar :: ArrowXml a => GG.GraphGrammar b c -> [a XmlTree XmlTree]
writeGrammar grammar = writeAggProperties ++ [writeTypes (GG.typeGraph grammar)] ++ [writeHostGraph] ++ writeRules grammar

writeTypes :: ArrowXml a => G.Graph b c -> a XmlTree XmlTree
writeTypes graph = mkelem "Types" []
  $ writeNodeTypes nodeTypeList ++ writeEdgeTypes edgeTypeList
  ++ [writeTypeGraph graph]
    where
      nodeTypeList = map (\n -> ("N" ++ show n, show n)) (G.nodes graph)
      edgeTypeList = map (\e -> ("E" ++ show e, show e)) (G.edges graph)

writeTypeGraph :: ArrowXml a => G.Graph b c -> a XmlTree XmlTree
writeTypeGraph graph = writeGraph "TypeGraph" "TG" "TypeGraph" nodeList edgeList
  where
    nodeList = map (\n -> ("n" ++ show n, "N" ++ show n)) (G.nodes graph)
    edgeList = map (\e -> ("e" ++ show e, "E" ++ show e, "n" ++ show (src graph e), "n" ++ show (tgt graph e))) (G.edges graph)

src :: G.Graph a b -> G.EdgeId -> G.NodeId
src g e = fromJust $ G.sourceOf g e

tgt :: G.Graph a b -> G.EdgeId -> G.NodeId
tgt g e = fromJust $ G.targetOf g e

writeNodeTypes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeNodeTypes = map writeNodeType

writeNodeType :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeNodeType (nodeId,nodeType) = mkelem "NodeType" [sattr "ID" nodeId,
  sattr"abstract" "false", sattr "name" $ completeNodeName nodeType] []

writeEdgeTypes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeEdgeTypes = map writeEdgeType

writeEdgeType :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeEdgeType (edgeId,edgeType) = mkelem "EdgeType" [sattr "ID" edgeId,
  sattr "abstract" "false", sattr "name" $ completeEdgeName edgeType] []

writeGraph :: ArrowXml a => String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraph graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodes nodes ++ writeEdges edges

writeGraphOverlaping :: ArrowXml a => String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraphOverlaping graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodesConflict graphId nodes ++ writeEdgesConflict graphId edges

writeOverlappings :: ArrowXml a => Overlappings -> [a XmlTree XmlTree]
writeOverlappings (n1,n2, overlaps) = map (\((graph,map1,map2),idx) -> writeOver (graph,map1,map2,idx)) (zip overlaps [0..])
  where
    writeOverGraph idx nodes edges = writeGraphOverlaping (graphId idx) "GRAPH" ("( "++show idx++ " ) " ++ "delete-use-conflict") nodes edges
    graphId idx = n1 ++ n2 ++ (show idx)
    writeOver ((_, nodes, edges), map1, map2, idx) =
      mkelem "Overlapping_Pair" []
        [writeOverGraph idx nodes edges,
         writeMorphism ("MorphOf_" ++ n1) "LHS" (mapAdjusted (graphId idx) map1),
         writeMorphism ("MorphOf_" ++ n2) "LHS" (mapAdjusted (graphId idx) map2)]
    mapAdjusted idx = map (\(x,y) -> (idx++"_"++x,y))

parseCPGraph :: (String,String,[CP.CriticalPair a b]) -> Overlappings
parseCPGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (\x -> (getGraph x,getMapM1 x,getMapM2 x)) cps
    getGraph x = serializeGraph (CP.getM1 x)
    getMapM1 x = getTgmMappings (CP.getM1 x)
    getMapM2 x = getTgmMappings (CP.getM2 x)

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

writeRules :: ArrowXml a => GG.GraphGrammar b c -> [a XmlTree XmlTree]
writeRules grammar = map writeRule $ GG.rules grammar

writeRule :: ArrowXml a => (String, GR.GraphRule b c) -> a XmlTree XmlTree
writeRule (ruleName, rule) =
  mkelem "Rule"
    [sattr "ID" ruleName, sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++ [writeMorphism ruleName "" morphism] ++ [writeConditions ruleName rule]
  where
    lhs = getLHS rule
    rhs = getRHS rule
    morphism = getMappings rule

getLHS :: GR.GraphRule a b -> ParsedTypedGraph
getLHS rule = serializeGraph $ GR.left rule

getRHS :: GR.GraphRule a b -> ParsedTypedGraph
getRHS rule = serializeGraph $ GR.right rule

getNacs :: String -> GR.GraphRule a b -> [(ParsedTypedGraph,[Mapping])]
getNacs ruleName rule = map getNac nacsWithIds
  where
    zipIds = zip ([0..]::[Int]) (GR.nacs rule)
    nacsWithIds = map (\(x,y) -> ("NAC_" ++ ruleName ++ "_" ++ show x, y)) zipIds

getNac :: (String, TGM.TypedGraphMorphism a b) -> (ParsedTypedGraph, [Mapping])
getNac (nacId,nac) = (graph, mappings)
  where
    (_,n,e) = serializeGraph nac
    graph = (nacId, n, e)
    mappings = getTgmMappings nac

serializeGraph :: TGM.TypedGraphMorphism a b -> ParsedTypedGraph
serializeGraph morphism = ("", nodes, edges)
  where
    graph = M.codomain morphism
    nodes = map (serializeNode graph) (G.nodes $ M.domain graph)
    edges = map (serializeEdge graph) (G.edges $ M.domain graph)

serializeNode :: GM.GraphMorphism a b -> G.NodeId -> ParsedTypedNode
serializeNode graph n = ("N" ++ show n, "N" ++ show (nodeType n))
  where
    nodeType node = fromJust $ GM.applyNode graph node

serializeEdge :: GM.GraphMorphism a b -> G.EdgeId -> ParsedTypedEdge
serializeEdge graph e = ("E" ++ show e, "E" ++ show (edgeType e), "N" ++ show (src (M.domain graph) e), "N" ++ show (tgt (M.domain graph) e))
  where
    edgeType edge = fromJust $ GM.applyEdge graph edge

getMappings :: GR.GraphRule a b -> [Mapping]
getMappings rule = nodesMorph ++ edgesMorph
  where
    invL = TGM.invertTGM (GR.left rule)
    lr = M.compose invL (GR.right rule)
    nodeMap n = fromJust $ TGM.applyNodeTGM lr n
    nodes = filter (isJust . TGM.applyNodeTGM lr) (TGM.nodesDomain lr)
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) nodes
    edgeMap e = fromJust $ TGM.applyEdgeTGM lr e
    edges = filter (isJust . TGM.applyEdgeTGM lr) (TGM.edgesDomain lr)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) edges

getTgmMappings :: TGM.TypedGraphMorphism a b -> [Mapping]
getTgmMappings nac = nodesMorph ++ edgesMorph
  where
    nodeMap n = fromJust $ TGM.applyNodeTGM nac n
    edgeMap e = fromJust $ TGM.applyEdgeTGM nac e
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) (TGM.nodesDomain nac)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) (TGM.edgesDomain nac)

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

writeConditions :: ArrowXml a => String -> GR.GraphRule b c -> a XmlTree XmlTree
writeConditions ruleName rule = mkelem "ApplCondition" [] $ map writeNac (getNacs ruleName rule)

writeNac :: ArrowXml a => (ParsedTypedGraph, [Mapping]) -> a XmlTree XmlTree
writeNac (nacGraph, nacMorphism) = mkelem "NAC" [] [writeNacGraph nacGraph, writeNacMorphism nacMorphism]
  where
    getId (nacId, _, _) = nacId
    writeNacGraph (nacId, nodes, edges) = writeGraph nacId "NAC" nacId nodes edges
    writeNacMorphism = writeMorphism (getId nacGraph) ""

--Functions to deal with ggx format specificities
writeRoot :: ArrowXml a => GG.GraphGrammar b c -> a XmlTree XmlTree
writeRoot gg = mkelem "Document" [sattr "version" "1.0"] [writeGts gg]

writeCpx :: ArrowXml a => GG.GraphGrammar b c -> [(String,String,[CP.CriticalPair b c])] -> a XmlTree XmlTree
writeCpx gg cp = mkelem "Document"
                   [sattr "version" "1.0"]
                   [mkelem "CriticalPairs"
                     [sattr "ID" "I0"]
                     ((writeGts gg) : writeCriticalPairAnalysis (GG.rules gg) parsedCP)]
  where
    parsedCP = map parseCPGraph cp

defaultGtsAttributes :: ArrowXml a => [a n XmlTree]
defaultGtsAttributes = [ sattr "ID" "I1", sattr "directed" "true", sattr "name" "GraGra", sattr "parallel" "true" ]

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
  mkelem "NodeLayout" [sattr "X" "455", sattr "Y" "241"] []

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

completeNodeName :: String -> String
completeNodeName a = a ++ defaultNodeNameComplement

completeEdgeName :: String -> String
completeEdgeName a = a ++ defaultEdgeNameComplement

defaultNodeNameComplement :: String
defaultNodeNameComplement = "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:"

defaultEdgeNameComplement :: String
defaultEdgeNameComplement = "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:"
