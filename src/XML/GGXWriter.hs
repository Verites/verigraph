module XML.GGXWriter where

import           Text.XML.HXT.Core
import           XML.ParsedTypes

writeRoot :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
writeRoot makebody = mkelem "Document" [sattr "version" "1.0"] [ makebody ]

writeGts :: ArrowXml a => a XmlTree XmlTree
writeGts = mkelem "GraphTransformationSystem" [ sattr "ID" "I1",
            sattr "directed" "true", sattr "name" "GraGra",
            sattr "parallel" "true" ]
            (writeProperties ++ [writeTypes] ++ [writeHostGraph] ++ (writeRules []))

defaultProperties = [("CSP","true"),("dangling","true"),("identification","true"),
  ("NACs","true"),("PACs","true"),("GACs","true"),("breakAllLayer","true"),
  ("showGraphAfterStep","true"),("TypeGraphLevel","ENABLED")]

defaultNodeNameComplement = "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:"
defaultEdgeNameComplement = "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:"

writeProperties :: ArrowXml a => [a XmlTree XmlTree]
writeProperties = writeAttrHandler : writeTaggedValues defaultProperties

writeAttrHandler :: ArrowXml a => a XmlTree XmlTree
writeAttrHandler =
  mkelem "TaggedValue" [sattr "Tag" "AttrHandler", sattr "TagValue" "Java Expr"]
  $ writeTaggedValues [("Package", "java.lang"), ("Package", "java.util")]

writeTaggedValues :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeTaggedValues = map writeTaggedValue


writeTaggedValue :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeTaggedValue (tag, value) = mkelem "TaggedValue" [sattr "Tag" tag,
                              sattr "TagValue" value] []

writeTypes :: ArrowXml a => a XmlTree XmlTree
writeTypes = mkelem "Types" []
  $ writeNodeTypes nodeTypeTestList ++ writeEdgeTypes edgeTypeTestList
  ++ [writeTypeGraph]

writeTypeGraph :: ArrowXml a => a XmlTree XmlTree
writeTypeGraph = writeGraph "I11" "TG" "TypeGraph" nodeTestList edgeTestList

cn a = a ++ defaultNodeNameComplement
ce a = a ++ defaultEdgeNameComplement

nodeTypeTestList = [("I2","1"),("I3","2"),("I4","4"),("I5","3")]
edgeTypeTestList = [("I6","3"),("I7","1"),("I8","2"),("I9","4"),("I10","5")]

nodeTestList = [("I12","I2"),("I13","I3"),("I14","I5"),("I15","I4")]
edgeTestList = [("I16","I7","I14","I12"),("I17","I8","I13","I12")
                ,("I18","I6","I13","I14"),("I19","I9","I13","I15")
                ,("I20","I10","I14","I15")]

--Ajustar isso aqui para uma regra exemplo
rulesTesteList = [("sendMsg",("I24",[("I25","1"),("I26","3"),("I27","4")],[])]
                  ([],[("I28","1","I26","I25")]),([],[("I33","5","I26","I27")])

writeNodeTypes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeNodeTypes = map writeNodeType

writeNodeType :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeNodeType (nodeId,nodeType) = mkelem "NodeType" [sattr "ID" nodeId,
  sattr"abstract" "false", sattr "name" $ cn nodeType] []

writeEdgeTypes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeEdgeTypes = map writeEdgeType

writeEdgeType :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeEdgeType (edgeId,edgeType) = mkelem "EdgeType" [sattr "ID" edgeId,
  sattr"abstract" "false", sattr "name" $ ce edgeType] []

writeGraph :: ArrowXml a => String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraph graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodes nodes ++ writeEdges edges

writeHostGraph :: ArrowXml a => a XmlTree XmlTree
writeHostGraph = writeGraph "I21" "HOST" "Graph" [] []

writeNodes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeNodes = map writeNode

writeNode :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeNode (nodeId, nodeType) =
  mkelem "Node"
    [ sattr "ID" nodeId, sattr "type" nodeType ]
    [ writeDefaultNodeLayout, writeAdditionalNodeLayout ]

writeDefaultNodeLayout :: ArrowXml a => a XmlTree XmlTree
writeDefaultNodeLayout =
  mkelem "NodeLayout" [sattr "X" "455", sattr "Y" "241"] []

writeAdditionalNodeLayout :: ArrowXml a => a XmlTree XmlTree
writeAdditionalNodeLayout = mkelem "additionalLayout"
                              [ sattr "age" "0", sattr "force" "10"
                              , sattr "frozen" "true", sattr "zone" "50"] []

writeEdges :: ArrowXml a => [(String,String,String,String)] -> [a XmlTree XmlTree]
writeEdges = map writeEdge

writeEdge :: ArrowXml a => (String,String,String,String) -> a XmlTree XmlTree
writeEdge (edgeId, edgeType, source, target) =
  mkelem "Edge"
    [ sattr "ID" edgeId, sattr "source" source, sattr "target" target
    , sattr "type" edgeType]
    [ writeDefaultEdgeLayout, writeAdditionalEdgeLayout ]

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

writeRules :: ArrowXml a => [Rule] -> [a XmlTree XmlTree]
writeRules = map writeRule

writeRule :: ArrowXml a => Rule -> a XmlTree XmlTree
writeRule (ruleName, lhs, rhs, maps) =
  mkelem "Rule"
    [sattr "ID" "IDRULE", sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++  writeMappings maps ++ [ writeApplicationCondition]

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (gid, nodes, edges) = writeGraph "graphId" "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (gid, nodes, edges)= writeGraph "graphId" "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => String -> [(String, String)]-> a XmlTree XmlTree
writeMorphism name mappings =
  mkelem "Morphism"
  [sattr "comment" "Formula: true", sattr "name" name]
  $ writeMappings mappings

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeApplicationCondition :: ArrowXml a => a XmlTree XmlTree
writeApplicationCondition = mkelem "Graph" [] []

writeMain :: ArrowXml a => a XmlTree XmlTree
writeMain = writeRoot writeGts

writeDown :: IOSLA (XIOState s) XmlTree XmlTree
writeDown = root [] [writeRoot writeGts] >>> writeDocument [withIndent yes] "hellow.ggx"

main = do
  runX writeDown
  return ()
