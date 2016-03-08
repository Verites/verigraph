module XML.GGXWriter where

import           Data.Maybe
import           Text.XML.HXT.Core
import           XML.ParsedTypes
import qualified Graph.Graph         as G
import qualified Graph.GraphGrammar  as GG
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphRule     as GR
import qualified Abstract.Morphism   as M
import qualified Graph.TypedGraphMorphism as TGM

writeGts :: ArrowXml a => GG.GraphGrammar b c -> a XmlTree XmlTree
writeGts grammar = mkelem "GraphTransformationSystem" defaultGtsAttributes $ writeGrammar grammar

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
  sattr"abstract" "false", sattr "name" $ completeEdgeName edgeType] []

writeGraph :: ArrowXml a => String -> String -> String -> [(String,String)]
              -> [(String, String, String, String)] -> a XmlTree XmlTree
writeGraph graphId kind name nodes edges =
  mkelem "Graph"
    [ sattr "ID" graphId, sattr "kind" kind, sattr "name" name ]
    $ writeNodes nodes ++ writeEdges edges

writeHostGraph :: ArrowXml a => a XmlTree XmlTree
writeHostGraph = writeGraph "Graph" "HOST" "Graph" [] []

writeNodes :: ArrowXml a => [(String,String)] -> [a XmlTree XmlTree]
writeNodes = map writeNode

writeNode :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeNode (nodeId, nodeType) =
  mkelem "Node"
    [ sattr "ID" nodeId, sattr "type" nodeType ]
    [ writeDefaultNodeLayout, writeAdditionalNodeLayout ]

writeEdges :: ArrowXml a => [(String,String,String,String)] -> [a XmlTree XmlTree]
writeEdges = map writeEdge

writeEdge :: ArrowXml a => (String,String,String,String) -> a XmlTree XmlTree
writeEdge (edgeId, edgeType, source, target) =
  mkelem "Edge"
    [ sattr "ID" edgeId, sattr "source" source, sattr "target" target
    , sattr "type" edgeType]
    [ writeDefaultEdgeLayout, writeAdditionalEdgeLayout ]

writeRules :: ArrowXml a => GG.GraphGrammar b c -> [a XmlTree XmlTree]
writeRules grammar = map writeRule $ GG.rules grammar

writeRule :: ArrowXml a => (String, GR.GraphRule b c) -> a XmlTree XmlTree
writeRule (ruleName, rule) =
  mkelem "Rule"
    [sattr "ID" ruleName, sattr "formula" "true", sattr "name" ruleName]
    $ [writeLHS ruleName lhs, writeRHS ruleName rhs] ++ [writeMorphism ruleName morphisms] ++ [ writeApplicationCondition]
  where
    lhs = getLHS rule
    rhs = getRHS rule
    morphisms = getMorphisms rule

getLHS :: GR.GraphRule a b -> ParsedTypedGraph
getLHS rule = serializeGraph $ GR.left rule

getRHS :: GR.GraphRule a b -> ParsedTypedGraph
getRHS rule = serializeGraph $ GR.right rule

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

getMorphisms :: GR.GraphRule a b -> [Morphism]
getMorphisms rule = nodesMorph ++ edgesMorph
  where
    invL = TGM.invertTGM (GR.left rule)
    lr = M.compose invL (GR.right rule)
    mapping = TGM.mapping lr
    nodeMap n = fromJust $ GM.applyNode mapping n
    nodes = filter (isJust . GM.applyNode mapping) (G.nodes (M.domain mapping))
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) nodes
    edgeMap e = fromJust $ GM.applyEdge mapping e
    edges = filter (isJust . GM.applyEdge mapping) (G.edges (M.domain mapping))
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) edges

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (_, nodes, edges) = writeGraph ("LeftOf_" ++ ruleName) "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (_, nodes, edges)= writeGraph ("RightOf_" ++ ruleName) "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => String -> [Morphism] -> a XmlTree XmlTree
writeMorphism name mappings =
  mkelem "Morphism"
  [sattr "comment" "Formula: true", sattr "name" name]
  $ writeMappings mappings

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeApplicationCondition :: ArrowXml a => a XmlTree XmlTree
writeApplicationCondition = mkelem "ApplCondition" [] $ writeNacs []

writeNac :: ArrowXml a => b -> a XmlTree XmlTree
writeNac _ = mkelem "NAC" [] []

writeNacs :: ArrowXml a => [b] -> [a XmlTree XmlTree]
writeNacs = map writeNac

-- writeDown :: IOSLA (XIOState s) XmlTree XmlTree
-- writeDown = root [] [writeRoot writeGts] >>> writeDocument [withIndent yes] "hellow.ggx

-- main = do
--   runX writeDown
--   return ()


--Functions to deal with ggx format specificities
writeRoot :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
writeRoot makebody = mkelem "Document" [sattr "version" "1.0"] [ makebody ]

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
