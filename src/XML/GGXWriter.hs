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

writeRoot :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
writeRoot makebody = mkelem "Document" [sattr "version" "1.0"] [ makebody ]

writeGts :: ArrowXml a => GG.GraphGrammar b c -> a XmlTree XmlTree
writeGts grammar = mkelem "GraphTransformationSystem" [ sattr "ID" "I1",
            sattr "directed" "true", sattr "name" "GraGra",
            sattr "parallel" "true" ]
            (writeProperties ++ [writeTypes (GG.typeGraph grammar)] ++ [writeHostGraph] ++ writeRules grammar)

defaultProperties :: [(String, String)]
defaultProperties = [("CSP","true"),("dangling","true"),("identification","true"),
  ("NACs","true"),("PACs","true"),("GACs","true"),("breakAllLayer","true"),
  ("showGraphAfterStep","true"),("TypeGraphLevel","ENABLED")]

defaultNodeNameComplement :: String
defaultNodeNameComplement = "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:"

defaultEdgeNameComplement :: String
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

cn :: String -> String
cn a = a ++ defaultNodeNameComplement

ce :: String -> String
ce a = a ++ defaultEdgeNameComplement

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
getLHS rule = ("", nodes, edges)
  where
    l = GR.left rule
    graphL = M.codomain l --typed graph --morphism
    typn n = fromJust $ GM.applyNode graphL n
    typed e = fromJust $ GM.applyEdge graphL e
    nodes = map (\n -> ("N" ++ show n, "N" ++ show (typn n))) (G.nodes (M.domain graphL))
    edges = map (\e -> ("E" ++ show e, "E" ++ show (typed e), "N" ++ show (src (M.domain graphL) e), "N" ++ show (tgt (M.domain graphL) e))) (G.edges $ M.domain graphL)

getRHS :: GR.GraphRule a b -> ParsedTypedGraph
getRHS rule = ("", nodes, edges)
  where
    r = GR.right rule
    graphR = M.codomain r --typed graph --morphism
    typn n = fromJust $ GM.applyNode graphR n
    typed e = fromJust $ GM.applyEdge graphR e
    nodes = map (\n -> ("N" ++ show n, "N" ++ show (typn n))) (G.nodes (M.domain graphR))
    edges = map (\e -> ("E" ++ show e, "E" ++ show (typed e), "N" ++ show (src (M.domain graphR) e), "N" ++ show (tgt (M.domain graphR) e))) (G.edges $ M.domain graphR)

getMorphisms :: GR.GraphRule a b -> [Morphism]
getMorphisms rule = nodesMorph ++ edgesMorph
  where
    invL = TGM.inverseTGM (GR.left rule)
    lr = M.compose invL (GR.right rule)
    mapping = TGM.mapping lr
    nodeMap n = fromJust $ GM.applyNode mapping n
    nodes = filter (\n -> GM.applyNode mapping n /= Nothing) (G.nodes (M.domain mapping))
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) nodes
    edgeMap e = fromJust $ GM.applyEdge mapping e
    edges = filter (\e -> GM.applyEdge mapping e /= Nothing) (G.edges (M.domain mapping))
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) edges

writeLHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeLHS ruleName (_, nodes, edges) = writeGraph ("LeftOf_" ++ ruleName) "LHS" ("LeftOf_" ++ ruleName) nodes edges

writeRHS :: ArrowXml a => String -> ParsedTypedGraph -> a XmlTree XmlTree
writeRHS ruleName (_, nodes, edges)= writeGraph ("RightOf_" ++ ruleName) "RHS" ("RightOf_" ++ ruleName) nodes edges

writeMorphism :: ArrowXml a => String -> [Morphism] -> a XmlTree XmlTree
writeMorphism name morphisms =
  mkelem "Morphism"
  [sattr "comment" "Formula: true", sattr "name" name]
  $ writeMappings morphisms

writeMappings :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeMappings = map writeMapping

writeMapping :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeMapping (image, orig) = mkelem "Mapping" [sattr "image" image, sattr "orig" orig] []

writeApplicationCondition :: ArrowXml a => a XmlTree XmlTree
writeApplicationCondition = mkelem "Graph" [] []

-- writeDown :: IOSLA (XIOState s) XmlTree XmlTree
-- writeDown = root [] [writeRoot writeGts] >>> writeDocument [withIndent yes] "hellow.ggx

-- main = do
--   runX writeDown
--   return ()
