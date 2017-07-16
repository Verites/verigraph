{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXParseIn
 ( parseGGName
 , parseNames
 , parseTypeGraph
 , parseGraphs
 , parseNacNames
 , parseRule
 , parseRuleSequence
 , parseAtomicConstraints
 , parseGraphConstraints
 ) where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.Formulas
import           XML.ParsedTypes
import           XML.Utilities            (clearIdUnsafe)
import           XML.XMLUtilities

clearId :: String -> String
clearId = clearIdUnsafe

-- | Parse the name of the Grammar
parseGGName :: ArrowXml cat => cat (NTree XNode) String
parseGGName = atTag "GraphTransformationSystem" >>>
  proc ggname -> do
    name <- getAttrValue "name" -< ggname
    returnA -< name

-- | Parse all type graphs
parseTypeGraph :: ArrowXml cat => cat (NTree XNode) ParsedTypeGraph
parseTypeGraph = atTag "Types" >>> atTag "Graph" >>>
  proc graph -> do
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (nodes, edges)

-- | Parse all NAC names in the format: "(NAC_(ruleName)_(index) , real NAC name)"
parseNacNames :: ArrowXml cat => cat (NTree XNode) [(String,String)]
parseNacNames = atTag "Rule" >>>
  proc graph -> do
    ruleName <- getAttrValue "name" -< graph
    nacNames <- listA parseNacNamess -< graph
    let l = map (\(x,y) -> ("NAC_"++ruleName++"_"++x,y)) (concat nacNames)
    returnA -< l

parseNacNamess :: ArrowXml cat => cat (NTree XNode) [(String,String)]
parseNacNamess = atTag "ApplCondition" >>>
  proc graph -> do
    let l = map show [0::Int ..]
    nacNames <- listA parseNacIfEnabled -< graph
    let a = zip l nacNames
    returnA -< a

parseNacIfEnabled :: ArrowXml cat => cat (NTree XNode) String
parseNacIfEnabled = atTag "NAC" >>>
  proc nac -> do
    _  <- isA (\str -> str == "" || str == "true") <<< getAttrValue "enabled" -< nac
    ret <- parseNacName -< nac
    returnA -< ret

parseNacName :: ArrowXml cat => cat (NTree XNode) String
parseNacName = atTag "Graph" >>>
  proc nacName -> do
    nacNam <- getAttrValue "name" -< nacName
    returnA -< nacNam

-- | Parse the names of the elements (Nodes and Edges)
parseNames :: ArrowXml cat => cat (NTree XNode) [(String,String)]
parseNames = atTag "Types" >>>
  proc graph -> do
    nodes <- listA parseNodeNames -< graph
    edges <- listA parseEdgeNames -< graph
    returnA -< (nodes ++ edges)

parseNodeNames :: ArrowXml cat => cat (NTree XNode) (String,String)
parseNodeNames = atTag "NodeType" >>>
  proc nodeName -> do
    nodeId <- getAttrValue "ID" -< nodeName
    nodeNam <- getAttrValue "name" -< nodeName
    returnA -< (nodeId, nodeNam)

parseEdgeNames :: ArrowXml cat => cat (NTree XNode) (String,String)
parseEdgeNames = atTag "EdgeType" >>>
  proc edgeName -> do
    edgeId <- getAttrValue "ID" -< edgeName
    edgeNam <- getAttrValue "name" -< edgeName
    returnA -< (edgeId, edgeNam)

parseNode :: ArrowXml cat => cat (NTree XNode) ParsedTypedNode
parseNode = atTag "Node" >>>
  proc node -> do
    nodeId <- getAttrValue "ID" -< node
    nodeName <- getAttrValue "name" -< node
    let setNodeName = case nodeName of
                       ""  -> Nothing
                       str -> Just str
    nodeType <- getAttrValue "type" -< node
    returnA -< (clearId nodeId, setNodeName, clearId nodeType)

parseEdge :: ArrowXml cat => cat (NTree XNode) ParsedTypedEdge
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeId <- getAttrValue "ID" -< node
    edgeName <- getAttrValue "name" -< node
    let setEdgeName = case edgeName of
                        ""  -> Nothing
                        str -> Just str
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    returnA -< (clearId edgeId, setEdgeName, clearId edgeType, clearId edgeSource, clearId edgeTarget)

-- | Given a GTS tag, parses all instance graphs
parseGraphs :: ArrowXml cat => cat (NTree XNode) [ParsedTypedGraph]
parseGraphs =
  atTag "GraphTransformationSystem" >>> listA (getChildren >>> isElem >>> hasName "Graph" >>> parseGraph)

parseGraph :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseGraph = atTag "Graph" >>>
  proc graph -> do
    graphId <- getAttrValue "ID" -< graph
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (clearId graphId, nodes, edges)

parseAtomicConstraints :: ArrowXml cat => cat (NTree XNode) ParsedAtomicConstraint
parseAtomicConstraints = atTag "Graphconstraint_Atomic" >>>
  proc atomic -> do
    name <- getAttrValue "name" -< atomic
    premise <- parseGraph <<< atTag "Premise" -< atomic
    conclusion <- parseGraph <<< atTag "Conclusion" -< atomic
    morphisms <- parseMorphism <<< atTag "Conclusion" -< atomic
    returnA -< (name, premise, conclusion, morphisms)

parseGraphConstraints :: ArrowXml cat => cat (NTree XNode) (String, Formula)
parseGraphConstraints = atTag "Formula" >>>
  proc constraint -> do
    name <- getAttrValue "name" -< constraint
    _ <- isA (/= "T") <<< getAttrValue "f" -< constraint -- discards agg not well formed formulas
    formula <- getAttrValue "f" -< constraint
    returnA -< (name, parseFormula formula)

-- | Parse all enabled rules of first-order
parseRule :: ArrowXml cat => cat (NTree XNode) RuleWithNacs
parseRule = atTag "Rule" >>>
  proc rule -> do
    ruleName <- getAttrValue "name" -< rule
    _  <- isA (\str -> str == "" || str == "true") <<< getAttrValue "enabled" -< rule
    lhs <- parseSideRule "LHS" -< rule
    rhs <- parseSideRule "RHS" -< rule
    morphism <- parseMorphism -< rule
    nacs <- listA parseNac -< rule
    returnA -< ((ruleName, lhs, rhs, morphism), nacs)

parseSideRule :: ArrowXml cat => String -> cat (NTree XNode) ParsedTypedGraph
parseSideRule str = atTag "Graph" >>>
  proc graph -> do
    _ <- isA (str ==) <<< getAttrValue "kind"-< graph
    lhs <- parseGraph -< graph
    returnA -< lhs

parseMorphism :: ArrowXml cat => cat (NTree XNode) [Mapping]
parseMorphism = getChildren >>> isElem >>> hasName "Morphism" >>>
  proc morphism -> do
    maps <- listA parseMappings -< morphism
    returnA -< maps

parseMappings :: ArrowXml cat => cat (NTree XNode) Mapping
parseMappings = atTag "Mapping" >>>
  proc mapping -> do
    image <- getAttrValue "image" -< mapping
    orig <- getAttrValue "orig" -< mapping
    returnA -< (clearId image, Nothing, clearId orig)

parseNac :: ArrowXml cat => cat (NTree XNode) (ParsedTypedGraph,[Mapping])
parseNac = atTag "NAC" >>>
  proc nac -> do
    graph <- parseGraph -< nac
    _  <- isA (\str -> str == "" || str == "true") <<< getAttrValue "enabled" -< nac
    mappings <- listA parseMappings -< nac
    returnA -< (graph, mappings)

parseRuleSequence :: ArrowXml cat => cat (NTree XNode) Sequence
parseRuleSequence = atTag "Sequence" >>>
   proc s -> do
     name <- getAttrValue "name" -< s
     subs <- listA parseSubSequence -< s
     flows <- listA parseObjectFlow -< s
     returnA -< (name, subs, flows)

parseSubSequence :: ArrowXml cat => cat (NTree XNode) SubSequence
parseSubSequence = atTag "Subsequence" >>>
  proc sub -> do
    iterations <- getAttrValue "iterations" -< sub
    rules <- listA parseSequenceItem -< sub
    let i = read iterations::Int
    returnA -< (i, rules)

parseSequenceItem :: ArrowXml cat => cat (NTree XNode) SequenceItem
parseSequenceItem = atTag "Item" >>>
  proc item -> do
    iterations <- getAttrValue "iterations" -< item
    ruleName <- getAttrValue "rule" -< item
    let i = read iterations::Int
    returnA -< (i, ruleName)

parseObjectFlow :: ArrowXml cat => cat (NTree XNode) ParsedObjectFlow
parseObjectFlow = atTag "ObjectFlow" >>>
  proc objectFlow -> do
    index <- getAttrValue "index" -< objectFlow
    input <- getAttrValue "input" -< objectFlow
    output <- getAttrValue "output" -< objectFlow
    maps <- listA parseMappings -< objectFlow
    returnA -< (index, input, output, maps)
