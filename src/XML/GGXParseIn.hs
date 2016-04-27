{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXParseIn
 ( parseGGName
 , parseNames
 , parseTypeGraph
 , parseNacNames
 , parseRule
 , parseRuleSequence
 ) where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.ParsedTypes
import           XML.XMLUtilities

-- | Parse the name of the Grammar
parseGGName :: ArrowXml cat => cat (NTree XNode) String
parseGGName = atTag "GraphTransformationSystem" >>>
  proc ggname -> do
    name <- getAttrValue "name" -< ggname
    returnA -< name

-- | Parse all type graphs
parseTypeGraph :: ArrowXml cat => cat (NTree XNode) TypeGraph
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
    nodeType <- getAttrValue "type" -< node
    returnA -< (clearId nodeId, clearId nodeType)

parseEdge :: ArrowXml cat => cat (NTree XNode) ParsedTypedEdge
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeId <- getAttrValue "ID" -< node
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    returnA -< (clearId edgeId, clearId edgeType, clearId edgeSource, clearId edgeTarget)

parseGraph :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseGraph = atTag "Graph" >>>
  proc graph -> do
    graphId <- getAttrValue "ID" -< graph
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (clearId graphId, nodes, edges)

-- | Parse all enabled rules
parseRule :: ArrowXml cat => cat (NTree XNode) RuleWithNacs
parseRule = atTag "Rule" >>>
  proc rule -> do
    ruleName <- getAttrValue "name" -< rule
    _  <- isA (\str -> str == "" || str == "true") <<< getAttrValue "enabled" -< rule
    lhs <- parseLHS -< rule
    rhs <- parseRHS -< rule
    morphism <- parseMorphism -< rule
    nacs <- listA parseNac -< rule
    returnA -< ((ruleName, lhs, rhs, morphism), nacs)

parseLHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseLHS = atTag "Graph" >>>
  proc graph -> do
    _ <- isA ("LHS" ==) <<< getAttrValue "kind"-< graph
    lhs <- parseGraph -< graph
    returnA -< lhs

parseRHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseRHS = atTag "Graph" >>>
  proc graph -> do
    _ <- isA ("RHS" ==) <<< getAttrValue "kind"-< graph
    rhs <- parseGraph -< graph
    returnA -< rhs

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
    returnA -< (clearId image, clearId orig)

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
     returnA -< (name, subs)

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

-- | Reads the id from the last to the head, stops when do not found a number
clearId :: String -> String
clearId [] = error "Error reading id"
clearId l = if isNum (last l) then (clearId (init l)) ++ [last l] else ""
  where
   isNum x = x `elem` ['0','1','2','3','4','5','6','7','8','9']
