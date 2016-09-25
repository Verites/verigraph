{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.NewParser (
 parseTypeGraph
) where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.ParsedTypes
import           XML.Utilities            (clearIdUnsafe)
import           XML.XMLUtilities

clearId :: String -> String
clearId = clearIdUnsafe

data VNode = VNode {
  nodeId :: String,
  nodeName :: String,
  nodeType :: String
} deriving (Show, Read)

data VEdge = VEdge {
  edgeId :: String,
  edgeName :: String,
  edgeType :: String,
  edgeSource :: String,
  edgeTarget :: String
} deriving (Show, Read)

data VGraph = VGraph {
  graphId :: String,
  graphName :: String,
  nodes :: [VNode],
  edges :: [VEdge]
} deriving (Show, Read)

data VMapping = VMapping {
  origin :: String,
  image :: String
} deriving (Show, Read)

data VMorphism = VMorphism {
  mname :: String,
  mappings :: [VMapping]
} deriving (Show, Read)

data VRule = VRule {
  rname :: String,
  lhs :: VGraph,
  rhs :: VGraph,
  morphism :: VMorphism
} deriving (Show, Read)

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) VGraph
parseTypeGraph = atTag "Types" >>>
  proc types -> do
    typeGraph <- parseGraph -< types
    returnA -< typeGraph

parseNode :: ArrowXml cat => cat (NTree XNode) VNode
parseNode = atTag "Node" >>>
  proc node -> do
    nodeId <- getAttrValue "ID" -< node
    nodeName <- getAttrValue "name" -< node -- can be the empty string
    nodeType <- getAttrValue "type" -< node
    returnA -< VNode (clearId nodeId) nodeName (clearId nodeType)

parseEdge :: ArrowXml cat => cat (NTree XNode) VEdge
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeId <- getAttrValue "ID" -< node
    edgeName <- getAttrValue "name" -< node
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    let setEdgeName = clearId edgeSource ++ ">-" ++ edgeName ++ "->" ++ clearId edgeTarget
    returnA -< VEdge (clearId edgeId) setEdgeName (clearId edgeType) (clearId edgeSource) (clearId edgeTarget)

parseGraph :: ArrowXml cat => cat (NTree XNode) VGraph
parseGraph = atTag "Graph" >>>
  proc graph -> do
    graphId <- getAttrValue "ID" -< graph
    graphName <- getAttrValue "name" -< graph
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< VGraph (clearId graphId) graphName nodes edges

parseMapping :: ArrowXml cat => cat (NTree XNode) VMapping
parseMapping = atTag "Mapping" >>>
  proc mapping -> do
    image <- getAttrValue "image" -< mapping
    orig <- getAttrValue "orig" -< mapping
    returnA -< VMapping (clearId orig) (clearId image)

parseMorphism :: ArrowXml cat => cat (NTree XNode) VMorphism
parseMorphism = atTag "Morphism" >>>
--parseMorphism = getChildren >>> isElem >>> hasName "Morphism" >>>
  proc morphism -> do
    name <- getAttrValue "name" -< morphism
    maps <- listA parseMapping -< morphism
    returnA -< VMorphism name maps

-- TODO: separate left from right side
parseRule :: ArrowXml cat => cat (NTree XNode) VRule
parseRule = atTag "Rule" >>>
  proc rule -> do
    ruleName <- getAttrValue "name" -< rule
    graphs <- listA parseGraph -< rule
    morphism <- parseMorphism -< rule
    returnA -< VRule ruleName (head graphs) (head graphs) morphism