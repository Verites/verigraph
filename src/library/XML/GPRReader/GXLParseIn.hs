{-# LANGUAGE Arrows #-}

module XML.GPRReader.GXLParseIn
  ( parseGPR
  , Label
  , Node
  , Id
  , ParsedNode
  , ParsedEdge
  , RuleGraph
  , ParsedRuleGraph
  ) where

import           Data.Tree.NTree.TypeDefs
import           System.FilePath          (dropExtension, takeFileName)
import           Text.XML.HXT.Core

import           XML.XMLUtilities

type Label = String
type RuleName = String
type Node = String
type Id = Int
type Edge = (Node,Node,Label)
type ParsedNode = (Node, Id)
type ParsedEdge = (Edge, Id)
type RuleGraph = ([ParsedNode], [ParsedEdge])
type ParsedRuleGraph = (RuleName,RuleGraph)

-- | Parses a Groove Production Rule (.gpr)
parseGPR :: String -> IO ParsedRuleGraph
parseGPR pathName = do
  [rule] <- parseRule pathName
  let ruleName = dropExtension (takeFileName pathName)
  return (ruleName,rule)

parseRule :: String -> IO [RuleGraph]
parseRule fileName = runX (parseXML fileName >>> parseGPRRule)

parseGPRRule :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) RuleGraph
parseGPRRule = atTag "gxl" >>> atTag "graph" >>>
  proc graph -> do
    nodes <- listA parseGPRNode -< graph
    edges <- listA parseGPREdge -< graph
    let nodesId = zip nodes ([0..]::[Int])
    let edgesId = zip edges ([0..]::[Int])
    returnA -< (nodesId,edgesId)

parseGPRNode :: IOSLA (XIOState ()) (NTree XNode) Node
parseGPRNode = atTag "node" >>>
  proc nodes -> do
    x <- getAttrValue "id" -< nodes
    returnA -< x

parseGPREdge :: IOSLA (XIOState ()) (NTree XNode) Edge
parseGPREdge = atTag "edge" >>>
  proc edges -> do
    from <- getAttrValue "from" -< edges
    to <- getAttrValue "to" -< edges
    label <- parseGPREdgeLabel -< edges
    returnA -< (from,to,label)

parseGPREdgeLabel :: IOSLA (XIOState ()) (NTree XNode) Label
parseGPREdgeLabel = atTag "attr" >>>
  proc attrs -> do
    _  <- isA (/= "layout") <<< getAttrValue "name" -< attrs
    string <- (getChildren >>> hasName "string" /> getText) -< attrs
    returnA -< string
