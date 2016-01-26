{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXReader where

import qualified Data.Map.Strict as Map
import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core
import           XML.XMLUtilities

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) ([String],[(String, String, String)])
parseTypeGraph = atTag "Types" >>> atTag "Graph" >>>
  proc graph -> do
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (nodes, edges)

parseNode :: ArrowXml cat => cat (NTree XNode) String
parseNode = atTag "Node" >>>
  proc node -> do
    nodeType <- getAttrValue "type" -< node
    returnA -< nodeType

parseEdge :: ArrowXml cat => cat (NTree XNode) (String, String, String)
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    returnA -< (edgeType, edgeSource, edgeTarget)

parseNodeType :: ArrowXml cat => cat (NTree XNode) (String, String)
parseNodeType = atTag "Node" >>>
  proc nodeType -> do
    nodeId    <- getAttrValue "ID" -< nodeType
    nodeType  <- getAttrValue "type" -< nodeType
    returnA -< (nodeId, nodeType)

parseNodeTypes :: ArrowXml cat => cat (NTree XNode) [(String, String)]
parseNodeTypes = atTag "Types" >>> atTag "Graph" >>>
  proc graph -> do
    nodes <- listA parseNodeType -< graph
    returnA -< nodes

--readTypes :: String -> IO [Map.Map String String]
--readTypes fileName = createMap $ runX (parseXML fileName >>> parseNodeTypes)

main = do
  a <- runX (parseXML "teste-conflito.ggx" >>> parseNodeType)
  print a
  return ()

--instatiateTypeGraph :: ParsedGraph -> G.Graph a b
