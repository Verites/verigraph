{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXReader where

import           Data.Maybe
import           Data.Tree.NTree.TypeDefs
import qualified Graph.Graph as G
import           Text.XML.HXT.Core
import           XML.ParsedTypes
import           XML.XMLUtilities

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) ([ParsedTypedNode],[ParsedTypedEdge])
parseTypeGraph = atTag "Types" >>> atTag "Graph" >>>
  proc graph -> do
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (nodes, edges)

parseNode :: ArrowXml cat => cat (NTree XNode) ParsedTypedNode
parseNode = atTag "Node" >>>
  proc node -> do
    nodeId <- getAttrValue "ID" -< node
    nodeType <- getAttrValue "type" -< node
    returnA -< (nodeId,nodeType)

parseEdge :: ArrowXml cat => cat (NTree XNode) ParsedTypedEdge
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeId <- getAttrValue "ID" -< node
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    returnA -< (edgeId, edgeType, edgeSource, edgeTarget)

--readTypes :: String -> IO [Map.Map String String]
--readTypes fileName = createMap $ runX (parseXML fileName >>> parseNodeTypes)

main :: IO()
main = do
  a <- runX (parseXML "teste-conflito.ggx" >>> parseTypeGraph)
  let (n,e) = head a
  let tg = instatiateTypeGraph n e
  print tg
  --print (G.sourceOf tg (G.EdgeId 6))
  return ()

instatiateTypeGraph :: [ParsedTypedNode] -> [ParsedTypedEdge] -> G.Graph a b
instatiateTypeGraph nodes edges = graphWithEdges
  where
    getNodeId y = G.NodeId (read (tail y) :: Int)
    getEdgeId y = G.EdgeId (read (tail y) :: Int)
    lkup e l = fromJust $ lookup e l
    getNodeType n = G.NodeId (read (tail (lkup n nodes)) :: Int)
    nodesId = map (\(x,y) -> getNodeId y) nodes
    graphWithNodes = foldr G.insertNode G.empty nodesId
    edgesId = map (\(id, typ, src, tgt) -> (getEdgeId typ, getNodeType src, getNodeType tgt)) edges
    graphWithEdges = foldr (\(id,src,tgt) g -> G.insertEdge id src tgt g) graphWithNodes edgesId

