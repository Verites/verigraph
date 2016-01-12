{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GTXLReader where

import           Data.String.Utils
import           System.Environment
import           Text.XML.HXT.Core
import           XML.XMLUtilities


isTypeGraph :: String -> Bool
isTypeGraph = ("TypeGraph" ==)

getTypeGraph = atTag "graph" >>>
  proc graph -> do
    idg <- isA isTypeGraph <<< getAttrValue "id" -< graph
    nodes <- listA getNodes -< graph
    edges <- listA getEdges -< graph
    returnA -< (idg, nodes, edges)

getNodes = atTag "node" >>>
  proc node -> do
    idn <- getAttrValue "id" -< node
    returnA -< clearNodeId idn

getEdges = atTag "edge" >>>
  proc edge -> do
    ide <- getAttrValue "id" -< edge
    source <- getAttrValue "from" -< edge
    target <- getAttrValue "to" -< edge
    returnA -< (clearEdgeId ide, clearNodeId source, clearNodeId target)

clearNodeId :: String -> String
clearNodeId = replace "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:" ""

clearEdgeId :: String -> String
clearEdgeId = replace "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:" ""

getGraphs = atTag "initial" >>>
  proc i -> do
    graph <- atTag "graph" -< i
    idg <- getAttrValue "id" -< graph
    node <- atTag "node" -< graph
    nodeId <- getAttrValue "id" -< node
    returnA -< (idg, nodeId)



readTypeGraph = runX (parseXML "instrutivo.xml" >>> getTypeGraph)

readGraphs = runX (parseXML "instrutivo.xml" >>> getGraphs)

--main = do
--  tg <- runX (parseXML "instrutivo.xml" >>> getTypeGraph)
--  print tg
--  return ()
