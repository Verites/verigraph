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

getInitialGraphs = atTag "initial" >>>
  proc i -> do
    graphs <- listA getGraphs -< i
    returnA -< graphs

getGraphs = atTag "graph" >>>
  proc graph -> do
    idg <- getAttrValue "id" -< graph
    nodes <- listA getTypedNodes -< graph
    edges <- listA getTypedEdges -< graph
    returnA -< (idg, nodes, edges)

getNodes = atTag "node" >>>
  proc node -> do
    idn <- getAttrValue "id" -< node
    returnA -< clearNodeId idn

getTypedNodes = atTag "node" >>>
  proc node -> do
    idn <- getAttrValue "id" -< node
    nt <- getType -< node
    returnA -< (idn, nt)

getEdges = atTag "edge" >>>
  proc edge -> do
    ide <- getAttrValue "id" -< edge
    source <- getAttrValue "from" -< edge
    target <- getAttrValue "to" -< edge
    returnA -< (clearEdgeId ide, clearNodeId source, clearNodeId target)

getTypedEdges = atTag "edge" >>>
  proc edge -> do
    ide <- getAttrValue "id" -< edge
    source <- getAttrValue "from" -< edge
    target <- getAttrValue "to" -< edge
    et <- getType -< edge
    returnA -< (clearEdgeId ide, et, clearNodeId source, clearNodeId target)

getType = atTag "type" >>>
  proc t -> do
    t <- getAttrValue "xlink:href" -< t
    returnA -< clearEdgeId $ clearNodeId t

clearNodeId :: String -> String
clearNodeId = replace "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:" ""

clearEdgeId :: String -> String
clearEdgeId = replace "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:" ""

readTypeGraph = runX (parseXML "instrutivo.xml" >>> getTypeGraph)

readGraphs = runX (parseXML "instrutivo.xml" >>> getInitialGraphs)

--main = do
--  tg <- runX (parseXML "instrutivo.xml" >>> getTypeGraph)
--  print tg
--  return ()
