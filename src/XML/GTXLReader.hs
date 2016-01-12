{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GTXLReader where

import           System.Environment
import           Text.XML.HXT.Core
import           XML.XMLUtilities


isTypeGraph :: String -> Bool
isTypeGraph = ("TypeGraph" ==)

getTypeGraph = atTag "graph" >>>
  proc graph -> do
    idg <- isA isTypeGraph <<< getAttrValue "id" -< graph
    node <- atTag "node" -< graph
    nodeid <- getAttrValue "id" -< node
    returnA -< (idg, nodeid)

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
