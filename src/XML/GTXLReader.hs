{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

--FIXME hERANÇA
module XML.GTXLReader where

import           Abstract.Valid
import           Data.Hashable
import           Data.String.Utils
import           Graph.Graph
import qualified Graph.GraphMorphism as GM
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

getRules = atTag "rule" >>>
  proc rule -> do
    ruleId <- getAttrValue "id" -< rule
    ruleName <- getAttrValue "name" -< rule
    preservedGraph <- getGraphs <<< atTag "preserved" -< rule
    nodesDeleted <- listA getTypedNodes <<< atTag "deleted" -< rule
    edgesDeleted <- listA getTypedEdges <<< atTag "deleted" -< rule
    nodesCreated <- listA getTypedNodes <<< atTag "created" -< rule
    edgesCreated <- listA getTypedEdges <<< atTag "created" -< rule
    returnA -< (ruleId, ruleName, preservedGraph, (nodesDeleted, edgesDeleted), (nodesCreated, edgesCreated))

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
clearNodeId = replace "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:" "" . replace "#" ""

clearEdgeId :: String -> String
clearEdgeId = last . split ":" . replace "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:" "" . replace "#" ""

readTypeGraph :: String -> IO [(String, [String], [(String, String, String)])]
readTypeGraph fileName = runX (parseXML fileName >>> getTypeGraph)

readGraphs :: String -> IO [[(String, [(String, String)],
  [(String, String, String, String)])]]
readGraphs fileName = runX (parseXML fileName >>> getInitialGraphs)

readRules fileName = runX (parseXML fileName >>> getRules)

instatiateTypeGraph :: (String, [String], [(String, String, String)]) -> Graph a b
instatiateTypeGraph (a,b,c) = build nodes edges
  where
    nodes = map hash b
    edges = map (\(x,y,z) -> (hash x, hash y, hash z)) c


instatiateTypedGraph (a,b,c) tg = GM.gmbuild k tg nodeTyping edgeTyping
  where
    k = build nodesK edgesK
    nodesK = map (hash . fst) b
    edgesK = map (\(x,_,y,z) -> (hash x, hash y, hash z)) c
    nodeTyping = map (\(x,y) -> (hash x, hash y)) b
    edgeTyping = map (\(x,y,_,_) -> (hash x, hash y)) c

--instatiateTypedGraphs :: x -> Graph a b
instatiateTypedGraphs tg (ruleId,ruleName,interfaceGraph,deletedElems,createdElems) = x
  where
    x = instatiateTypedGraph interfaceGraph tg

i1 = readTypeGraph fileName
i2 = readGraphs fileName
i3 = readRules fileName


main = do
  typeGraph <- readTypeGraph fileName
  a <- return $ fmap instatiateTypeGraph typeGraph
  print (fmap valid a)
  return a

fileName = "teste-conflito.xml"
main2 = do
  rules <- readRules fileName
  typeGraph <- readTypeGraph fileName
  let tg = instatiateTypeGraph $ head typeGraph
  print (fmap (instatiateTypedGraphs tg) rules)
  print (fmap valid  (fmap (instatiateTypedGraphs tg) rules))
  return ()
