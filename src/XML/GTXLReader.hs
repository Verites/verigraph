{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

--FIXME hERANÇA
module XML.GTXLReader where

import           Abstract.Valid
import           Data.Hashable
import           Data.String.Utils
import qualified Graph.Graph as G
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Abstract.Morphism as M
import qualified Graph.GraphRule as GR
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

instatiateTypeGraph :: (String, [String], [(String, String, String)]) -> G.Graph a b
instatiateTypeGraph (a,b,c) = G.build nodes edges
  where
    nodes = map hash b
    edges = map (\(x,y,z) -> (hash x, hash y, hash z)) c


instatiateTypedGraph (a,b,c) tg = GM.gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (hash . fst) b
    edgesG = map (\(x,_,y,z) -> (hash x, hash y, hash z)) c
    nodeTyping = map (\(x,y) -> (hash x, hash y)) b
    edgeTyping = map (\(x,y,_,_) -> (hash x, hash y)) c

addElemsToGraph :: GM.GraphMorphism a b
               {-}-> [(String,String)]
               -> [(String,String,String,String)]-}
               -> ([(String, String)],[(String, String, String, String)])
               -> GM.GraphMorphism a b
addElemsToGraph graph (nodes,edges) = graphWithNewEdges
  where
    graphWithNewNodes = foldl insertNode graph nodes
    graphWithNewEdges = foldl insertEdge graphWithNewNodes edges

insertEdge :: GM.GraphMorphism a b -> (String,String,String,String) -> GM.GraphMorphism a b
insertEdge graph (edgeId,edgeType,src,tgt) =
    GM.updateEdges (G.EdgeId hashId) (G.EdgeId (hash edgeType)) graphWithEdgeTyped
  where
    hashId = hash edgeId
    graphWithEdge = G.insertEdge (G.EdgeId hashId) (G.NodeId (hash src)) (G.NodeId (hash tgt)) (M.domain graph)
    graphWithEdgeTyped = GM.updateDomain graphWithEdge graph

insertNode :: GM.GraphMorphism a b -> (String,String) -> GM.GraphMorphism a b
insertNode graph (nodeId,nodeType) =
    GM.updateNodes (G.NodeId hashId) (G.NodeId (hash nodeType)) graphWithNodeTyped
  where
    hashId = hash nodeId
    graphWithNode = G.insertNode (G.NodeId hashId) (M.domain graph)
    graphWithNodeTyped = GM.updateDomain graphWithNode graph

instatiateTypedGraphs :: G.Graph a b
                      -> (String,
                          String,
                          (String,
                           [(String, String)],
                           [(String, String, String, String)]),
                          ([(String, String)],
                           [(String, String, String, String)]),
                          ([(String, String)],
                           [(String, String, String, String)]))
                      -> (GM.GraphMorphism a b,
                          GM.GraphMorphism a b,
                          GM.GraphMorphism a b)
instatiateTypedGraphs tg (ruleId,ruleName,interfaceGraph,deletedElems,createdElems) = (l,k,r)
  where
    k = instatiateTypedGraph interfaceGraph tg
    l = addElemsToGraph k deletedElems
    r = addElemsToGraph k createdElems

instatiateRule tg rule = GR.graphRule mapL mapR []
  where
    (l,k,r) = instatiateTypedGraphs tg rule
    mapL = TGM.typedMorphism k l (mapId k l)
    mapR = TGM.typedMorphism k r (mapId k r)

mapId :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> GM.GraphMorphism a b
mapId g r = edgesUpdated
  where
    init = GM.empty (M.domain g) (M.domain r)
    zipNodes = zip (G.nodes $ M.domain g) (G.nodes $ M.domain g)
    zipEdges = zip (G.edges $ M.domain g) (G.edges $ M.domain g)
    nodesUpdated = foldr (\(a,b) -> GM.updateNodes a b) init zipNodes
    edgesUpdated = foldr (\(a,b) -> GM.updateEdges a b) nodesUpdated zipEdges

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
  --print (fmap (instatiateTypedGraphs tg) rules)
  print (fmap (instatiateRule tg) rules)
  let rulesVerigraph = fmap (instatiateRule tg) rules
  print (fmap valid rulesVerigraph)
  --print
  return ()
