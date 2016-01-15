{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

--FIXME hERANÇA
module XML.GTXLReader where

import           Abstract.Valid
import           CriticalPairs.CriticalPairsTeste
import           Data.Hashable
import           Data.Matrix
import           Data.String.Utils
import qualified Graph.Graph as G
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Abstract.Morphism as M
import qualified Graph.GraphRule as GR
import           System.Environment
import           Text.XML.HXT.Core
import           XML.XMLUtilities
import           Data.Tree.NTree.TypeDefs

isTypeGraph :: String -> Bool
isTypeGraph = ("TypeGraph" ==)

type ParsedNode = String -- NodeId
type ParsedTypedNode = (String, String) -- (NodeId, NodeType)
type ParsedEdge = (String, String, String)
type ParsedTypedEdge = (String, String, String, String)
type ParsedGraph = (String, [ParsedNode], [ParsedEdge])
type ParsedTypedGraph = (String, [ParsedTypedNode], [ParsedTypedEdge])
type ParsedRule = (String, String, ParsedTypedGraph,
                   ([ParsedTypedNode], [ParsedTypedEdge]),
                   ([ParsedTypedNode], [ParsedTypedEdge]))
type ParsedNAC = ([ParsedTypedNode], [ParsedTypedEdge])

parseTypeGraphs :: ArrowXml cat => cat (NTree XNode) ParsedGraph
parseTypeGraphs = atTag "graph" >>>
  proc graph -> do
    idg <- isA isTypeGraph <<< getAttrValue "id" -< graph
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (idg, nodes, edges)

parseInitialGraphs :: ArrowXml cat => cat (NTree XNode) [ParsedTypedGraph]
parseInitialGraphs = atTag "initial" >>>
  proc i -> do
    graphs <- listA parseGraph -< i
    returnA -< graphs

parseGraph :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseGraph = atTag "graph" >>>
  proc graph -> do
    idg <- getAttrValue "id" -< graph
    nodes <- listA parseTypedNode -< graph
    edges <- listA parseTypedEdge -< graph
    returnA -< (idg, nodes, edges)

parseRule :: ArrowXml cat => cat (NTree XNode) ParsedRule
parseRule = atTag "rule" >>>
  proc rule -> do
    ruleId          <- getAttrValue "id"                        -< rule
    ruleName        <- getAttrValue "name"                      -< rule
    preservedGraph  <- parseGraph <<< atTag "preserved"         -< rule
    nodesDeleted    <- listA parseTypedNode <<< atTag "deleted" -< rule
    edgesDeleted    <- listA parseTypedEdge <<< atTag "deleted" -< rule
    nodesCreated    <- listA parseTypedNode <<< atTag "created" -< rule
    edgesCreated    <- listA parseTypedEdge <<< atTag "created" -< rule
    nacs            <- listA parseNAC <<< atTag "precondition"  -< rule
    returnA -< (ruleId, ruleName, preservedGraph, (nodesDeleted, edgesDeleted), (nodesCreated, edgesCreated))

parseNode :: ArrowXml cat => cat (NTree XNode) ParsedNode
parseNode = atTag "node" >>>
  proc node -> do
    idn <- getAttrValue "id" -< node
    returnA -< clearNodeId idn

parseTypedNode :: ArrowXml cat => cat (NTree XNode) ParsedTypedNode
parseTypedNode = atTag "node" >>>
  proc node -> do
    idn <- getAttrValue "id" -< node
    nt <- parseType -< node
    returnA -< (idn, nt)

parseEdge :: ArrowXml cat => cat (NTree XNode) ParsedEdge
parseEdge = atTag "edge" >>>
  proc edge -> do
    ide <- getAttrValue "id" -< edge
    source <- getAttrValue "from" -< edge
    target <- getAttrValue "to" -< edge
    returnA -< (clearEdgeId ide, clearNodeId source, clearNodeId target)

parseTypedEdge :: ArrowXml cat => cat (NTree XNode) ParsedTypedEdge
parseTypedEdge = atTag "edge" >>>
  proc edge -> do
    ide <- getAttrValue "id" -< edge
    source <- getAttrValue "from" -< edge
    target <- getAttrValue "to" -< edge
    et <- parseType -< edge
    returnA -< (clearEdgeId ide, et, clearNodeId source, clearNodeId target)

parseType :: ArrowXml cat => cat (NTree XNode) String
parseType = atTag "type" >>>
  proc t -> do
    t <- getAttrValue "xlink:href" -< t
    returnA -< clearEdgeId $ clearNodeId t

parseNAC :: ArrowXml cat => cat(NTree XNode) ParsedNAC
parseNAC = atTag "condition" >>> atTag "graphCondition" >>>
  proc gc -> do
  name  <- isA isNac <<< getAttrValue "name"  -< gc
  nodes <- listA parseTypedNode               -< gc
  edges <- listA parseTypedEdge               -< gc
  returnA -< (nodes, edges)

isNac :: String -> Bool
isNac = ("Nac" == )

clearNodeId :: String -> String
clearNodeId = replace "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:" "" . replace "#" ""

clearEdgeId :: String -> String
clearEdgeId = last . split ":" . replace "%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:" "" . replace "#" ""

readTypeGraph :: String -> IO [ParsedGraph]
readTypeGraph fileName = runX (parseXML fileName >>> parseTypeGraphs)

readGraphs :: String -> IO [[ParsedTypedGraph]]
readGraphs fileName = runX (parseXML fileName >>> parseInitialGraphs)

readRule :: String -> IO [ParsedRule]
readRule fileName = runX (parseXML fileName >>> parseRule)

readNac :: String -> IO [ParsedNAC]
readNac fileName = runX (parseXML fileName >>> parseNAC)

instatiateTypeGraph :: ParsedGraph -> G.Graph a b
instatiateTypeGraph (a,b,c) = G.build nodes edges
  where
    nodes = map hash b
    edges = map (\(x,y,z) -> (hash x, hash y, hash z)) c


instatiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GM.GraphMorphism a b
instatiateTypedGraph (a,b,c) tg = GM.gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (hash . fst) b
    edgesG = map (\(x,_,y,z) -> (hash x, hash y, hash z)) c
    nodeTyping = map (\(x,y) -> (hash x, hash y)) b
    edgeTyping = map (\(x,y,_,_) -> (hash x, hash y)) c

addElemsToGraph :: GM.GraphMorphism a b
               -> ([ParsedTypedNode],[ParsedTypedEdge])
               -> GM.GraphMorphism a b
addElemsToGraph graph (nodes,edges) = graphWithNewEdges
  where
    graphWithNewNodes = foldl insertNode graph nodes
    graphWithNewEdges = foldl insertEdge graphWithNewNodes edges

insertEdge :: GM.GraphMorphism a b -> ParsedTypedEdge -> GM.GraphMorphism a b
insertEdge graph (edgeId,edgeType,src,tgt) =
    GM.updateEdges (G.EdgeId hashId) (G.EdgeId (hash edgeType)) graphWithEdgeTyped
  where
    hashId = hash edgeId
    graphWithEdge = G.insertEdge (G.EdgeId hashId) (G.NodeId (hash src)) (G.NodeId (hash tgt)) (M.domain graph)
    graphWithEdgeTyped = GM.updateDomain graphWithEdge graph

insertNode :: GM.GraphMorphism a b -> ParsedTypedNode -> GM.GraphMorphism a b
insertNode graph (nodeId,nodeType) =
    GM.updateNodes (G.NodeId hashId) (G.NodeId (hash nodeType)) graphWithNodeTyped
  where
    hashId = hash nodeId
    graphWithNode = G.insertNode (G.NodeId hashId) (M.domain graph)
    graphWithNodeTyped = GM.updateDomain graphWithNode graph

instatiateTypedGraphs :: G.Graph a b -> ParsedRule ->
  ( GM.GraphMorphism a b, GM.GraphMorphism a b, GM.GraphMorphism a b)
instatiateTypedGraphs tg (ruleId,ruleName,interfaceGraph,deletedElems,createdElems) = (l,k,r)
  where
    k = instatiateTypedGraph interfaceGraph tg
    l = addElemsToGraph k deletedElems
    r = addElemsToGraph k createdElems

instatiateRule :: G.Graph a b -> ParsedRule -> GR.GraphRule a b
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
i3 = readRule fileName
i4 = readNac fileName


main = do
  typeGraph <- readTypeGraph fileName
  a <- return $ fmap instatiateTypeGraph typeGraph
  print (fmap valid a)
  return a

fileName = "teste-conflito.xml"

main2 = do
  rules <- readRule fileName
  typeGraph <- readTypeGraph fileName
  let tg = instatiateTypeGraph $ head typeGraph
  --print (fmap (instatiateTypedGraphs tg) rules)
  print (fmap (instatiateRule tg) rules)
  let rulesVerigraph = fmap (instatiateRule tg) rules
  print (fmap valid rulesVerigraph)
  let cps = matrix (length rulesVerigraph) (length rulesVerigraph) (\y -> fst $ countCP2 (rulesVerigraph!!((fst y)-1)) (rulesVerigraph!!((snd y)-1)))
  print cps
  return ()
