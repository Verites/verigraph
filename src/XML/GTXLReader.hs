{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

--FIXME hERANÇA
module XML.GTXLReader where

import qualified Abstract.Morphism                as M
import           Abstract.Valid
import           CriticalPairs.CriticalPairsTeste
import           Data.Hashable
import           Data.Matrix
import           Data.String.Utils
import           Data.Tree.NTree.TypeDefs
import qualified Graph.Graph                      as G
import qualified Graph.GraphMorphism              as GM
import qualified Graph.GraphRule                  as GR
import qualified Graph.TypedGraphMorphism         as TGM
import           XML.ParsedTypes
import           System.Environment
import           Text.XML.HXT.Core
import           XML.XMLUtilities

isTypeGraph :: String -> Bool
isTypeGraph = ("TypeGraph" ==)

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
    nacs            <- listA parseNAC                           -< rule
    returnA -< (ruleId, ruleName, preservedGraph, (nodesDeleted, edgesDeleted), (nodesCreated, edgesCreated), nacs)

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
parseNAC = atTag "precondition" >>> atTag "condition" >>> atTag "graphCondition" >>>
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

instatiateTypedGraphsFromRule :: G.Graph a b -> ParsedRule ->
  ( GM.GraphMorphism a b, GM.GraphMorphism a b, GM.GraphMorphism a b, [GM.GraphMorphism a b])
instatiateTypedGraphsFromRule tg (ruleId,ruleName,interfaceGraph,deletedElems,createdElems,nacs) = (l,k,r,n)
  where
    k = instatiateTypedGraph interfaceGraph tg
    l = addElemsToGraph k deletedElems
    r = addElemsToGraph k createdElems
    n = instatiateNacMorphisms l nacs

instatiateNacMorphisms :: GM.GraphMorphism a b -> [ParsedNAC] -> [GM.GraphMorphism a b]
instatiateNacMorphisms lhs = map (createNacMorphism lhs)

createNacMorphism :: GM.GraphMorphism a b -> ParsedNAC -> GM.GraphMorphism a b
createNacMorphism = addElemsToGraph


instatiateRule :: G.Graph a b -> ParsedRule -> GR.GraphRule a b
instatiateRule tg rule = GR.graphRule mapL mapR mapNacs
  where
    (l,k,r,ns) = instatiateTypedGraphsFromRule tg rule
    mapL = instatiateTypedMorphism k l
    mapR = instatiateTypedMorphism k r
    mapNacs = map (instatiateTypedMorphism l) ns

instatiateTypedMorphism :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> TGM.TypedGraphMorphism a b
instatiateTypedMorphism a b = TGM.typedMorphism a b (mapId a b)

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

fileName = "teste.xml"

main2 fs = do
  rules <- readRule fs
  typeGraph <- readTypeGraph fs
  let tg = instatiateTypeGraph $ head typeGraph
  --print (fmap (instatiateTypedGraphs tg) rules)
  --print (fmap (instatiateRule tg) rules)
  let rulesVerigraph = fmap (instatiateRule tg) rules
  --print (fmap valid rulesVerigraph)
  print (head rulesVerigraph)
  --let cps = matrix (length rulesVerigraph) (length rulesVerigraph) (\(x,y) -> fst $ countCP2 (rulesVerigraph!!(x-1)) (rulesVerigraph!!(y-1)))
  --print cps
  --let cps2 = matrix (length rulesVerigraph) (length rulesVerigraph) (\(x,y) -> snd $ countCP2 (rulesVerigraph!!(x-1)) (rulesVerigraph!!(y-1)))
  --print cps2
  return ()
