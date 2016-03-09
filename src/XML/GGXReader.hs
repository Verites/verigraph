{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXReader where

import           Data.Maybe
import           Data.Tree.NTree.TypeDefs
import qualified Graph.Graph as G
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.GraphRule as GR
import qualified Abstract.Morphism as M
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
    returnA -< (clearId nodeId, clearId nodeType)

parseEdge :: ArrowXml cat => cat (NTree XNode) ParsedTypedEdge
parseEdge = atTag "Edge" >>>
  proc node -> do
    edgeId <- getAttrValue "ID" -< node
    edgeType <- getAttrValue "type" -< node
    edgeSource <- getAttrValue "source" -< node
    edgeTarget <- getAttrValue "target" -< node
    returnA -< (clearId edgeId, clearId edgeType, clearId edgeSource, clearId edgeTarget)

parseGraph :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseGraph = atTag "Graph" >>>
  proc graph -> do
    graphId <- getAttrValue "ID" -< graph
    nodes <- listA parseNode -< graph
    edges <- listA parseEdge -< graph
    returnA -< (clearId graphId, nodes, edges)

parseRule :: ArrowXml cat => cat (NTree XNode) Rule
parseRule = atTag "Rule" >>>
  proc rule -> do
    ruleName <- getAttrValue "name" -< rule
    lhs <- parseLHS -< rule
    rhs <- parseRHS -< rule
    morphism <- parseMorphism -< rule
    returnA -< (ruleName, lhs, rhs, morphism)

parseLHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseLHS = atTag "Graph" >>>
  proc graph -> do
    kind <- isA ("LHS" ==) <<< getAttrValue "kind"-< graph
    lhs <- parseGraph -< graph
    returnA -< lhs

parseRHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseRHS = atTag "Graph" >>>
  proc graph -> do
    kind <- isA ("RHS" ==) <<< getAttrValue "kind"-< graph
    rhs <- parseGraph -< graph
    returnA -< rhs

parseMorphism :: ArrowXml cat => cat (NTree XNode) [Mapping]
parseMorphism = atTag "Morphism">>>
  proc morphism -> do
    maps <- listA parseMappings -< morphism
    returnA -< maps

parseMappings :: ArrowXml cat => cat (NTree XNode) Mapping
parseMappings = atTag "Mapping" >>>
  proc mapping -> do
    image <- getAttrValue "image" -< mapping
    orig <- getAttrValue "orig" -< mapping
    returnA -< (clearId image, clearId orig)

main :: IO()
main = do
  a <- runX (parseXML "teste-conflito.ggx" >>> parseTypeGraph)
  b <- runX (parseXML "teste-conflito.ggx" >>> parseRule)
  let (h1,h2,h3,h4) = b!!1
  print h2
  print h3
  print h4
  let (n,e) = head a
  let tg = instatiateTypeGraph n e
  let lhs = instatiateTypedGraph h2 tg
  print lhs
  let k =  instatiateInterface h4 lhs
  print k
  return ()

instatiateTypeGraph :: [ParsedTypedNode] -> [ParsedTypedEdge] -> G.Graph a b
instatiateTypeGraph nodes edges = graphWithEdges
  where
    getNodeId y = G.NodeId (read y :: Int)
    getEdgeId y = G.EdgeId (read y :: Int)
    lkup e l = fromJust $ lookup e l
    getNodeType n = G.NodeId (read (lkup n nodes) :: Int)
    nodesId = map (\(_,y) -> getNodeId y) nodes
    graphWithNodes = foldr G.insertNode G.empty nodesId
    edgesId = map (\(_, typ, src, tgt) -> (getEdgeId typ, getNodeType src, getNodeType tgt)) edges
    graphWithEdges = foldr (\(ide,src,tgt) g -> G.insertEdge ide src tgt g) graphWithNodes edgesId

--instatiateRule :: Rule -> GR.GraphRule a b

-- instatiateGraph :: G.Graph a b -> ParsedTypedGraph -> GM.GraphMorphism a b
-- instatiateGraph typeGraph parsedGraph =
--   where
--     init = G.empty


instatiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GM.GraphMorphism a b
instatiateTypedGraph (a,b,c) tg = GM.gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (toN . fst) b
    edgesG = map (\(x,_,y,z) -> (toN x, toN y, toN z)) c
    nodeTyping = map (\(x,y) -> (toN x, toN y)) b
    edgeTyping = map (\(x,y,_,_) -> (toN x, toN y)) c

instatiateInterface :: [Mapping] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
instatiateInterface mapping l = GM.gmbuild graphK (M.codomain l) nodeType edgeType
  where
    listMap = map (toN . snd) mapping
    nodesK = filter (\(G.NodeId x) -> x `elem` listMap) (G.nodes (M.domain l))
    nodesK2 = map (\(G.NodeId x) -> x) nodesK
    edgesK = filter (\(G.EdgeId x) -> x `elem` listMap) (G.edges (M.domain l))
    src e = fromJust $ G.sourceOf (M.domain l) e
    src2 (G.NodeId e) = e
    tgt e = fromJust $ G.targetOf (M.domain l) e
    tgt2 (G.NodeId e) = e
    edgesK2 = map (\edg@(G.EdgeId x) -> (x, src2 (src edg), tgt2 (tgt edg))) edgesK

    graphK = G.build nodesK2 edgesK2
    applyN t n = fromJust $ GM.applyNode t n
    applyE t e = fromJust $ GM.applyEdge t e
    nodeTyping = map (\n -> (n, applyN l n)) nodesK
    edgeTyping = map (\e -> (e, applyE l e)) edgesK
    nodeType = map (\(G.NodeId x, G.NodeId y) -> (x,y)) nodeTyping
    edgeType = map (\(G.EdgeId x, G.EdgeId y) -> (x,y)) edgeTyping

clearId :: String -> String
clearId = tail

toN :: String -> Int
toN x = read x :: Int
