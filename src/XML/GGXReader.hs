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

parseTypeGraph :: ArrowXml cat => cat (NTree XNode) TypeGraph
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

parseRule :: ArrowXml cat => cat (NTree XNode) RuleWithNacs
parseRule = atTag "Rule" >>>
  proc rule -> do
    ruleName <- getAttrValue "name" -< rule
    lhs <- parseLHS -< rule
    rhs <- parseRHS -< rule
    morphism <- parseMorphism -< rule
    nacs <- listA parseNac -< rule
    returnA -< ((ruleName, lhs, rhs, morphism), nacs)

parseLHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseLHS = atTag "Graph" >>>
  proc graph -> do
    _ <- isA ("LHS" ==) <<< getAttrValue "kind"-< graph
    lhs <- parseGraph -< graph
    returnA -< lhs

parseRHS :: ArrowXml cat => cat (NTree XNode) ParsedTypedGraph
parseRHS = atTag "Graph" >>>
  proc graph -> do
    _ <- isA ("RHS" ==) <<< getAttrValue "kind"-< graph
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

parseNac :: ArrowXml cat => cat (NTree XNode) (ParsedTypedGraph,[Mapping])
parseNac = atTag "NAC" >>>
  proc nac -> do
    graph <- parseGraph -< nac
    mappings <- listA parseMappings -< nac
    returnA -< (graph, mappings)

main :: IO()
main = do
  a <- runX (parseXML "teste-conflito.ggx" >>> parseTypeGraph)
  b <- runX (parseXML "teste-conflito.ggx" >>> parseRule)
  --let ((h1,h2,h3,h4),h5) = b!!0
  --print h2
  --print h3
  --print h4
  --print h5
  --let tg = instantiateTypeGraph $ head a
  --let l = instantiateTypedGraph h2 tg
  --let r = instantiateTypedGraph h3 tg
  --let nacs = map (instantiateNac l tg) h5
  ----print $ instantiateTgm lhs (nacs!!0) (snd $ head h5)
  --let k =  instantiateInterface h4 l
  --let rhs = instantiateTgm k r h4
  --let lhs = instantiateLeft k l
  --print $ GR.graphRule lhs rhs nacs
  let rules = map (instantiateRule $ head a) b
  print rules
  return ()

readTypeGraph :: String -> IO[TypeGraph]
readTypeGraph fileName = runX (parseXML fileName >>> parseTypeGraph)

readRules :: String -> IO[RuleWithNacs]
readRules fileName = runX (parseXML fileName >>> parseRule)


instantiateRule :: TypeGraph -> RuleWithNacs -> GR.GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = GR.graphRule lhsTgm rhsTgm nacsTgm
  where
    tg = instantiateTypeGraph typeGraph
    lm = instantiateTypedGraph lhs tg
    rm = instantiateTypedGraph rhs tg
    km = instantiateInterface mappings lm
    nacsTgm = map (instantiateNac lm tg) nacs
    rhsTgm = instantiateTgm km rm mappings
    lhsTgm = instantiateLeft km lm
    -- doIt = GR.graphRule lhsTgm rhsTgm nacsTgm



instantiateNac ::  GM.GraphMorphism a b -> G.Graph a b -> Nac -> TGM.TypedGraphMorphism a b
instantiateNac lhs graph (nacG, maps) = nacTgm
  where
    nacMorphism = instantiateTypedGraph nacG graph
    nacTgm = instantiateTgm lhs nacMorphism maps


instantiateTypeGraph :: TypeGraph -> G.Graph a b
instantiateTypeGraph (nodes, edges) = graphWithEdges
  where
    getNodeId y = G.NodeId (read y :: Int)
    getEdgeId y = G.EdgeId (read y :: Int)
    lkup e l = fromJust $ lookup e l
    getNodeType n = G.NodeId (read (lkup n nodes) :: Int)
    nodesId = map (\(_,y) -> getNodeId y) nodes
    graphWithNodes = foldr G.insertNode G.empty nodesId
    edgesId = map (\(_, typ, src, tgt) -> (getEdgeId typ, getNodeType src, getNodeType tgt)) edges
    graphWithEdges = foldr (\(ide,src,tgt) g -> G.insertEdge ide src tgt g) graphWithNodes edgesId

--instantiateRule :: Rule -> GR.GraphRule a b

-- instantiateGraph :: G.Graph a b -> ParsedTypedGraph -> GM.GraphMorphism a b
-- instantiateGraph typeGraph parsedGraph =
--   where
--     init = G.empty


instantiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GM.GraphMorphism a b
instantiateTypedGraph (_,b,c) tg = GM.gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (toN . fst) b
    edgesG = map (\(x,_,y,z) -> (toN x, toN y, toN z)) c
    nodeTyping = map (\(x,y) -> (toN x, toN y)) b
    edgeTyping = map (\(x,y,_,_) -> (toN x, toN y)) c

instantiateInterface :: [Mapping] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
instantiateInterface mapping l = GM.gmbuild graphK (M.codomain l) nodeType edgeType
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

instantiateLeft :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> TGM.TypedGraphMorphism a b
instantiateLeft k l = TGM.typedMorphism k l edges
  where
    ini = GM.empty (M.domain k) (M.domain l)
    nodes = foldr (\n gm -> GM.updateNodes n n gm) ini (G.nodes (M.domain k))
    edges = foldr (\e gm -> GM.updateEdges e e gm) nodes (G.edges (M.domain k))

instantiateTgm :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> [Mapping] -> TGM.TypedGraphMorphism a b
instantiateTgm s t maps = TGM.typedMorphism s t gmMap
  where
    graphS = M.domain s
    initGm = GM.empty graphS (M.domain t)
    listNodes = map (\(G.NodeId x) -> x) (G.nodes graphS)
    listInt = map (\(x,y) -> (toN x, toN y)) maps
    gmMap = foldr (\(imag,orig) gm ->
      if orig `elem` listNodes
         then GM.updateNodes (G.NodeId orig) (G.NodeId imag) gm
         else GM.updateEdges (G.EdgeId orig) (G.EdgeId imag) gm)
      initGm listInt

clearId :: String -> String
clearId = tail

toN :: String -> Int
toN x = read x :: Int
