{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.GGXReader
 ( readName,
   readNames,
   readNacNames,
   readTypeGraph,
   readRules,
   instantiateRule
   ) where

import           Abstract.Morphism
import           Data.Maybe (fromJust)
import           Data.Tree.NTree.TypeDefs
import qualified Graph.Graph as G
import           Graph.GraphMorphism
import           Graph.TypedGraphMorphism
import           Graph.GraphRule
import           Text.XML.HXT.Core
import           XML.GGXParseIn
import           XML.ParsedTypes
import           XML.XMLUtilities

readName :: String -> IO [String]
readName fileName = runX (parseXML fileName >>> parseGGName)

readTypeGraph :: String -> IO[TypeGraph]
readTypeGraph fileName = runX (parseXML fileName >>> parseTypeGraph)

readNacNames :: String -> IO [[(String,String)]]
readNacNames fileName = runX (parseXML fileName >>> parseNacNames)

readNames :: String -> IO [[(String,String)]]
readNames fileName = runX (parseXML fileName >>> parseNames)

readRules :: String -> IO[RuleWithNacs]
readRules fileName = runX (parseXML fileName >>> parseRule)

instantiateRule :: TypeGraph -> RuleWithNacs -> GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = graphRule lhsTgm rhsTgm nacsTgm
  where
    tg = instantiateTypeGraph typeGraph
    lm = instantiateTypedGraph lhs tg
    rm = instantiateTypedGraph rhs tg
    km = instantiateInterface mappings lm
    nacsTgm = map (instantiateNac lm tg) nacs
    rhsTgm = instantiateTgm km rm mappings
    lhsTgm = instantiateLeft km lm

instantiateNac ::  GraphMorphism a b -> G.Graph a b -> Nac -> TypedGraphMorphism a b
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

instantiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GraphMorphism a b
instantiateTypedGraph (_,b,c) tg = gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (toN . fst) b
    edgesG = map (\(x,_,y,z) -> (toN x, toN y, toN z)) c
    nodeTyping = map (\(x,y) -> (toN x, toN y)) b
    edgeTyping = map (\(x,y,_,_) -> (toN x, toN y)) c

instantiateInterface :: [Mapping] -> GraphMorphism a b -> GraphMorphism a b
instantiateInterface mapping l = gmbuild graphK (codomain l) nodeType edgeType
  where
    listMap = map (toN . snd) mapping
    nodesK = filter (\(G.NodeId x) -> x `elem` listMap) (G.nodes (domain l))
    nodesK2 = map (\(G.NodeId x) -> x) nodesK
    edgesK = filter (\(G.EdgeId x) -> x `elem` listMap) (G.edges (domain l))
    src e = fromJust $ G.sourceOf (domain l) e
    src2 (G.NodeId e) = e
    tgt e = fromJust $ G.targetOf (domain l) e
    tgt2 (G.NodeId e) = e
    edgesK2 = map (\edg@(G.EdgeId x) -> (x, src2 (src edg), tgt2 (tgt edg))) edgesK

    graphK = G.build nodesK2 edgesK2
    applyN t n = fromJust $ applyNode t n
    applyE t e = fromJust $ applyEdge t e
    nodeTyping = map (\n -> (n, applyN l n)) nodesK
    edgeTyping = map (\e -> (e, applyE l e)) edgesK
    nodeType = map (\(G.NodeId x, G.NodeId y) -> (x,y)) nodeTyping
    edgeType = map (\(G.EdgeId x, G.EdgeId y) -> (x,y)) edgeTyping

instantiateLeft :: GraphMorphism a b -> GraphMorphism a b -> TypedGraphMorphism a b
instantiateLeft k l = typedMorphism k l edges
  where
    ini = empty (domain k) (domain l)
    nodes = foldr (\n -> updateNodes n n) ini (G.nodes (domain k))
    edges = foldr (\e -> updateEdges e e) nodes (G.edges (domain k))

instantiateTgm :: GraphMorphism a b -> GraphMorphism a b -> [Mapping] -> TypedGraphMorphism a b
instantiateTgm s t maps = typedMorphism s t gmMap
  where
    graphS = domain s
    initGm = empty graphS (domain t)
    listNodes = map (\(G.NodeId x) -> x) (G.nodes graphS)
    listInt = map (\(x,y) -> (toN x, toN y)) maps
    gmMap = foldr (\(imag,orig) gm ->
      if orig `elem` listNodes
         then updateNodes (G.NodeId orig) (G.NodeId imag) gm
         else updateEdges (G.EdgeId orig) (G.EdgeId imag) gm)
      initGm listInt

toN :: String -> Int
toN x = read x :: Int
