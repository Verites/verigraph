module XML.GGXReader.Span where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Data.Maybe               (fromMaybe)
import qualified Graph.Graph              as G
import           Graph.GraphMorphism      as GM
import           TypedGraph.DPO.GraphRule as GR
import           TypedGraph.Graph
import           TypedGraph.Morphism
import           XML.ParsedTypes
import           XML.Utilities

type TypeGraph a b = G.Graph (Maybe a) (Maybe b)

instantiateRule :: TypeGraph a b -> RuleWithNacs -> GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = buildProduction lhsTgm rhsTgm nacsTgm
  where
    lm = instantiateTypedGraph lhs typeGraph
    rm = instantiateTypedGraph rhs typeGraph
    (lhsTgm, rhsTgm) = instantiateSpan lm rm mappings
    nacsTgm = map (instantiateNac lm typeGraph) nacs

instantiateNac :: TypedGraph a b -> G.Graph (Maybe a) (Maybe b) -> Nac -> TypedGraphMorphism a b
instantiateNac lhs tg (nacGraph, maps) = nacTgm
  where
    nacMorphism = instantiateTypedGraph nacGraph tg
    (_,nacTgm) = instantiateSpan lhs nacMorphism maps


instantiateTypedGraph :: ParsedTypedGraph -> TypeGraph a b -> GraphMorphism (Maybe a) (Maybe b)
instantiateTypedGraph (_, nodes, edges) tg = buildGraphMorphism g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG

    nodesG = map (toN . fstOfThree) nodes
    edgesG = map (\(id,_,_,src,tgt) -> (toN id, toN src, toN tgt)) edges

    nodeTyping = map (\(id,_,typ) -> (toN id, toN typ)) nodes
    edgeTyping = map (\(id,_,typ,_,_) -> (toN id, toN typ)) edges


instantiateSpan :: TypedGraph a b -> TypedGraph a b -> [Mapping] -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
instantiateSpan left right mapping = (leftM, rightM)
  where
    parsedMap = map (\(t,_,s) -> (toN t, toN s)) mapping

    leftM = buildTypedGraphMorphism k left leftMap
    rightM = buildTypedGraphMorphism k right rightMap

    nodesLeft = G.nodeIds (domain left)
    nodesRight = G.nodeIds (domain right)

    edgesLeft = G.edgeIds (domain left)
    edgesRight = G.edgeIds (domain right)

    typegraph = codomain left
    initK = empty G.empty typegraph
    initL = empty G.empty (domain left)
    initR = empty G.empty (domain right)

    updateEdgeMorphisms (k,l,r) (tgt,src)
      | edgeSrc `elem` edgesLeft && edgeTgt `elem` edgesRight = (newEdgeK, updateEdgesL, updateEdgesR)
      | otherwise = (k, l, r)
      where
        edgeSrc = G.EdgeId src
        edgeTgt = G.EdgeId tgt
        src_ e = fromMaybe (error (show e)) (G.sourceOf (domain left) e)
        tgt_ e = fromMaybe (error (show e)) (G.targetOf (domain left) e)
        edgeDom
          = G.insertEdge edgeSrc (src_ edgeSrc) (tgt_ edgeSrc) (domain k)
        edgeType = extractEdgeType left edgeSrc
        newEdgeK = updateEdges edgeSrc edgeType (updateDomain edgeDom k)
        updateEdgesL = updateEdges edgeSrc edgeSrc (updateDomain edgeDom l)
        updateEdgesR = updateEdges edgeSrc edgeTgt (updateDomain edgeDom r)


    updateMorphisms (k,l,r) (tgt,src)
      | nodeSrc `elem` nodesLeft && nodeTgt `elem` nodesRight = (newNodeK, updateNodesL, updateNodesR)
      | otherwise = (k, l, r)
      where nodeSrc = G.NodeId src
            nodeTgt = G.NodeId tgt
            nodeDom = G.insertNode nodeSrc (domain k)
            nodeType = extractNodeType left nodeSrc
            newNodeK = updateNodes nodeSrc nodeType (updateDomain nodeDom k)
            updateNodesL = updateNodes nodeSrc nodeSrc (updateDomain nodeDom l)
            updateNodesR = updateNodes nodeSrc nodeTgt (updateDomain nodeDom r)


    (k, leftMap, rightMap) = foldl updateEdgeMorphisms morphismWithNodes parsedMap
    morphismWithNodes = foldl updateMorphisms (initK, initL, initR) parsedMap
