module XML.GGXReader.Span where

import           Abstract.Category
import           Abstract.Rewriting.DPO
import qualified Data.Graphs                   as G
import           Data.Graphs.Morphism          as GM
import           Data.Maybe                    (fromMaybe)
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph      as GR

import           XML.ParsedTypes
import           XML.Utilities

type TypeGraph a b = G.Graph (Maybe a) (Maybe b)

instantiateRule :: TypeGraph a b -> RuleWithNacs -> TypedGraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = Production lhsTgm rhsTgm nacsTgm
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
      | edgeIdSrc `elem` edgesLeft && edgeIdTgt `elem` edgesRight = (newEdgeK, updateEdgesL, updateEdgesR)
      | otherwise = (k, l, r)
      where
        edgeIdSrc = G.EdgeId src
        edgeIdTgt = G.EdgeId tgt

        edgeSrc = fromMaybe
                    (error "updateEdgeMorphisms: edgeId is not in its graph")
                    (G.lookupEdge edgeIdSrc (domain left))

        edgeDom = G.insertEdge edgeIdSrc (G.sourceId edgeSrc) (G.targetId edgeSrc) (domain k)

        edgeType = extractEdgeType left edgeIdSrc
        newEdgeK = updateEdges edgeIdSrc edgeType (updateDomain edgeDom k)
        updateEdgesL = updateEdges edgeIdSrc edgeIdSrc (updateDomain edgeDom l)
        updateEdgesR = updateEdges edgeIdSrc edgeIdTgt (updateDomain edgeDom r)


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
