module XML.GPRReader.GXLInstatiator
  ( instatiateTypeGraph
  , instatiateOneRule
  , instatiateRules
  ) where

import qualified Data.List                          as L

import           Abstract.Category.FinitaryCategory (identity)
import           Abstract.Rewriting.DPO
import qualified Data.Graphs                        as G
import           Data.TypedGraph.Morphism           as TGM
import           Rewriting.DPO.TypedGraph           as GR
import           XML.GPRReader.GXLParseIn
import           XML.GPRReader.GXLPreProcessing

instatiateTypeGraph :: ParsedRuleGraph -> G.Graph (Maybe n) (Maybe e)
instatiateTypeGraph (_,(nodes,types)) = G.build nodesToBuild edgesToBuild
  where
    (_,edges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) types
    nodesToBuild = map snd nodes
    edgesToBuild = map (\((src,tgt,_),id) -> (id, idt src, idt tgt)) edges
    idt name = head [nid | (node,nid) <- nodes, node == name]

instatiateRules :: G.Graph (Maybe a) (Maybe b) -> ProcessedTypeGraph -> [ParsedRuleGraph] -> [(String, TypedGraphRule a b)]
instatiateRules typeGraph typesWithId = map (instatiateOneRule typeGraph typesWithId)

instatiateOneRule :: G.Graph (Maybe a) (Maybe b) -> ProcessedTypeGraph -> ParsedRuleGraph -> (String, TypedGraphRule a b)
instatiateOneRule typeGraph types rule = (fst rule, instatiateRule typeGraph processedNodes processedEdges)
  where
    (processedNodes,processedEdges) = processRuleGraph types rule

instatiateRule :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRule typeGraph nodes edges = instatiateRuleEdges (instatiateRuleNodes typeGraph nodes) edges

instatiateRuleNodes :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> TypedGraphRule a b
instatiateRuleNodes typeGraph [] = emptyGraphRule typeGraph
instatiateRuleNodes typeGraph ((n,ntype,cond):nodes) = action cond (instatiateRuleNodes typeGraph nodes)
  where
    action Preservation = addsPreservationNode n ntype
    action Creation = addsCreationNode n ntype
    action Deletion = addsDeletionNode n ntype
    action Forbidden = addsForbiddenNode n ntype

instatiateRuleEdges :: TypedGraphRule a b -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRuleEdges init [] = init
instatiateRuleEdges init ((e,src,tgt,etype,cond):edges) = action cond (instatiateRuleEdges init edges)
  where
    action Preservation = addsPreservationEdge e src tgt etype
    action Creation = addsCreationEdge e src tgt etype
    action Deletion = addsDeletionEdge e src tgt etype
    action Forbidden = addsForbiddenEdge e src tgt etype

addsDeletionNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionNode nodeId ntype r =
  buildProduction
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (getLHS r))
    (getRHS r)
    (map
      (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) .
        TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId))
    (getNACs r))

addsForbiddenNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsForbiddenNode nodeId ntype r =
  buildProduction
    (getLHS r)
    (getRHS r)
    ((TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) isoL) : getNACs r)
  where
    typeGraphL = codomainGraph (getLHS r)
    isoL = identity typeGraphL

addsCreationNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationNode nodeId ntype r =
  buildProduction
    (getLHS r)
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (getRHS r))
    (getNACs r)

addsPreservationNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationNode nodeId ntype r =
  buildProduction
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (getLHS r)))
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (getRHS r)))
    (map
      (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) .
         TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId))
      (getNACs r))

addsDeletionEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionEdge edgeId src tgt etype r =
  buildProduction
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (getLHS r))
    (getRHS r)
    (map
      (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) .
        TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId))
      (getNACs r))

addsForbiddenEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsForbiddenEdge edgeId src tgt etype r =
  buildProduction
    (getLHS r)
    (getRHS r)
    newNac
  where
    left = getLHS r
    typeGraphL = codomainGraph left
    isoL = identity typeGraphL
    -- provisorial solution for adding of forbidden edges.
    -- if: src and tgt of edge in L
    --   then: creates a new NAC with edge
    --   else: for all nacs of r such that src and tgt of edge in nac: adds the edge
    -- it should be processed on GXLPreProcessing, however for now it is only possible to check it here.
    newNac =
      if (G.NodeId src) `elem` nodeIdsFromCodomain left && (G.NodeId tgt) `elem` nodeIdsFromCodomain left
        then ((TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) isoL) : getNACs r)
        else (map addEdgeNAC (getNACs r))
    addEdgeNAC nac =
      if (G.NodeId src) `elem` nodeIdsFromCodomain nac && (G.NodeId tgt) `elem` nodeIdsFromCodomain nac
        then TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) nac
        else nac

addsCreationEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationEdge edgeId src tgt etype r =
  buildProduction
    (getLHS r)
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (getRHS r))
    (getNACs r)

addsPreservationEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationEdge edgeId src tgt etype r =
  buildProduction
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (getLHS r)))
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (getRHS r)))
    (map
      (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) .
        TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId))
      (getNACs r))
