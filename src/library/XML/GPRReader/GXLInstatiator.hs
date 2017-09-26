module XML.GPRReader.GXLInstatiator
  ( instatiateTypeGraph
  , instatiateRules
  , instatiateRule
  ) where

import qualified Data.List                          as L

import           Abstract.Category                  
import           Abstract.Rewriting.DPO
import qualified Data.Graphs                        as G
import           Data.TypedGraph                    hiding (NodeId, EdgeId)
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
instatiateRules typeGraph typesWithId = map (instatiateRule typeGraph typesWithId)

instatiateRule :: G.Graph (Maybe a) (Maybe b) -> ProcessedTypeGraph -> ParsedRuleGraph -> (String, TypedGraphRule a b)
instatiateRule typeGraph types rule = (fst rule, ruleWithNacs)
  where
    (processedNodes,processedEdges) = processRuleGraph types rule

    (forbiddenNodes,nodesWithoutNac) =
      L.partition (\(_,_,cond) -> cond `notElem` [Preservation,Creation,Deletion]) processedNodes

    (forbiddenEdges,edgesWithoutNac) =
      L.partition (\(_,_,_,_,cond) -> cond == ForbiddenEdge) processedEdges

    (nacsWithOnlyOneEdge,edgesInNacsWithNodes) =
      L.partition (\(_,src,tgt,_,_) -> G.NodeId src `elem` nodesL && G.NodeId tgt `elem` nodesL) forbiddenEdges
    nodesL = nodeIds . codomain $ leftMorphism ruleWithoutNacs

    groupNodesByNac =
      L.groupBy
        (\(_,_,ForbiddenNode x) (_,_,ForbiddenNode y) -> x == y)
        (L.sortBy (\(_,_,ForbiddenNode x) (_,_,ForbiddenNode y) -> compare x y) forbiddenNodes)

    ruleWithoutNacs = instatiateRuleEdges (instatiateRuleNodes typeGraph nodesWithoutNac) edgesWithoutNac

    nacsWithOnlyNodes = map (instatiateNacNodes ruleWithoutNacs) groupNodesByNac
    nacs1 = instatiateNacEdges edgesInNacsWithNodes nacsWithOnlyNodes
    nacs2 = instatiateLonelyNacEdges (leftMorphism ruleWithoutNacs) nacsWithOnlyOneEdge

    ruleWithNacs = Production (leftMorphism ruleWithoutNacs) (rightMorphism ruleWithoutNacs) (nacs1 ++ nacs2)

instatiateNacNodes :: Production (TypedGraphMorphism a b) -> [ProcessedNode] -> TypedGraphMorphism a b
instatiateNacNodes r =
  foldr (\(n,ntype,_) -> TGM.createNodeOnCodomain (G.NodeId n) (G.NodeId ntype)) isoL
  where
    isoL = identity (leftObject r)

instatiateNacEdges :: [ProcessedEdge] -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b]
instatiateNacEdges _ [] = []
instatiateNacEdges edges (n:nacs) = foldr insertEdge n edges : instatiateNacEdges edges nacs
  where
    insertEdge (e,src,tgt,tp,_) n =
      if G.NodeId src `elem` nodeIds (codomain n) && G.NodeId tgt `elem` nodeIds (codomain n)
        then TGM.createEdgeOnCodomain (G.EdgeId e) (G.NodeId src) (G.NodeId tgt) (G.EdgeId tp) n
        else n

instatiateLonelyNacEdges :: TypedGraphMorphism a b -> [ProcessedEdge] -> [TypedGraphMorphism a b]
instatiateLonelyNacEdges l = map insertEdge
  where
    isoL = identity (codomainGraph l)

    insertEdge (e,src,tgt,tp,_) =
      createEdgeOnCodomain (G.EdgeId e) (G.NodeId src) (G.NodeId tgt) (G.EdgeId tp) isoL

instatiateRuleNodes :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> TypedGraphRule a b
instatiateRuleNodes typeGraph [] = emptyGraphRule typeGraph
instatiateRuleNodes typeGraph ((n,ntype,cond):nodes) = action cond (instatiateRuleNodes typeGraph nodes)
  where
    action Preservation = addsPreservationNode n ntype
    action Creation     = addsCreationNode n ntype
    action Deletion     = addsDeletionNode n ntype
    action _            = error "instatiateRuleNodes: it's not supposed to be here"

instatiateRuleEdges :: TypedGraphRule a b -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRuleEdges init [] = init
instatiateRuleEdges init ((e,src,tgt,etype,cond):edges) = action cond (instatiateRuleEdges init edges)
  where
    action Preservation = addsPreservationEdge e src tgt etype
    action Creation     = addsCreationEdge e src tgt etype
    action Deletion     = addsDeletionEdge e src tgt etype
    action _            = error "instatiateRuleEdges: it's not supposed to be here"

addsDeletionNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionNode nodeId ntype r =
  Production
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (leftMorphism r))
    (rightMorphism r)
    []

addsCreationNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationNode nodeId ntype r =
  Production
    (leftMorphism r)
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (rightMorphism r))
    []

addsPreservationNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationNode nodeId ntype r =
  Production
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (leftMorphism r)))
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (rightMorphism r)))
    []

addsDeletionEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionEdge edgeId src tgt etype r =
  Production
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (leftMorphism r))
    (rightMorphism r)
    []

addsCreationEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationEdge edgeId src tgt etype r =
  Production
    (leftMorphism r)
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (rightMorphism r))
    []

addsPreservationEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationEdge edgeId src tgt etype r =
  Production
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (leftMorphism r)))
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (rightMorphism r)))
    []
