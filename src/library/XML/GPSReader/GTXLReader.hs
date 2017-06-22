module XML.GPSReader.GTXLReader where

import qualified Data.List                     as L
import           System.Directory
import           System.FilePath

import           Base.Valid

import           Abstract.Rewriting.DPO
import qualified Data.Graphs                   as G
import           Data.TypedGraph.Morphism      as TGM
import           Rewriting.DPO.TypedGraph      as GR
import           XML.GPSReader.GTXLParseIn
import           XML.GPSReader.GTXLPreProcessing

readGrammar :: String -> IO ()
readGrammar fileName = do
  print fileName
  files <- getDirectoryContents fileName
  
  --let stateNames = filter (\name -> takeExtension name == ".gst") files
  let ruleNames = filter (\name -> takeExtension name == ".gpr") files
      typeGraphName = head (filter (\name -> takeExtension name == ".gty") files)
      
      rulePathNames = map (\n -> fileName ++ "/" ++ n) ruleNames
      typeGraphPathName = fileName ++ "/" ++ typeGraphName
      
  parsedTypeGraph <- parseGPR typeGraphPathName
  
  let toProcess = snd . snd
      processedTypes = processTypeGraph (toProcess parsedTypeGraph)      
      typeGraph = instatiateTypeGraph parsedTypeGraph
      
  --print processedTypes

  parsedRules <- mapM parseGPR rulePathNames
  
  --print parsedRules
  
  let rules = map (instatiateOneRule typeGraph processedTypes) parsedRules
  --print (rules!!1)
  --print typeGraph
  
  print rules
  print (map (Base.Valid.isValid . snd) rules)
  
  return ()

instatiateTypeGraph :: NamedRuleGraph -> G.Graph (Maybe a) (Maybe b)
instatiateTypeGraph (_,(_,types)) = G.build nodesToBuild edgesToBuild
  where
    (nodes,edges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) types
    nodesToBuild = map snd nodes
    edgesToBuild = map (\((src,tgt,_),id) -> (id, getNodeType nodes src, getNodeType nodes tgt)) edges

getNodeType :: [EdgeWithId] -> String -> Id
getNodeType nodes node = head [id | ((name,_,_),id) <- nodes, node == name]

instatiateOneRule :: G.Graph (Maybe a) (Maybe b) -> ([NodeWithId],[NodeWithId]) -> NamedRuleGraph -> (RuleName, TypedGraphRule a b)
instatiateOneRule typeGraph (nodeTypes,edgeTypes) rule = (fst rule, instatiateRule typeGraph processedNodes processedEdges)
  where
    (nodes,edges) = snd rule
    (selfLoopEdges,directedEdges) = L.partition (\((from,to,_),_) -> from == to) edges
    (typingEdges,ruleEdges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) selfLoopEdges
    
    processedNodes = processNodes nodeTypes nodes ruleEdges typingEdges
    processedEdges = processEdges edgeTypes nodes directedEdges

instatiateRule :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRule typeGraph nodes = instatiateRuleEdges (instatiateRuleNodes typeGraph nodes)

instatiateRuleNodes :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> TypedGraphRule a b
instatiateRuleNodes typeGraph [] = emptyGraphRule typeGraph
instatiateRuleNodes typeGraph ((n,ntype,cond):nodes) = action cond (instatiateRuleNodes typeGraph nodes)
  where
    action Preservation = addsPreservationNode n ntype
    action Creation = addsCreationNode n ntype
    action Deletion = addsDeletionNode n ntype
    action Forbidden = Prelude.id

instatiateRuleEdges :: TypedGraphRule a b -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRuleEdges init [] = init
instatiateRuleEdges init ((e,src,tgt,etype,cond):edges) = action cond (instatiateRuleEdges init edges)
  where
    action Preservation = addsPreservationEdge e src tgt etype
    action Creation = addsCreationEdge e src tgt etype
    action Deletion = addsDeletionEdge e src tgt etype
    action Forbidden = Prelude.id

addsDeletionNode :: Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionNode nodeId ntype r =
  buildProduction
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (getLHS r))
    (getRHS r)
    (getNACs r)

addsCreationNode :: Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationNode nodeId ntype r =
  buildProduction
    (getLHS r)
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (getRHS r))
    (getNACs r)

addsPreservationNode :: Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationNode nodeId ntype r =
  buildProduction
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (getLHS r)))
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)
      (TGM.createNodeOnDomain (G.NodeId nodeId) (G.NodeId ntype) (G.NodeId nodeId) (getRHS r)))
    (getNACs r)

addsDeletionEdge :: Id -> Id -> Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionEdge edgeId src tgt etype r =
  buildProduction
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (getLHS r))
    (getRHS r)
    (getNACs r)

addsCreationEdge :: Id -> Id -> Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsCreationEdge edgeId src tgt etype r =
  buildProduction
    (getLHS r)
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (getRHS r))
    (getNACs r)

addsPreservationEdge :: Id -> Id -> Id -> Id -> TypedGraphRule a b -> TypedGraphRule a b
addsPreservationEdge edgeId src tgt etype r =
  buildProduction
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (getLHS r)))
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)
      (TGM.createEdgeOnDomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (G.EdgeId edgeId) (getRHS r)))
    (getNACs r)
