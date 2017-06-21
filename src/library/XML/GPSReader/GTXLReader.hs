module XML.GPSReader.GTXLReader where

import qualified Data.List                     as L
import           System.Directory
import           System.FilePath

import           Abstract.Rewriting.DPO
import qualified Data.Graphs                   as G
import           Data.TypedGraph.Morphism      as TGM
import           Rewriting.DPO.TypedGraph      as GR
import           XML.GPSReader.GTXLParseIn
import           XML.GPSReader.GTXLPreProcessing

type Rule = (RuleName,RuleGraph)

tempTypeGraph :: G.Graph (Maybe n) (Maybe e)
tempTypeGraph = G.build [100] [(100,100,100)]

readGrammar :: String -> IO ()
readGrammar fileName = do
  print fileName
  files <- getDirectoryContents fileName
  
  --let stateNames = filter (\name -> takeExtension name == ".gst") files
  let ruleNames = filter (\name -> takeExtension name == ".gpr") files
      ruleFileNames = map (\n -> fileName ++ "/" ++ n) ruleNames
  
  t <- mapM parseGPR ruleFileNames
  
  print (instatiateOneRule (t!!0))
  
  return ()

instatiateOneRule :: Rule -> (RuleName, TypedGraphRule a b)
instatiateOneRule rule = (fst rule, instatiateRule processedNodes processedEdges)
  where
    (nodes,edges) = snd rule
    (selfLoopEdges,directedEdges) = L.partition (\((from,to,_),_) -> from == to) edges
    (typingEdges,ruleEdges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) selfLoopEdges
    
    processedNodes = processNodes nodes ruleEdges typingEdges
    processedEdges = processEdges nodes directedEdges

instatiateRule :: [ProcessedNode] -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRule nodes = instatiateRuleEdges (instatiateRuleNodes nodes)

instatiateRuleNodes :: [ProcessedNode] -> TypedGraphRule a b
instatiateRuleNodes [] = emptyGraphRule tempTypeGraph
instatiateRuleNodes ((n,ntype,cond):nodes) = action cond (instatiateRuleNodes nodes)
  where
    action Preservation = addsPreservationNode n ntype
    action Creation = addsCreationNode n ntype
    action Deletion = addsDeletionNode n ntype

instatiateRuleEdges :: TypedGraphRule a b -> [ProcessedEdge] -> TypedGraphRule a b
instatiateRuleEdges init [] = init
instatiateRuleEdges init ((e,src,tgt,etype,cond):edges) = action cond (instatiateRuleEdges init edges)
  where
    action Preservation = addsPreservationEdge e src tgt etype
    action Creation = addsCreationEdge e src tgt etype
    action Deletion = addsDeletionEdge e src tgt etype

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
