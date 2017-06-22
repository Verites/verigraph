module XML.GPSReader.GTXLReader (readGrammar) where

import qualified Data.List                     as L
import           System.Directory
import           System.FilePath
import           System.IO

import           Base.Valid

import           Abstract.Rewriting.DPO
import qualified Data.Graphs                   as G
import           Data.TypedGraph.Morphism      as TGM
import           Rewriting.DPO.TypedGraph      as GR
import           XML.GPSReader.GTXLParseIn
import           XML.GPSReader.GTXLPreProcessing

readGrammar :: String -> IO (Grammar (TypedGraphMorphism a b))
readGrammar fileName = do
  print fileName
  files <- getDirectoryContents fileName
  
  -- system.properties file
  let systemPropertiesFile = "system.properties" --head (filter (\name -> takeExtension name == ".properties") files)
      systemPropertiesPath = fileName ++ "/" ++ systemPropertiesFile
  
  handleProperties <- openFile systemPropertiesPath ReadMode
  systemProperties <- hGetContents handleProperties
  
  let typeGraphName = getOption systemProperties "typeGraph="
      stateGraphName = getOption systemProperties "startGraph="
  
  -- type graph
  let typeGraphPathName = fileName ++ "/" ++ typeGraphName ++ ".gty"
  
  parsedTypeGraph <- parseGPR typeGraphPathName
  
  let typesWithId = processTypeGraph parsedTypeGraph      
      typeGraph = instatiateTypeGraph parsedTypeGraph
  
  -- initial state, it uses the rule parser to get the initial state
  let stateGraphPathName = fileName ++ "/" ++ stateGraphName ++ ".gst"
  stateGraph <- parseGPR stateGraphPathName
  let (_,state) = instatiateOneRule typeGraph typesWithId stateGraph
      initialState = domainGraph (getLHS state)
    
  -- rules
  let ruleNames = filter (\name -> takeExtension name == ".gpr") files
      rulePathNames = map ((fileName ++ "/") ++) ruleNames
  parsedRules <- mapM parseGPR rulePathNames
  
  let rules = instatiateRules typeGraph typesWithId parsedRules
  
  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") rules
  _ <- (L.null rules && error "No first order productions were found, at least one is needed.") `seq` return ()
  
  return (grammar initialState [] rules)

-- Auxiliary function to read a text file with grammar properties 
getOption :: String -> String -> String
getOption systemProperties string = tail (L.dropWhile (/= '=') unique)
  where
    inLines = lines systemProperties
    names = filter (L.isPrefixOf string) inLines
    unique = case names of
               []    -> error ("error, '" ++ string ++ "' not found")
               ptg:_ -> ptg

instatiateTypeGraph :: NamedRuleGraph -> G.Graph (Maybe a) (Maybe b)
instatiateTypeGraph (_,(_,types)) = G.build nodesToBuild edgesToBuild
  where
    (nodes,edges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) types
    nodesToBuild = map snd nodes
    edgesToBuild = map (\((src,tgt,_),id) -> (id, getNodeType nodes src, getNodeType nodes tgt)) edges

getNodeType :: [EdgeWithId] -> Node -> Id
getNodeType nodes node = head [id | ((name,_,_),id) <- nodes, node == name]

instatiateRules :: G.Graph (Maybe a) (Maybe b) -> GraphTypes -> [NamedRuleGraph] -> [(RuleName, TypedGraphRule a b)]
instatiateRules typeGraph typesWithId = map (instatiateOneRule typeGraph typesWithId)

instatiateOneRule :: G.Graph (Maybe a) (Maybe b) -> GraphTypes -> NamedRuleGraph -> (RuleName, TypedGraphRule a b)
instatiateOneRule typeGraph (nodeTypes,edgeTypes) rule = (fst rule, instatiateRule typeGraph processedNodes processedEdges)
  where
    (nodes,edges) = snd rule
    (selfLoopEdges,directedEdges) = L.partition (\((from,to,_),_) -> from == to) edges
    (typingEdges,ruleEdges) = L.partition (\((_,_,label),_) -> L.isPrefixOf "type:" label) selfLoopEdges
    (creDelEdges,preservationSelfLoopEdges) =
      L.partition (\((_,_,label),_) -> (any (`L.isPrefixOf` label) ["new:","del:","not:"])) ruleEdges
    edgesToAdd = directedEdges ++ preservationSelfLoopEdges
    
    processedNodes = processNodes nodeTypes nodes creDelEdges typingEdges
    processedEdges = processEdges edgeTypes nodes edgesToAdd

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

addsDeletionNode :: NodeId -> NodeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionNode nodeId ntype r =
  buildProduction
    (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype) (getLHS r))
    (getRHS r)
    (getNACs r)

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
    (getNACs r)

addsDeletionEdge :: EdgeId -> NodeId -> NodeId -> EdgeTypeId -> TypedGraphRule a b -> TypedGraphRule a b
addsDeletionEdge edgeId src tgt etype r =
  buildProduction
    (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype) (getLHS r))
    (getRHS r)
    (getNACs r)

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
    (getNACs r)
