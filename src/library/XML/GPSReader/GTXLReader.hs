module XML.GPSReader.GTXLReader (readGrammar) where

import qualified Data.List                     as L
import           System.Directory
import           System.FilePath
import           System.IO

import           Base.Valid

import           Abstract.Category.FinitaryCategory
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
  let (_,stateRule) = instatiateOneRule typeGraph typesWithId stateGraph
      initialState = domainGraph (getLHS stateRule)
  
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
instatiateRule typeGraph nodes edges = removeIsoNacs preRule
  where
    preRule = instatiateRuleEdges (instatiateRuleNodes typeGraph nodes) edges
    removeIsoNacs r = buildProduction (getLHS r) (getRHS r) (filter (not . isIsomorphism) (getNACs r))

instatiateRuleNodes :: G.Graph (Maybe a) (Maybe b) -> [ProcessedNode] -> TypedGraphRule a b
instatiateRuleNodes typeGraph [] = emptyRuleEmptyNac
  where
    emptyRule = emptyGraphRule typeGraph
    emptyRuleEmptyNac = buildProduction (getLHS emptyRule) (getRHS emptyRule) [getLHS emptyRule]
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
    (map (TGM.createNodeOnCodomain (G.NodeId nodeId) (G.NodeId ntype)) (getNACs r))

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
    (map (TGM.createEdgeOnCodomain (G.EdgeId edgeId) (G.NodeId src) (G.NodeId tgt) (G.EdgeId etype)) (getNACs r))

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
