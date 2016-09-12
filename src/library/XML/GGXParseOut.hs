module XML.GGXParseOut
 ( parseCPGraph
 , parseCSGraph
 , serializeGraph
 , XML.GGXParseOut.getLHS
 , XML.GGXParseOut.getRHS
 , getNacs
 , getMappings
 ) where

import           Abstract.DPO
import qualified Abstract.Morphism         as M
import qualified Analysis.CriticalPairs    as CP
import qualified Analysis.CriticalSequence as CS
import           Data.Maybe                (fromMaybe, isJust)
import qualified Graph.Graph               as G
import           TypedGraph.Graph
import qualified TypedGraph.GraphRule      as GR
import           TypedGraph.Morphism
import           XML.ParsedTypes

parseCPGraph :: (String,String,[CP.CriticalPair (TypedGraphMorphism a b)]) -> Overlappings
parseCPGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (overlapsCP name2) cps

overlapsCP :: String -> CP.CriticalPair (TypedGraphMorphism a b) -> (ParsedTypedGraph, [Mapping], [Mapping], String, String)
overlapsCP name2 cs = (graph, mapM1, mapM2 ++ mapM2WithNac, nacName cs, csType cs)
  where
    (m1,m2) = case CP.getCriticalPairType cs of
                CP.ProduceForbid -> fromMaybe (error "Error when exporting ProduceForbid") (CP.getCriticalPairComatches cs)
                _ -> CP.getCriticalPairMatches cs
    graph = serializeGraph [] m1
    mapM1 = getTgmMappings Nothing m1
    mapM2 = getTgmMappings Nothing m2
    mapM2WithNac = case CP.getCriticalPairType cs of
                     CP.ProduceForbid -> addNacMap
                     _                -> []
    nacMatch = fromMaybe (error "Error when exporting ProduceForbid") (CP.getNacMatchOfCriticalPair cs)
    addNacMap = getTgmMappings (Just (nacName cs)) nacMatch
    nacName = parseNacName name2 CP.getNacIndexOfCriticalPair
    csType = show . CP.getCriticalPairType

parseCSGraph :: (String,String,[CS.CriticalSequence (TypedGraphMorphism a b)]) -> Overlappings
parseCSGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (overlapsCS name2) cps

overlapsCS :: String -> CS.CriticalSequence (TypedGraphMorphism a b)
          -> (ParsedTypedGraph, [Mapping], [Mapping], String, String)
overlapsCS name2 cs = (graph, mapM1, mapM2 ++ mapM2WithNac, nacName cs, csType cs)
  where
    (m1,m2) = case CS.getCriticalSequenceType cs of
                CS.DeleteForbid -> fromMaybe (error "Error when exporting DeleteForbid") (CS.getCriticalSequenceMatches cs)
                CS.ForbidProduce -> fromMaybe (error "Error when exporting ForbidProduce") (CS.getCriticalSequenceMatches cs)
                _ -> CS.getCriticalSequenceComatches cs
    graph = serializeGraph [] m1
    mapM1 = getTgmMappings Nothing m1
    mapM2 = getTgmMappings Nothing m2
    mapM2WithNac = case CS.getCriticalSequenceType cs of
                     CS.DeleteForbid  -> addNacMap
                     CS.ForbidProduce -> addNacMap
                     _                -> []
    nacMatch = fromMaybe (error "Error when exporting DeleteForbid or ForbidProduce") (CS.getNacMatchOfCriticalSequence cs)
    addNacMap = getTgmMappings (Just (nacName cs)) nacMatch
    nacName = parseNacName name2 CS.getNacIndexOfCriticalSequence
    csType = show . CS.getCriticalSequenceType

getTgmMappings :: Maybe String -> TypedGraphMorphism a b -> [Mapping]
getTgmMappings prefix tgm = nodesMorph ++ edgesMorph
  where
    nodeMap = applyNodeUnsafe tgm
    edgeMap = applyEdgeUnsafe tgm
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), prefix, "N" ++ show n)) (nodesFromDomain tgm)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), prefix, "E" ++ show e)) (edgesFromDomain tgm)

getLHS :: [Mapping] -> GR.GraphRule a b -> ParsedTypedGraph
getLHS objName rule = serializeGraph objName $ GR.getLHS rule

getRHS :: [Mapping] -> GR.GraphRule a b -> ParsedTypedGraph
getRHS objName rule = serializeGraph objName $ GR.getRHS rule

getNacs :: String -> GR.GraphRule a b -> [(ParsedTypedGraph,[Mapping])]
getNacs ruleName rule = map getNac nacsWithIds
  where
    zipIds = zip ([0..]::[Int]) (getNACs rule)
    nacsWithIds = map (\(x,y) -> ("NAC_" ++ ruleName ++ "_" ++ show x, y)) zipIds

getNac :: (String, TypedGraphMorphism a b) -> (ParsedTypedGraph, [Mapping])
getNac (nacId,nac) = (graph, mappings)
  where
    (_,n,e) = serializeGraph [] nac
    graph = (nacId, n, e)
    mappings = getTgmMappings Nothing nac

getMappings :: GR.GraphRule a b -> [Mapping]
getMappings rule = nodesMorph ++ edgesMorph
  where
    no = Nothing
    invL = invert (GR.getLHS rule)
    lr = M.compose invL (GR.getRHS rule)
    nodeMap = applyNodeUnsafe lr
    nodes = filter (isJust . applyNode lr) (nodesFromDomain lr)
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), no, "N" ++ show n)) nodes
    edgeMap = applyEdgeUnsafe lr
    edges = filter (isJust . applyEdge lr) (edgesFromDomain lr)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), no, "E" ++ show e)) edges

parseNacName :: String -> (t -> Maybe Int) -> t -> String
parseNacName ruleName f x = case f x of
                   Just n  -> "NAC_" ++ ruleName ++ "_" ++ show n
                   Nothing -> ""

serializeGraph :: [Mapping] -> TypedGraphMorphism a b -> ParsedTypedGraph
serializeGraph objName morphism = ("", nodes, edges)
  where
    graph = M.codomain morphism
    nodes = map (serializeNode (map (\(x,_,y) -> (x,y)) objName) graph) (G.nodes $ M.domain graph)
    edges = map (serializeEdge (map (\(x,_,y) -> (x,y)) objName) graph) (G.edges $ M.domain graph)

serializeNode :: [(String,String)] -> TypedGraph a b -> G.NodeId -> ParsedTypedNode
serializeNode objName graph n = ("N" ++ show n,
                         lookup (show n) objName,
                         "N" ++ show (getNodeType graph n))

serializeEdge :: [(String,String)] -> TypedGraph a b -> G.EdgeId -> ParsedTypedEdge
serializeEdge objName graph e = ("E" ++ show e,
                         lookup (show e) objName,
                         "E" ++ show (getEdgeType graph e),
                         "N" ++ show (G.sourceOfUnsafe (M.domain graph) e),
                         "N" ++ show (G.targetOfUnsafe (M.domain graph) e))
