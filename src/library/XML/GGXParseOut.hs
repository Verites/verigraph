module XML.GGXParseOut
 ( parseCPGraph
 , parseCSGraph
 , serializeGraph
 , XML.GGXParseOut.getLHS
 , XML.GGXParseOut.getRHS
 , getNacs
 , getMappings
 ) where

import           Data.Maybe                (fromMaybe, isJust)

import           Abstract.Category         as FC
import           Abstract.Rewriting.DPO
import qualified Analysis.CriticalPairs    as CP
import qualified Analysis.CriticalSequence as CS
import qualified Data.Graphs               as G
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import qualified Rewriting.DPO.TypedGraph  as GR
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
    (graph, mapM1, mapM2) = serializePair m1 m2
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
    (graph, mapM1, mapM2) = serializePair m1 m2
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
    nodeMap = applyNodeIdUnsafe tgm
    edgeMap = applyEdgeIdUnsafe tgm
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), prefix, "N" ++ show n)) (nodeIds $ domain tgm)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), prefix, "E" ++ show e)) (edgeIds $ domain tgm)

getLHS :: [Mapping] -> [Mapping] -> GR.TypedGraphRule a b -> ParsedTypedGraph
getLHS objNameN objNameE rule = serializeGraph objNameN objNameE $ GR.leftMorphism rule

getRHS :: [Mapping] -> [Mapping] -> GR.TypedGraphRule a b -> ParsedTypedGraph
getRHS objNameN objNameE rule = serializeGraph objNameN objNameE $ GR.rightMorphism rule

getNacs :: String -> GR.TypedGraphRule a b -> [(ParsedTypedGraph,[Mapping])]
getNacs ruleName rule = map getNac nacsWithIds
  where
    zipIds = zip ([0..]::[Int]) (nacs rule)
    nacsWithIds = map (\(x,y) -> ("NAC_" ++ ruleName ++ "_" ++ show x, y)) zipIds

getNac :: (String, TypedGraphMorphism a b) -> (ParsedTypedGraph, [Mapping])
getNac (nacId,nac) = (graph, mappings)
  where
    (_,n,e) = serializeGraph [] [] nac
    graph = (nacId, n, e)
    mappings = getTgmMappings Nothing nac

getMappings :: GR.TypedGraphRule a b -> [Mapping]
getMappings rule = nodesMorph ++ edgesMorph
  where
    no = Nothing
    invL = invert (GR.leftMorphism rule)
    lr = GR.rightMorphism rule <&> invL
    nodeMap = applyNodeIdUnsafe lr
    nodes = filter (isJust . applyNodeId lr) (nodeIds $ domain lr)
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), no, "N" ++ show n)) nodes
    edgeMap = applyEdgeIdUnsafe lr
    edges = filter (isJust . applyEdgeId lr) (edgeIds $ domain lr)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), no, "E" ++ show e)) edges

parseNacName :: String -> (t -> Maybe Int) -> t -> String
parseNacName ruleName f x = case f x of
                   Just n  -> "NAC_" ++ ruleName ++ "_" ++ show n
                   Nothing -> ""

serializePair :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (ParsedTypedGraph, [Mapping], [Mapping])
serializePair m1 m2 = (serializeGraph [] [] m1, getTgmMappings Nothing m1, getTgmMappings Nothing m2)

serializeGraph :: [Mapping] -> [Mapping] -> TypedGraphMorphism a b -> ParsedTypedGraph
serializeGraph objNameNodes objNameEdges morphism = ("", nodes, edges)
  where
    graph = FC.codomain morphism
    nodes = map (serializeNode (map (\(x,_,y) -> (x,y)) objNameNodes) graph) (G.nodeIds $ FC.domain graph)
    edges = map (serializeEdge (map (\(x,_,y) -> (x,y)) objNameEdges) graph) (G.edges $ FC.domain graph)

serializeNode :: [(String,String)] -> TypedGraph a b -> G.NodeId -> ParsedTypedNode
serializeNode objName graph n = ("N" ++ show n,
                         lookup (show n) objName,
                         "N" ++ show (extractNodeType graph n))

serializeEdge :: [(String,String)] -> TypedGraph a b -> G.Edge (Maybe b) -> ParsedTypedEdge
serializeEdge objName graph e = ("E" ++ show (G.edgeId e),
                         lookup (show (G.edgeId e)) objName,
                         "E" ++ show (extractEdgeType graph (G.edgeId e)),
                         "N" ++ show (G.sourceId e),
                         "N" ++ show (G.targetId e))
