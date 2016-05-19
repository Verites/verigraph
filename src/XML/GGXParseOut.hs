module XML.GGXParseOut
 ( parseCPGraph
 , parseCSGraph
 , serializeGraph
 , getLHS
 , getRHS
 , getNacs
 , getMappings
 ) where

import           Abstract.DPO
import qualified Abstract.Morphism         as M
import qualified Analysis.CriticalPairs    as CP
import qualified Analysis.CriticalSequence as CS
import           Data.Maybe                (fromMaybe, isJust)
import qualified Graph.Graph               as G
import qualified Graph.GraphMorphism       as GM
import qualified Graph.GraphRule           as GR
import           Graph.TypedGraphMorphism
import           XML.ParsedTypes

parseCPGraph :: (String,String,[CP.CriticalPair a b]) -> Overlappings
parseCPGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (overlapsCP name2) cps

overlapsCP :: String -> CP.CriticalPair a b -> (ParsedTypedGraph, [Mapping], [Mapping], String, String)
overlapsCP name2 cs = (graph, mapM1, mapM2 ++ mapM2WithNac, nacName cs, csType cs)
  where
    (m1,m2) = case CP.getCP cs of
                CP.ProduceForbid -> fromMaybe (error "Error when exporting ProduceForbid") (CP.getComatch cs)
                _ -> CP.getMatch cs
    graph = serializeGraph [] m1
    mapM1 = getTgmMappings Nothing m1
    mapM2 = getTgmMappings Nothing m2
    mapM2WithNac = case CP.getCP cs of
                     CP.ProduceForbid -> addNacMap
                     _ -> []
    nacMatch = fromMaybe (error "Error when exporting ProduceForbid") (CP.getCPNac cs)
    addNacMap = getTgmMappings (Just (nacName cs)) nacMatch
    nacName = parseNacName name2 CP.getCPNacIdx
    csType = show . CP.getCP

parseCSGraph :: (String,String,[CS.CriticalSequence a b]) -> Overlappings
parseCSGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (overlapsCS name2) cps

overlapsCS :: String -> CS.CriticalSequence a b -> (ParsedTypedGraph, [Mapping], [Mapping], String, String)
overlapsCS name2 cs = (graph, mapM1, mapM2 ++ mapM2WithNac, nacName cs, csType cs)
  where
    (m1,m2) = case CS.getCS cs of
                CS.DeliverDelete -> fromMaybe (error "Error when exporting DeliverDelete") (CS.getMatch cs)
                _ -> CS.getComatch cs
    graph = serializeGraph [] m1
    mapM1 = getTgmMappings Nothing m1
    mapM2 = getTgmMappings Nothing m2
    mapM2WithNac = case CS.getCS cs of
                     CS.DeliverDelete -> addNacMap
                     _ -> []
    nacMatch = fromMaybe (error "Error when exporting DeliverDelete") (CS.getCSNac cs)
    addNacMap = getTgmMappings (Just (nacName cs)) nacMatch
    nacName = parseNacName name2 CS.getCSNacIdx
    csType = show . CS.getCS

getTgmMappings :: Maybe String -> TypedGraphMorphism a b -> [Mapping]
getTgmMappings prefix tgm = nodesMorph ++ edgesMorph
  where
    nodeMap = applyNodeTGMUnsafe tgm
    edgeMap = applyEdgeTGMUnsafe tgm
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), prefix, "N" ++ show n)) (nodesDomain tgm)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), prefix, "E" ++ show e)) (edgesDomain tgm)

getLHS :: [Mapping] -> GR.GraphRule a b -> ParsedTypedGraph
getLHS objName rule = serializeGraph objName $ left rule

getRHS :: [Mapping] -> GR.GraphRule a b -> ParsedTypedGraph
getRHS objName rule = serializeGraph objName $ right rule

getNacs :: String -> GR.GraphRule a b -> [(ParsedTypedGraph,[Mapping])]
getNacs ruleName rule = map getNac nacsWithIds
  where
    zipIds = zip ([0..]::[Int]) (nacs rule)
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
    invL = invertTGM (left rule)
    lr = M.compose invL (right rule)
    nodeMap = applyNodeTGMUnsafe lr
    nodes = filter (isJust . applyNodeTGM lr) (nodesDomain lr)
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), no, "N" ++ show n)) nodes
    edgeMap = applyEdgeTGMUnsafe lr
    edges = filter (isJust . applyEdgeTGM lr) (edgesDomain lr)
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

serializeNode :: [(String,String)] -> GM.GraphMorphism a b -> G.NodeId -> ParsedTypedNode
serializeNode objName graph n = ("N" ++ show n,
                         (lookup (show n) objName),
                         "N" ++ show (GM.applyNodeUnsafe graph n))

serializeEdge :: [(String,String)] -> GM.GraphMorphism a b -> G.EdgeId -> ParsedTypedEdge
serializeEdge objName graph e = ("E" ++ show e,
                         (lookup (show e) objName),
                         "E" ++ show (GM.applyEdgeUnsafe graph e),
                         "N" ++ show (G.sourceOfUnsafe (M.domain graph) e),
                         "N" ++ show (G.targetOfUnsafe (M.domain graph) e))
