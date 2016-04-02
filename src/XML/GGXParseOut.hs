module XML.GGXParseOut
 ( parseCPGraph
 , parseCSGraph
 , serializeGraph
 , getLHS
 , getRHS
 , getNacs
 , getMappings
 ) where

import qualified Abstract.Morphism         as M
import qualified Analysis.CriticalPairs    as CP
import qualified Analysis.CriticalSequence as CS
import           Data.List.Utils           (startswith)
import           Data.Maybe
import qualified Graph.Graph               as G
import           Graph.GraphGrammar
import qualified Graph.GraphMorphism       as GM
import qualified Graph.GraphRule           as GR
import           Graph.TypedGraphMorphism
import           Text.XML.HXT.Core
import           XML.ParsedTypes

parseCPGraph :: (String,String,[CP.CriticalPair a b]) -> Overlappings
parseCPGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (\x -> (getGraph x, getMapM1 x, getMapM2 x, nacName x, cpType x)) cps
    getGraph = serializeGraph . CP.getM1
    getMapM1 = getTgmMappings . CP.getM1
    getMapM2 = getTgmMappings . CP.getM2
    nacName = parseNacName name2 CP.getCPNac
    cpType = show . CP.getCP

parseCSGraph :: (String,String,[CS.CriticalSequence a b]) -> Overlappings
parseCSGraph (name1,name2,cps) = (name1,name2,overlaps)
  where
    overlaps = map (\x -> (getGraph x, getMapM1 x, getMapM2 x, nacName x, csType x)) cps
    getGraph = serializeGraph . CS.getM1
    getMapM1 = getTgmMappings . CS.getM1
    getMapM2 = getTgmMappings . CS.getM2
    nacName = parseNacName name2 CS.getCSNac
    csType = show . CS.getCS

getTgmMappings :: TypedGraphMorphism a b -> [Mapping]
getTgmMappings nac = nodesMorph ++ edgesMorph
  where
    nodeMap n = fromJust $ applyNodeTGM nac n
    edgeMap e = fromJust $ applyEdgeTGM nac e
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) (nodesDomain nac)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) (edgesDomain nac)

getLHS :: GR.GraphRule a b -> ParsedTypedGraph
getLHS rule = serializeGraph $ GR.left rule

getRHS :: GR.GraphRule a b -> ParsedTypedGraph
getRHS rule = serializeGraph $ GR.right rule

getNacs :: String -> GR.GraphRule a b -> [(ParsedTypedGraph,[Mapping])]
getNacs ruleName rule = map getNac nacsWithIds
  where
    zipIds = zip ([0..]::[Int]) (GR.nacs rule)
    nacsWithIds = map (\(x,y) -> ("NAC_" ++ ruleName ++ "_" ++ show x, y)) zipIds

getNac :: (String, TypedGraphMorphism a b) -> (ParsedTypedGraph, [Mapping])
getNac (nacId,nac) = (graph, mappings)
  where
    (_,n,e) = serializeGraph nac
    graph = (nacId, n, e)
    mappings = getTgmMappings nac

getMappings :: GR.GraphRule a b -> [Mapping]
getMappings rule = nodesMorph ++ edgesMorph
  where
    invL = invertTGM (GR.left rule)
    lr = M.compose invL (GR.right rule)
    nodeMap n = fromJust $ applyNodeTGM lr n
    nodes = filter (isJust . applyNodeTGM lr) (nodesDomain lr)
    nodesMorph = map (\n -> ("N" ++ show (nodeMap n), "N" ++ show n)) nodes
    edgeMap e = fromJust $ applyEdgeTGM lr e
    edges = filter (isJust . applyEdgeTGM lr) (edgesDomain lr)
    edgesMorph = map (\e -> ("E" ++ show (edgeMap e), "E" ++ show e)) edges

parseNacName :: String -> (t -> Maybe Int) -> t -> String
parseNacName ruleName f x = case f x of
                   Just n  -> "NAC_" ++ ruleName ++ "_" ++ show n
                   Nothing -> ""

serializeGraph :: TypedGraphMorphism a b -> ParsedTypedGraph
serializeGraph morphism = ("", nodes, edges)
  where
    graph = M.codomain morphism
    nodes = map (serializeNode graph) (G.nodes $ M.domain graph)
    edges = map (serializeEdge graph) (G.edges $ M.domain graph)

serializeNode :: GM.GraphMorphism a b -> G.NodeId -> ParsedTypedNode
serializeNode graph n = ("N" ++ show n, "N" ++ show (nodeType n))
  where
    nodeType node = fromJust $ GM.applyNode graph node

serializeEdge :: GM.GraphMorphism a b -> G.EdgeId -> ParsedTypedEdge
serializeEdge graph e = ("E" ++ show e,
                         "E" ++ show (edgeType e),
                         "N" ++ show (fromJust (G.sourceOf (M.domain graph) e)),
                         "N" ++ show (fromJust (G.targetOf (M.domain graph) e)))
  where
    edgeType edge = fromJust $ GM.applyEdge graph edge
