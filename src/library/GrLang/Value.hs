{-# LANGUAGE OverloadedStrings #-}
module GrLang.Value
  (
    Value(..)
  , updateTypeGraph
    -- * Graphs
  , GrRule
  , GrMorphism
  , GrGraph
  , GrNode
  , GrEdge
  , Metadata(..)
  , nodeName
  , nodeExactName
  , nodeLocation
  , edgeName
  , edgeExactName
  , edgeLocation
    -- ** Types
  , TypeGraph
  , NodeType
  , EdgeType
  , nodeTypeName
  , edgeTypeName
  , formatEdgeType
    -- ** Conversion
  , generateTypes
  , generateGraph
  , generateMorphism
  , generateRule
  ) where

import           Data.Function             (on)
import qualified Data.List                 as List
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isJust, mapMaybe)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (Pretty (..))

import           Abstract.Category
import           Base.Annotation           (Annotated (..))
import           Base.Location             (Location (..))
import           Category.TypedGraph       ()
import           Data.Graphs               (Graph)
import qualified Data.Graphs               as TypeGraph
import qualified Data.Graphs.Morphism      as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           GrLang.AST
import           Rewriting.DPO.TypedGraph
import qualified Util.List                 as List
import qualified Util.Map                  as Map

data Value
  = VGraph GrGraph
  | VMorph GrMorphism
  | VRule GrRule
  deriving (Show, Eq)

instance Pretty Value where
  pretty (VGraph graph) = pretty . DeclGraph (A Nothing "") $ generateGraph graph
  pretty (VMorph morph) = pretty . DeclMorphism (A Nothing "")  (A Nothing "dom") (A Nothing "cod") $ generateMorphism morph
  pretty (VRule rule)   = pretty . DeclRule (A Nothing "") $ generateRule rule

-- | Update values when new node/edge types have been created. Only works if the
-- current type graph of the values is a subgraph of the new type graph.
updateTypeGraph :: TypeGraph -> Value -> Value
updateTypeGraph tgraph (VGraph g) =
  VGraph . updateTypedGraph tgraph $ g
updateTypeGraph tgraph (VMorph f) =
  VMorph . updateTypedMorphism tgraph $ f
updateTypeGraph tgraph (VRule (Production l r ns)) =
  VRule $ Production (updateTypedMorphism tgraph l) (updateTypedMorphism tgraph r) (map (updateTypedMorphism tgraph) ns)

updateTypedGraph :: TypeGraph -> GrGraph -> GrGraph
updateTypedGraph tgraph = fromGraphMorphism . update . toGraphMorphism
  where update morphism = morphism { Graph.codomainGraph = tgraph }

updateTypedMorphism :: TypeGraph -> GrMorphism -> GrMorphism
updateTypedMorphism tgraph (TypedGraphMorphism dom cod morph) =
  TypedGraphMorphism (updateTypedGraph tgraph dom)  (updateTypedGraph tgraph cod) morph


data Metadata = Metadata
  { mdName     :: Maybe Text
  , mdLocation :: Maybe Location }
  deriving Show

type GrRule = Production GrMorphism
type GrMorphism = TypedGraphMorphism Metadata Metadata
type GrGraph = TypedGraph Metadata Metadata
type GrNode = Node (Maybe Metadata)
type GrEdge = Edge (Maybe Metadata)

-- | Obtain the exact name of the node, if it has one.
nodeExactName :: GrNode -> Maybe Text
nodeExactName (Node _ metadata) = mdName =<< metadata

-- | Obtain the exact name of the edge, if it has one.
edgeExactName :: GrEdge -> Maybe Text
edgeExactName (Edge _ _ _ metadata) = mdName =<< metadata

-- | Obtain the exact name of the node or create one based on its id.
nodeName :: GrNode -> Text
nodeName (Node n metadata) = nameOrId n metadata

-- | Obtain the exact name of the edge or create one based on its id.
edgeName :: GrEdge -> Text
edgeName (Edge e _ _ metadata) = nameOrId e metadata

nameOrId :: Show id => id -> Maybe Metadata -> Text
nameOrId id metadata = fromMaybe (Text.pack $ '?' : show id) (mdName =<< metadata)

-- | Obtain the location of the node in the source code, if it has one.
nodeLocation :: GrNode -> Maybe Location
nodeLocation (Node _ metadata) = mdLocation =<< metadata

-- | Obtain the location of the edge in the source code, if it has one.
edgeLocation :: GrEdge -> Maybe Location
edgeLocation (Edge _ _ _ metadata) = mdLocation =<< metadata

type TypeGraph = Graph (Maybe Metadata) (Maybe Metadata)
type NodeType = GrNode
type EdgeType = GrEdge

nodeTypeName :: NodeType -> Text
nodeTypeName = nodeName

edgeTypeName :: EdgeType -> NodeType -> NodeType -> Text
edgeTypeName e src tgt = formatEdgeType (edgeName e) (nodeTypeName src) (nodeTypeName tgt)

formatEdgeType :: Text -> Text -> Text -> Text
formatEdgeType e src tgt = Text.concat [ e, ": ", src, " -> ", tgt ]

minElemsPerDecl, maxElemsPerDecl :: Int
(minElemsPerDecl, maxElemsPerDecl) = (3, 5)

-- | Generate declarations for the node/edge types of the given type graph.
--
-- Uses the metadata to define the names of node/edge types.
generateTypes :: TypeGraph -> [TopLevelDeclaration]
generateTypes tgraph = nodeTypes ++ edgeTypes
  where
    nodeTypes =
      [ DeclNodeType (A Nothing $ nodeName n) | n <- TypeGraph.nodes tgraph ]
    edgeTypes =
      [ DeclEdgeType (A Nothing $ edgeName e) (A Nothing $ nodeName src) (A Nothing $ nodeName tgt)
          | ((src, _), e, (tgt, _)) <- TypeGraph.edgesInContext tgraph ]

-- | Generate declarations for the given graph.
--
-- Uses the metadata to define the names of nodes/edges and their types.
generateGraph :: GrGraph -> [GraphDeclaration]
generateGraph graph = generateNodes (nodesInContext graph) ++ generateEdges (edgesInContext graph)

generateNodes :: [NodeInContext Metadata Metadata] -> [GraphDeclaration]
generateNodes nodes = concatMap (mapMaybe generateChunk . List.chunksOf maxElemsPerDecl) nodesByType
  where
    nodesByType = List.chunksBy (\(_,t,_) -> nodeId t) nodes
    generateChunk [] = Nothing
    generateChunk ns@((_, ntype, _):_) =
      Just $ DeclNodes [ A Nothing (nodeName n) | (n, _, _) <- ns ] (A Nothing $ nodeName ntype)

generateEdges :: [EdgeInContext Metadata Metadata] -> [GraphDeclaration]
generateEdges = concatMap generateChunk . List.chunksBy sourceTarget
  where
    sourceTarget ((s,_,_),_,_,(t,_,_)) = (nodeId s, nodeId t)
    generateChunk es =
      let
        (namedEdges, anonEdges) = List.partition (\(_,e,_,_) -> isJust (edgeExactName e)) es
        namedEdgesByType = List.chunksBy edgeType namedEdges
        (namedEdgesByType', smallChunks) = List.partition (\l -> length l > minElemsPerDecl) namedEdgesByType
        mixedEdges = List.sortBy (compare `on` edgeType) (concat smallChunks ++ anonEdges)
      in
        concatMap (mapMaybe generateSingleType . List.chunksOf maxElemsPerDecl) namedEdgesByType'
        ++ (mapMaybe generateMultipleTypes . List.chunksOf maxElemsPerDecl) mixedEdges
      where edgeType (_,_,t,_) = edgeId t

    generateSingleType = generateEdgeDecl $ \es etype ->
      [ ( NamedEdges [A Nothing (edgeName e) | (_,e,_,_) <- es]
        , A Nothing (edgeName etype)) ]
    generateMultipleTypes = generateEdgeDecl $ \es _ ->
      [ ( namedOrAnonymous (edgeExactName e), A Nothing (edgeName t) ) | (_,e,t,_) <- es ]
      where
        namedOrAnonymous Nothing  = AnonymousEdge
        namedOrAnonymous (Just n) = NamedEdges [A Nothing n]
    generateEdgeDecl _ [] = Nothing
    generateEdgeDecl makeEdges es@(e:_) =
      let ((s,_,_),_,etype,(t,_,_)) = e
      in Just $ DeclEdges (A Nothing $ nodeName s) (makeEdges es etype) (A Nothing $ nodeName t)

-- | Generate declarations for the given morphism.
-- Uses the metadata to define the names of nodes/edges and their types.
--
-- Note: will not handle domain and codomain
generateMorphism :: GrMorphism -> [MorphismDeclaration]
generateMorphism morph = mappingsFor nodeName inverseNodeMap ++ mappingsFor edgeName inverseEdgeMap
  where
    inverseNodeMap = preimagesOf (nodeMapping morph) (`lookupNode` domain morph) (`lookupNode` codomain morph)
    inverseEdgeMap = preimagesOf (edgeMapping morph) (`lookupEdge` domain morph) (`lookupEdge` codomain morph)

    mappingsFor nameOf = map (makeMapping nameOf)
    makeMapping nameOf (image, preimages) =
      DeclMapping [ A Nothing $ nameOf x | x <- preimages ] (A Nothing $ nameOf image)

-- | Generate declarations for the given rule.
--
-- Uses the metadata to define the names of nodes/edges and their types.
generateRule :: GrRule -> [RuleDeclaration]
generateRule rule = match : forbids ++ deletes ++ clones ++ creates ++ joins
  where
    match = DeclMatch (generateGraph $ leftObject rule)

    forbids = map generateNac (nacs rule)
    generateNac nac =
      let (nodes, edges) = elementsOutsideImage nac
      in DeclForbid Nothing $ generateNodes nodes ++ generateEdges edges

    deletes =
      let
        (deletedNodes, deletedEdges) = elementsOutsideImage (leftMorphism rule)
        (deletedIsolatedNodes, deletedNodesWithEdges) = List.partition isIsolated deletedNodes
          where isIsolated (_, _, ctx) = List.null (incidentEdges ctx)
        deletedEdgesWithoutNode = filter (not . adjacentToDeletedNode) deletedEdges
          where
            adjacentToDeletedNode ((s,_,_),_,_,(t,_,_)) = nodeId s `Set.member` deletedIds || nodeId t `Set.member` deletedIds
            deletedIds = Set.fromList [ nodeId n | (n,_,_) <- deletedNodesWithEdges ]
      in
        [ DeclDelete [A Nothing (nodeName n) | (n,_,_) <- chunk] WithMatchedEdges
            | chunk <- List.chunksOf maxElemsPerDecl deletedNodesWithEdges ]
        ++ [ DeclDelete [A Nothing (nodeName n) | (n,_,_) <- chunk] Isolated
              | chunk <- List.chunksOf maxElemsPerDecl deletedIsolatedNodes ]
        ++ [ DeclDelete [A Nothing (edgeName e)| (_,e,_,_) <- chunk] Isolated
              | chunk <- List.chunksOf maxElemsPerDecl deletedEdgesWithoutNode ]

    clones =
      let
        nodePreimages = preimagesOf (nodeMapping $ leftMorphism rule) (`lookupNode` interfaceObject rule) (`lookupNode` leftObject rule)
        edgePreimages = preimagesOf (edgeMapping $ leftMorphism rule) (`lookupEdge` interfaceObject rule) (`lookupEdge` leftObject rule)
      in
        [ DeclClone (A Nothing $ nodeName n) [ A Nothing $ nodeName c | c <- clones, nodeName c /= nodeName n ]
            | (n, clones) <- nodePreimages, length clones >= 2 ]
        ++ [ DeclClone (A Nothing $ edgeName e) [ A Nothing $ edgeName c | c <- clones, edgeName c /= edgeName e ]
              | (e, clones) <- edgePreimages, length clones >= 2 ]

    creates = case elementsOutsideImage (rightMorphism rule) of
      ([], []) -> []
      (createdNodes, createdEdges) -> [DeclCreate $ generateNodes createdNodes ++ generateEdges createdEdges]

    joins =
      let
        nodePreimages = preimagesOf (nodeMapping $ rightMorphism rule) (`lookupNode` interfaceObject rule) (`lookupNode` rightObject rule)
        edgePreimages = preimagesOf (edgeMapping $ rightMorphism rule) (`lookupEdge` interfaceObject rule) (`lookupEdge` rightObject rule)
      in
        [ DeclJoin [ A Nothing $ nodeName j | j <- joined ] (Just . A Nothing $ nodeName n)
            | (n, joined) <- nodePreimages, length joined >= 2 ]
        ++ [ DeclJoin [ A Nothing $ edgeName j | j <- joined ] (Just . A Nothing $ edgeName e)
              | (e, joined) <- edgePreimages, length joined >= 2]

elementsOutsideImage :: GrMorphism -> ([NodeInContext Metadata Metadata], [EdgeInContext Metadata Metadata])
elementsOutsideImage morph = (filter (not . isNodeInImage) $ nodesInContext cod, filter (not . isEdgeInImage) $ edgesInContext cod)
  where
    cod = codomain morph
    isNodeInImage (n,_,_) = nodeId n `Map.member` inverseNodeMap
    isEdgeInImage (_,e,_,_) = edgeId e `Map.member` inverseEdgeMap

    inverseNodeMap = Map.inverse (nodeMapping morph)
    inverseEdgeMap = Map.inverse (edgeMapping morph)

preimagesOf :: Ord idB => [(idA, idB)] -> (idA -> Maybe a) -> (idB -> Maybe b) -> [(b, [a])]
preimagesOf mapping lookupDom lookupCod =
  [ (lookupUnsafeCod n, map lookupUnsafeDom clones)
      | (n, clones) <- Map.toList $ Map.inverse mapping ]
  where
    lookupUnsafeDom = fromMaybe (error "generateRule: malformed graph morphism") . lookupDom
    lookupUnsafeCod = fromMaybe (error "generateRule: malformed graph morphism") . lookupCod
