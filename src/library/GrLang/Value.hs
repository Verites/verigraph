{-# LANGUAGE OverloadedStrings #-}
module GrLang.Value
  (
    Value(..)
  , updateTypeGraph
    -- * Graphs
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
  ) where

import           Data.Function   (on)
import qualified Data.List       as List
import           Data.Maybe      (fromMaybe, isJust, mapMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Text.Prettyprint.Doc (Pretty (..))

import           Base.Annotation (Annotated (..))
import           Base.Location   (Location (..))
import           Data.Graphs     (Graph)
import qualified Data.Graphs     as TypeGraph
import qualified Data.Graphs.Morphism as Graph
import           Data.TypedGraph
import           GrLang.AST
import qualified Util.List       as List

data Value = VGraph GrGraph
    deriving (Show, Eq)

instance Pretty Value where
  pretty (VGraph graph) = pretty (generateGraph "" graph)

-- | Update values when new node/edge types have been created. Only works if the
-- current type graph of the values is a subgraph of the new type graph.
updateTypeGraph :: TypeGraph -> Value -> Value
updateTypeGraph tgraph (VGraph g) = VGraph . fromGraphMorphism . update . toGraphMorphism $ g
  where update morphism = morphism { Graph.codomainGraph = tgraph }


data Metadata = Metadata
  { mdName     :: Maybe Text
  , mdLocation :: Maybe Location }

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
generateGraph :: Text -> TypedGraph Metadata Metadata -> TopLevelDeclaration
generateGraph graphName graph = DeclGraph (A Nothing graphName) (nodes ++ edges)
  where
    (minElemsPerDecl, maxElemsPerDecl) = (3, 5)

    nodes = concatMap (mapMaybe generateNodes . List.chunksOf maxElemsPerDecl) nodesByType
    nodesByType = List.chunksBy (\(_,t,_) -> nodeId t) $ nodesInContext graph
    generateNodes [] = Nothing
    generateNodes ns@((_, ntype, _):_) =
      Just $ DeclNodes [ A Nothing (nodeName n) | (n, _, _) <- ns ] (A Nothing $ nodeName ntype)

    edges = concatMap generateEdges . List.chunksBy sourceTarget $ edgesInContext graph
      where sourceTarget ((s,_,_),_,_,(t,_,_)) = (nodeId s, nodeId t)
    generateEdges es =
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
      SingleType [A Nothing (edgeName e) | (_,e,_,_) <- es] (A Nothing $ edgeName etype)
    generateMultipleTypes = generateEdgeDecl $ \es _ ->
      MultipleTypes [A Nothing (edgeExactName e, edgeName t) | (_,e,t,_) <- es]
    generateEdgeDecl _ [] = Nothing
    generateEdgeDecl makeEdges es@(e:_) =
      let ((s,_,_),_,etype,(t,_,_)) = e
      in Just $ DeclEdges (A Nothing $ nodeName s) (makeEdges es etype) (A Nothing $ nodeName t)

