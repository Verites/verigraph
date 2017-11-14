{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang.Compiler
  ( compileFile
  , compile
  , compileDecl
  , compileGraph
  ) where

import           Control.Monad.Except           (ExceptT (..), mapExceptT, runExceptT)
import           Control.Monad.State
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Text.Lazy.IO              as Text
import           Data.Text.Prettyprint.Doc      (Pretty (..), (<>))
import qualified Data.Text.Prettyprint.Doc      as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import           System.FilePath                (takeDirectory, (</>))
import           System.IO.Error                (ioeGetErrorString, tryIOError)

import           Base.Annotation                (Annotated (..), Located)
import           Base.Location
import           Data.TypedGraph                (Edge (..), EdgeId, Node (..), NodeId)
import qualified Data.TypedGraph                as TGraph
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.Value
import qualified Util.Map                       as Map
import           Util.Monad

compileFile :: (MonadIO m, MonadGrLang m) => FilePath -> ExceptT Error m ()
compileFile path = importIfNeeded_ path $
  loadModule (A Nothing path) >>= compile

compile :: (MonadIO m, MonadGrLang m) => [TopLevelDeclaration] -> ExceptT Error m ()
compile = mapMCollectErrors_ compileDecl

errorToMaybe :: Either a t -> Maybe a
errorToMaybe (Left err) = Just err
errorToMaybe (Right _)  = Nothing

compileDecl :: (MonadIO m, MonadGrLang m) => TopLevelDeclaration -> ExceptT Error m ()
compileDecl (DeclNodeType n) = addNodeType n
compileDecl (DeclEdgeType e s t) = addEdgeType e s t
compileDecl (DeclGraph name graphDecls) =
  putValue name . VGraph =<< compileGraph graphDecls
compileDecl (Import (A loc path)) = do
  -- Interpret the imported path relative to the directory of the current path
  let path' = case loc of
        Nothing -> path
        Just (Location currPath _) -> takeDirectory currPath </> path
  importIfNeeded_ path' $
    loadModule (A loc path') >>= compile

loadModule :: (MonadIO m, MonadGrLang m) => Located FilePath -> ExceptT Error m [TopLevelDeclaration]
loadModule (A loc path) = do
  textOrError <- liftIO . tryIOError $ Text.readFile path
  case textOrError of
    Right text -> parseModule path text
    Left ioError -> throwError loc . PP.fillSep $
      PP.words "Error reading file" ++ [ PP.dquotes (pretty path) <> PP.colon, pretty (ioeGetErrorString ioError) ]

type GrElem = Either (GrNode, NodeType) (GrEdge, EdgeType)

data GraphState = GrSt
  { grNamedElements  :: Map Text GrElem
  , grAnonymousEdges :: [(GrEdge, EdgeType)]
  , grFreeNodeId     :: NodeId
  , grFreeEdgeId     :: EdgeId
  }

type GraphT m a = ExceptT Error (StateT GraphState m) a

emptyGraphState :: GraphState
emptyGraphState = GrSt Map.empty [] 0 0

getNode :: MonadGrLang m => Located Text -> GraphT m (GrNode, NodeType)
getNode (A loc name) = do
  existingElem <- gets (Map.lookup name . grNamedElements)
  let existingNode = getLeft =<< existingElem
  getOrError loc "node" name (pure existingNode) (nodeLocation . fst)
  where getLeft = either Just (const Nothing)

assembleGraph :: MonadGrLang m => GraphT m GrGraph
assembleGraph = do
  tgraph <- getTypeGraph
  (nodes, edges) <- gets (Map.partitionEithers . grNamedElements)
  anonEdges <- gets grAnonymousEdges
  return $ TGraph.fromNodesAndEdges tgraph
    [ (node, nodeId ntype) | (node, ntype) <- Map.elems nodes ]
    [ (edge, edgeId etype) | (edge, etype) <- Map.elems edges ++ anonEdges ]

compileGraph' :: MonadGrLang m => [GraphDeclaration] -> m (Result GrGraph)
compileGraph' = runExceptT . compileGraph

compileGraph :: MonadGrLang m => [GraphDeclaration] -> ExceptT Error m GrGraph
compileGraph decls = mapExceptT (`evalStateT` emptyGraphState) $ do
  mapMCollectErrors_ compileGraphDecl decls
  assembleGraph

compileGraphDecl :: MonadGrLang m => GraphDeclaration -> GraphT m ()
compileGraphDecl (DeclNodes nodes typeName) = do
  nodeType <- getNodeType typeName
  mapMCollectErrors_ (createNode nodeType) nodes

compileGraphDecl (DeclEdges srcName edges tgtName) = do
  (src, srcType) <- getNode srcName
  (tgt, tgtType) <- getNode tgtName
  case edges of
    SingleType edges' typeName -> do
      edgeType <- getEdgeType typeName srcType tgtType
      forMCollectErrors_ edges' $ \(A loc name) ->
        createEdge edgeType src tgt loc (Just name)
    MultipleTypes edges' ->
      forMCollectErrors_ edges' $ \(A loc (name, typeName)) -> do
        edgeType <- getEdgeType (A loc typeName) srcType tgtType
        createEdge edgeType src tgt loc name

createNode :: MonadGrLang m => NodeType -> Located Text -> GraphT m ()
createNode nodeType (A loc name) = do
  prevNode <- gets (Map.lookup name . grNamedElements)
  case prevNode of
    Just (Left (n, _)) -> reportAlreadyDefined loc "Node" name (nodeLocation n)
    Just (Right (e, _)) -> reportAlreadyDefined loc "An edge" name (edgeLocation e)
    Nothing -> modify $ \state ->
      let newId = grFreeNodeId state
          node = Node newId (Just (Metadata (Just name) loc))
      in state
        { grNamedElements = Map.insert name (Left (node, nodeType)) (grNamedElements state)
        , grFreeNodeId = newId + 1 }

createEdge :: Monad m => EdgeType -> GrNode -> GrNode -> Maybe Location -> Maybe Text -> GraphT m ()
createEdge edgeType src tgt loc (Just name) = do
  prevEdge <- gets (Map.lookup name . grNamedElements)
  case prevEdge of
    Just (Right (e,_)) -> reportAlreadyDefined loc "Edge" name (edgeLocation e)
    Just (Left (n,_)) -> reportAlreadyDefined loc "A node" name (nodeLocation n)
    Nothing -> modify $ \state ->
      let newId = grFreeEdgeId state
          edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata (Just name) loc)
      in state
        { grNamedElements = Map.insert name (Right (edge, edgeType)) (grNamedElements state)
        , grFreeEdgeId = newId + 1 }

createEdge edgeType src tgt loc Nothing = modify $ \state ->
  let newId = grFreeEdgeId state
      edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata Nothing loc)
  in state
    { grAnonymousEdges = (edge, edgeType) : grAnonymousEdges state
    , grFreeEdgeId = newId + 1 }
