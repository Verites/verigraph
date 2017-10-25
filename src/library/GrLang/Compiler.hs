{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang.Compiler
  (
    -- * Generating GrLang from graphs.
    generateTypes
  , generateGraph

    -- * Compiling GrLang into graphs.
  , compileFile
  , compile
  , compileDecl
  ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..), (<>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import           System.FilePath           ((</>), takeDirectory)
import Data.Text.Lazy.IO as Text
import           System.IO.Error           (ioeGetErrorString, tryIOError)

import           Base.Annotation           (Annotated (..), Located)
import           Base.Location
import           Data.TypedGraph           (Edge (..), EdgeId, Node (..), NodeId, TypedGraph)
import qualified Data.TypedGraph           as TGraph
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.Value

compileFile :: MonadIO m => FilePath -> GrLangT (Set FilePath) m ()
compileFile path = loadModule (A Nothing path) >>= compile

compile :: MonadIO m => [TopLevelDeclaration] -> GrLangT (Set FilePath) m ()
compile = throwingPendingErrors . mapM_ (suspendErrors . compileDecl)

compileDecl :: MonadIO m => TopLevelDeclaration -> GrLangT (Set FilePath) m ()
compileDecl (DeclNodeType n) = addNodeType n
compileDecl (DeclEdgeType e s t) = addEdgeType e s t
compileDecl (DeclGraph name graphDecls) = putValue name . VGraph =<< compileGraph graphDecls
compileDecl (Import (A loc path)) = do
  -- Interpret the imported path relative to the directory of the current path
  let path' = case loc of
        Nothing -> path
        Just (Location currPath _) -> takeDirectory currPath </> path
  alreadyImported <- gets (Set.member path')
  unless alreadyImported $
    loadModule (A loc path') >>= compile
  
loadModule :: MonadIO m => Located FilePath -> GrLangT u m [TopLevelDeclaration]
loadModule (A loc path) = do
  textOrError <- liftIO . tryIOError $ Text.readFile path
  case textOrError of
    Right text -> parseModule path text
    Left ioError -> throwError loc . PP.fillSep $
      PP.words "Error reading file" ++ [ PP.dquotes (pretty path) <> PP.colon, pretty (ioeGetErrorString ioError) ]

data GraphState = GrSt
  { grNodes          :: Map Text (Node (Maybe Metadata), NodeType)
  , grNodeId         :: NodeId
  , grEdges          :: Map Text (Edge (Maybe Metadata), EdgeType)
  , grAnonymousEdges :: [(Edge (Maybe Metadata), EdgeType)]
  , grEdgeId         :: EdgeId
  }

emptyGraphState :: GraphState
emptyGraphState = GrSt Map.empty 0 Map.empty [] 0

getNode :: Monad m => Located Text -> GrLangT GraphState m (Node (Maybe Metadata), NodeType)
getNode (A loc name) = getOrError loc "node" name $ gets (Map.lookup name . grNodes)

compileGraph :: Monad m => [GraphDeclaration] -> GrLangT inner m (TypedGraph Metadata Metadata)
compileGraph decls = throwingPendingErrors . withLocalState emptyGraphState $ do
    mapM_ (suspendErrors . compile) decls
    tgraph <- getTypeGraph
    nodes <- gets grNodes
    edges <- gets grEdges
    anonEdges <- gets grAnonymousEdges
    return $ TGraph.fromNodesAndEdges tgraph
      [ (node, nodeId ntype) | (node, ntype) <- Map.elems nodes ]
      [ (edge, edgeId etype) | (edge, etype) <- Map.elems edges ++ anonEdges ]
  where
    compile (DeclNodes nodes typeName) = do
      nodeType <- getNodeType typeName
      mapM_ (createNode nodeType) nodes

    compile (DeclEdges srcName edges tgtName) = do
      (src, srcType) <- getNode srcName
      (tgt, tgtType) <- getNode tgtName
      case edges of
        SingleType edges' typeName -> do
          edgeType <- getEdgeType typeName srcType tgtType
          forM_ edges' $ \(A loc name) ->
            createEdge edgeType src tgt loc (Just name)
        MultipleTypes edges' ->
          forM_ edges' $ \(A loc (name, typeName)) -> suspendErrors $ do
            edgeType <- getEdgeType (A loc typeName) srcType tgtType
            createEdge edgeType src tgt loc name

createNode :: Monad m => NodeType -> Located Text -> GrLangT GraphState m ()
createNode nodeType (A loc name) = do
  prevNode <- gets (Map.lookup name . grNodes)
  case prevNode of
    Just (n, _) -> registerAlreadyDefined loc "Node" name (nodeLocation n)
    Nothing -> modify $ \state ->
      let newId = grNodeId state
          node = Node newId (Just (Metadata (Just name) loc))
      in state
        { grNodes = Map.insert name (node, nodeType) (grNodes state)
        , grNodeId = newId + 1 }

createEdge :: Monad m => EdgeType -> Node (Maybe Metadata) -> Node (Maybe Metadata) -> Maybe Location -> Maybe Text -> GrLangT GraphState m ()
createEdge edgeType src tgt loc (Just name) = do
  prevEdge <- gets (Map.lookup name . grEdges)
  case prevEdge of
    Just (e, _) -> registerAlreadyDefined loc "Edge" name (edgeLocation e)
    Nothing -> modify $ \state ->
      let newId = grEdgeId state
          edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata (Just name) loc)
      in state
        { grEdges = Map.insert name (edge, edgeType) (grEdges state)
        , grEdgeId = newId + 1 }

createEdge edgeType src tgt loc Nothing = modify $ \state ->
  let newId = grEdgeId state
      edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata Nothing loc)
  in state
    { grAnonymousEdges = (edge, edgeType) : grAnonymousEdges state
    , grEdgeId = newId + 1 }


