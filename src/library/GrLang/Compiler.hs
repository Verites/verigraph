{-# LANGUAGE FlexibleContexts #-}
module GrLang.Compiler
  (
    -- * Generating GrLang from graphs.
    generateTypes
  , generateGraph

    -- * Compiling GrLang into graphs.
  , compile
  , compileFile
  , CompilerResult(..)
  , CompilerError
  , showErrors
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           System.IO            (IOMode (..), hGetContents, withFile)
import           System.IO.Error      (ioeGetErrorString, tryIOError)

import           Base.Annotation      (Annotated (..), Located, Location, reportLocation)
import qualified Data.Graphs          as TypeGraph
import           Data.TypedGraph      (Edge (..), EdgeId, Node (..), NodeId, TypedGraph)
import qualified Data.TypedGraph      as TGraph
import           GrLang.AST
import           GrLang.Graph
import           GrLang.Parser

data CompilerResult = Result
  { compiledTypeGraph :: TypeGraph
  , compiledGraphs    :: Map Text (Located (TypedGraph Metadata Metadata)) }

compileFile :: FilePath -> IO (Either [CompilerError] CompilerResult)
compileFile path = runCompilerT emptyState $
  loadFile Nothing path
  >>= withInnerState (Set.singleton path) . resolveImports
  >>= compileResolvedDecls

compile :: [TopLevelDeclaration] -> IO (Either [CompilerError] CompilerResult)
compile decls = runCompilerT emptyState $
  withInnerState Set.empty (resolveImports decls)
  >>= compileResolvedDecls

compileResolvedDecls :: Monad m => [TopLevelDeclaration] -> CompilerT Void m CompilerResult
compileResolvedDecls decls = do
  decls1 <- compileTypeGraph decls
  graphs <- compileGraphs decls1
  tgraph <- gets (typeGraph . fst)
  return $ Result tgraph graphs

type CompilerT inner m a = ExceptT [CompilerError] (StateT (CompilerState, inner) m) a
type CompilerIO inner a = CompilerT inner IO a
type CompilerError = Located String

showErrors :: [CompilerError] -> String
showErrors [] = ""
showErrors es = List.intercalate "\n\t" ("Compilation errors:" : map showError es)
  where
    showError (A loc descr) = reportLocation loc ++ descr

data CompilerState = St
  { typeGraph     :: TypeGraph
  , nodeTypes     :: Map Text NodeId
  , edgeTypes     :: Map (Text, NodeId, NodeId) EdgeId
  , pendingErrors :: [CompilerError]
  }

emptyState :: CompilerState
emptyState = St TypeGraph.empty Map.empty Map.empty []

data Void

runCompilerT :: Monad m => CompilerState -> CompilerT Void m a -> m (Either [CompilerError] a)
runCompilerT st action = evalStateT (runExceptT action) (st, undefined)

withInnerState :: Monad m => inner -> CompilerT inner m a -> CompilerT outer m a
withInnerState inner = mapExceptT changeState
  where changeState action = do
          (prevState, outer) <- gets id
          (result, (newState, _)) <- lift $ runStateT action (prevState, inner)
          put (newState, outer)
          return result

registerError :: Monad m => Maybe Location -> String -> CompilerT inner m ()
registerError src descr = modify . first $ \state ->
  state { pendingErrors = A src descr : pendingErrors state }

registerAlreadyDefined :: Monad m => Maybe Location -> String -> String -> Maybe Location -> CompilerT inner m ()
registerAlreadyDefined loc kind name prevLoc = registerError loc $
  kind ++ " '" ++ name ++ "' already defined" ++ reportLocation prevLoc

suspendErrors :: Monad m => CompilerT inner m a -> CompilerT inner m (Maybe a)
suspendErrors action =
  catchError (Just <$> action) $ \errors -> do
    modify . first $ \state -> state { pendingErrors = errors ++ pendingErrors state }
    return Nothing

throwingPendingErrors :: Monad m => CompilerT inner m a -> CompilerT inner m a
throwingPendingErrors action = do
  prevErrors <- gets (pendingErrors . fst)
  modify . first $ \state -> state { pendingErrors = [] }
  result <- action
  newErrors <- gets (pendingErrors . fst)
  modify . first $ \state -> state { pendingErrors = prevErrors }
  if null newErrors
    then return result
    else throwError newErrors

throwSingleError :: Monad m => Maybe Location -> String -> CompilerT inner m a
throwSingleError src descr = throwError [A src descr]

lookupNodeType :: Monad m => Text -> CompilerT inner m (Maybe NodeType)
lookupNodeType name = do
  tgraph <- gets (typeGraph . fst)
  ntypes <- gets (nodeTypes . fst)
  return $ (`TypeGraph.lookupNode` tgraph) =<< Map.lookup name ntypes

getOrError :: Monad m => Maybe Location -> String -> String -> CompilerT inner m (Maybe a) -> CompilerT inner m a
getOrError loc kind name getter = do
  result <- getter
  case result of
    Just x -> return x
    Nothing -> throwSingleError loc $
      "Undefined " ++ kind ++ " '" ++ name ++ "'"

getNodeType :: Monad m => Located Text -> CompilerT inner m NodeType
getNodeType (A loc name) =
  getOrError loc "node type" (Text.unpack name) (lookupNodeType name)

lookupEdgeType :: Monad m => Text -> NodeType -> NodeType -> CompilerT inner m (Maybe EdgeType)
lookupEdgeType name (Node srcId _) (Node tgtId _) = do
  tgraph <- gets (typeGraph . fst)
  etypes <- gets (edgeTypes . fst)
  return $ (`TypeGraph.lookupEdge` tgraph) =<< Map.lookup (name, srcId, tgtId) etypes

getEdgeType :: Monad m => Located Text -> NodeType -> NodeType -> CompilerT inner m EdgeType
getEdgeType (A loc name) srcType tgtType =
  getOrError loc "edge type" (showEdgeType name srcType tgtType) (lookupEdgeType name srcType tgtType)

loadFile :: Maybe Location -> FilePath -> CompilerIO inner [TopLevelDeclaration]
loadFile loc path = do
  result <- liftIO . tryIOError . withFile path ReadMode $ \file ->
    parseTopLevel path <$> hGetContents file
  case result of
    Right (Right decls) -> return decls
    Right (Left parseError) -> throwSingleError loc (show parseError)
    Left ioError -> throwSingleError loc (ioeGetErrorString ioError)

resolveImports :: [TopLevelDeclaration] -> CompilerIO (Set FilePath) [TopLevelDeclaration]
resolveImports decls = concat <$> mapM resolveImport decls
  where
    resolveImport (Import (A loc file)) = do
      alreadyImported <- Set.member file <$> gets snd
      if alreadyImported
        then return []
        else loadFile loc file >>= resolveImports
    resolveImport decl = return [decl]

compileTypeGraph :: Monad m => [TopLevelDeclaration] -> CompilerT inner m [TopLevelDeclaration]
compileTypeGraph decls = throwingPendingErrors (compile decls)
  where
    compile []                           = return []
    compile (DeclNodeType n : decls)     = createNodeType n >> compile decls
    compile (DeclEdgeType e s t : decls) = createEdgeType e s t >> compile decls
    compile (d : decls)                  = (d:) <$> compile decls

createNodeType :: Monad m => Located Text -> CompilerT inner m ()
createNodeType (A loc name) = do
  existingType <- lookupNodeType name
  case existingType of
    Just n ->
      registerAlreadyDefined loc "Node type" (Text.unpack name) (nodeLocation n)
    Nothing -> modify . first $ \state ->
      let newId:_ = TypeGraph.newNodes (typeGraph state)
          metadata = Metadata (Just name) loc
      in state
        { typeGraph = TypeGraph.insertNodeWithPayload newId (Just metadata) (typeGraph state)
        , nodeTypes = Map.insert name newId (nodeTypes state) }

createEdgeType :: Monad m => Located Text -> Located Text -> Located Text -> CompilerT inner m ()
createEdgeType (A loc name) srcName tgtName = do
  srcType <- getNodeType srcName
  tgtType <- getNodeType tgtName
  existingType <- lookupEdgeType name srcType tgtType
  case existingType of
    Just e ->
      registerAlreadyDefined loc "Edge type" (showEdgeType name srcType tgtType) (edgeLocation e)
    Nothing -> modify . first $ \state ->
      let newId:_ = TypeGraph.newEdges (typeGraph state)
          metadata = Metadata (Just name) loc
          (Node srcId _, Node tgtId _) = (srcType, tgtType)
      in state
        { typeGraph = TypeGraph.insertEdgeWithPayload newId srcId tgtId (Just metadata) (typeGraph state)
        , edgeTypes = Map.insert (name, srcId, tgtId) newId (edgeTypes state) }

compileGraphs :: Monad m => [TopLevelDeclaration] -> CompilerT inner m (Map Text (Located (TypedGraph Metadata Metadata)))
compileGraphs = compile Map.empty
  where
    compile graphs [] = return graphs
    compile graphs (DeclGraph (A loc name) graphDecls : decls) =
      case Map.lookup name graphs of
        Just (A prevLoc _) -> do
          registerAlreadyDefined loc "Graph" (Text.unpack name) prevLoc
          compile graphs decls
        Nothing -> do
          result <- suspendErrors (compileGraph graphDecls)
          case result of
            Just graph -> compile (Map.insert name (A loc graph) graphs) decls
            Nothing -> compile graphs decls
    compile graphs (_:decls) = compile graphs decls

data GraphState = GrSt
  { grNodes          :: Map Text (Node (Maybe Metadata), NodeType)
  , grNodeId         :: NodeId
  , grEdges          :: Map Text (Edge (Maybe Metadata), EdgeType)
  , grAnonymousEdges :: [(Edge (Maybe Metadata), EdgeType)]
  , grEdgeId         :: EdgeId
  }

emptyGraphState :: GraphState
emptyGraphState = GrSt Map.empty 0 Map.empty [] 0

lookupNode :: Monad m => Text -> CompilerT GraphState m (Maybe (Node (Maybe Metadata), NodeType))
lookupNode name = gets (Map.lookup name . grNodes . snd)

getNode :: Monad m => Located Text -> CompilerT GraphState m (Node (Maybe Metadata), NodeType)
getNode (A loc name) = getOrError loc "node" (Text.unpack name) (lookupNode name)

lookupEdge :: Monad m => Text -> CompilerT GraphState m (Maybe (Edge (Maybe Metadata), EdgeType))
lookupEdge name = gets (Map.lookup name . grEdges . snd)

compileGraph :: Monad m => [GraphDeclaration] -> CompilerT inner m (TypedGraph Metadata Metadata)
compileGraph decls = throwingPendingErrors . withInnerState emptyGraphState $ do
    mapM_ (suspendErrors . compile) decls
    GrSt nodes _ edges anonEdges _ <- gets snd
    tgraph <- gets (typeGraph . fst)
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

createNode :: Monad m => NodeType -> Located Text -> CompilerT GraphState m ()
createNode nodeType (A loc name) = do
  prevNode <- lookupNode name
  case prevNode of
    Just (n, _) ->
      registerAlreadyDefined loc "Node" (Text.unpack name) (nodeLocation n)
    Nothing -> modify . second $ \state ->
      let newId = grNodeId state
          node = Node newId (Just (Metadata (Just name) loc))
      in state
        { grNodes = Map.insert name (node, nodeType) (grNodes state)
        , grNodeId = newId + 1 }

createEdge :: Monad m => EdgeType -> Node (Maybe Metadata) -> Node (Maybe Metadata) -> Maybe Location -> Maybe Text -> CompilerT GraphState m ()
createEdge edgeType src tgt loc (Just name) = do
  prevEdge <- lookupEdge name
  case prevEdge of
    Just (e, _) ->
      registerAlreadyDefined loc "Edge" (Text.unpack name) (edgeLocation e)
    Nothing -> modify . second $ \state ->
      let newId = grEdgeId state
          edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata (Just name) loc)
      in state
        { grEdges = Map.insert name (edge, edgeType) (grEdges state)
        , grEdgeId = newId + 1 }
createEdge edgeType src tgt loc Nothing = modify . second $ \state ->
  let newId = grEdgeId state
      edge = Edge newId (nodeId src) (nodeId tgt) (Just $ Metadata Nothing loc)
  in state
    { grAnonymousEdges = (edge, edgeType) : grAnonymousEdges state
    , grEdgeId = newId + 1 }


showEdgeType :: Text -> NodeType -> NodeType -> String
showEdgeType name srcType tgtType =
  Text.unpack name ++ ": " ++ nameOf srcType ++ " -> " ++ nameOf tgtType
  where
    nameOf (Node n Nothing)                         = '?' : show n
    nameOf (Node n (Just (Metadata Nothing _)))     = '?' : show n
    nameOf (Node _ (Just (Metadata (Just name) _))) = Text.unpack name

