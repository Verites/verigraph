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
import           Data.Function        (on)
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe, isJust, mapMaybe)
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
import           GrLang.Metadata
import           GrLang.Parser

import           Debug.Trace

generateTypes :: TypeGraph -> [TopLevelDeclaration]
generateTypes tgraph = nodeTypes ++ edgeTypes
  where
    nodeTypes =
      [ DeclNodeType (A Nothing $ nodeName n) | n <- TypeGraph.nodes tgraph ]
    edgeTypes =
      [ DeclEdgeType (A Nothing $ edgeName e) (A Nothing $ nodeName src) (A Nothing $ nodeName tgt)
          | ((src, _), e, (tgt, _)) <- TypeGraph.edgesInContext tgraph ]

generateGraph :: Text -> TypedGraph Metadata Metadata -> TopLevelDeclaration
generateGraph graphName graph = DeclGraph (A Nothing graphName) (nodes ++ edges)
  where
    (minElemsPerDecl, maxElemsPerDecl) = (3, 5)

    nodes = concatMap (mapMaybe generateNodes . chunksOf maxElemsPerDecl) nodesByType
    nodesByType = chunksBy (\(_,t,_) -> nodeId t) $ TGraph.nodesInContext graph
    generateNodes [] = Nothing
    generateNodes ns@((_, ntype, _):_) =
      Just $ DeclNodes [ A Nothing (nodeName n) | (n, _, _) <- ns ] (A Nothing $ nodeName ntype)

    edges = concatMap generateEdges . chunksBy sourceTarget $ TGraph.edgesInContext graph
      where sourceTarget ((s,_,_),_,_,(t,_,_)) = (nodeId s, nodeId t)
    generateEdges es =
      let
        (namedEdges, anonEdges) = List.partition (\(_,Edge _ _ _ metadata,_,_) -> isJust (name =<< metadata)) es
        namedEdgesByType = chunksBy edgeType namedEdges
        (namedEdgesByType', smallChunks) = List.partition (\l -> length l > minElemsPerDecl) namedEdgesByType
        mixedEdges = List.sortBy (compare `on` edgeType) (concat smallChunks ++ anonEdges)
      in
        concatMap (mapMaybe generateSingleType . chunksOf maxElemsPerDecl) namedEdgesByType'
        ++ (mapMaybe generateMultipleTypes . chunksOf maxElemsPerDecl) mixedEdges
      where edgeType (_,_,t,_) = edgeId t
    generateSingleType = generateEdgeDecl $ \es etype ->
      SingleType [A Nothing (edgeName e) | (_,e,_,_) <- es] (A Nothing $ edgeName etype)
    generateMultipleTypes = generateEdgeDecl $ \es _ ->
      MultipleTypes [A Nothing (maybeEdgeName e, edgeName t) | (_,e,t,_) <- es]
      where maybeEdgeName (Edge _ _ _ metadata) = name =<< metadata
    generateEdgeDecl _ [] = Nothing
    generateEdgeDecl makeEdges es@(e:_) =
      let ((s,_,_),_,etype,(t,_,_)) = e
      in Just $ DeclEdges (A Nothing $ nodeName s) (makeEdges es etype) (A Nothing $ nodeName t)

chunksBy :: Ord b => (a -> b) -> [a] -> [[a]]
chunksBy proj = List.groupBy ((==) `on` proj) . List.sortBy (compare `on` proj)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l =
  let (chunk, rest) = splitAt n l
  in chunk : chunksOf n rest

nodeName :: Node (Maybe Metadata) -> Text
nodeName (Node n metadata) = nameOrId n metadata

edgeName :: Edge (Maybe Metadata) -> Text
edgeName (Edge e _ _ metadata) = nameOrId e metadata

nameOrId :: Show id => id -> Maybe Metadata -> Text
nameOrId id metadata = fromMaybe (Text.pack $ '?' : show id) (name =<< metadata)

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
    Just (Node _ metadata) ->
      registerAlreadyDefined loc "Node type" (Text.unpack name) (sourcePos =<< metadata)
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
    Just (Edge _ _ _ metadata) ->
      registerAlreadyDefined loc "Edge type" (showEdgeType name srcType tgtType) (sourcePos =<< metadata)
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
    Just (Node _ metadata, _) ->
      registerAlreadyDefined loc "Node" (Text.unpack name) (sourcePos =<< metadata)
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
    Just (Edge _ _ _ metadata, _) ->
      registerAlreadyDefined loc "Edge" (Text.unpack name) (sourcePos =<< metadata)
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

