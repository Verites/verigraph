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
  ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc as PP
import           System.IO                 (IOMode (..), hGetContents, withFile)
import           System.IO.Error           (ioeGetErrorString, tryIOError)
import qualified Text.Parsec.Error         as Parsec
import qualified Text.Parsec.Pos           as Parsec

import           Base.Annotation           (Annotated (..), Located)
import           Base.Location
import           Data.TypedGraph           (Edge (..), EdgeId, Node (..), NodeId, TypedGraph)
import qualified Data.TypedGraph           as TGraph
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.Value

data CompilerResult = Result
  { compiledTypeGraph :: TypeGraph
  , compiledValues    :: Map Text (Located Value) }

compileFile :: FilePath -> IO (Either [Error] CompilerResult)
compileFile path = runGrLangT emptyState $
  loadFile Nothing path
  >>= withLocalState (Set.singleton path) . resolveImports
  >>= compileResolvedDecls

compile :: [TopLevelDeclaration] -> IO (Either [Error] CompilerResult)
compile decls = runGrLangT emptyState $
  withLocalState Set.empty (resolveImports decls)
  >>= compileResolvedDecls

compileResolvedDecls :: Monad m => [TopLevelDeclaration] -> GrLangT u m CompilerResult
compileResolvedDecls decls = do
  decls1 <- compileTypeGraph decls
  mapM_ (suspendErrors . compileTopLevelDecl) decls1
  Result <$> getTypeGraph <*> getValueContext

loadFile :: MonadIO m => Maybe Location -> FilePath -> GrLangT u m [TopLevelDeclaration]
loadFile loc path = do
  result <- liftIO . tryIOError . withFile path ReadMode $ \file ->
    parseTopLevel path <$> hGetContents file
  case result of
    Right (Right decls) -> return decls
    Right (Left parseError) -> throwErrors (Just $ locationFromParsec (Parsec.errorPos parseError))
      [ reflow (Parsec.messageString msg) | msg <- Parsec.errorMessages parseError ]
    Left ioError -> throwError loc (reflow $ ioeGetErrorString ioError)

locationFromParsec :: Parsec.SourcePos -> Location
locationFromParsec pos =
  Location (Parsec.sourceName pos) $ Position (Parsec.sourceLine pos) (Parsec.sourceColumn pos)

reflow :: String -> Doc ann
reflow = PP.fillSep . map pretty . words

resolveImports :: MonadIO m => [TopLevelDeclaration] -> GrLangT (Set FilePath) m [TopLevelDeclaration]
resolveImports decls = concat <$> mapM resolveImport decls
  where
    resolveImport (Import (A loc file)) = do
      alreadyImported <- gets (Set.member file)
      if alreadyImported
        then return []
        else loadFile loc file >>= resolveImports
    resolveImport decl = return [decl]

compileTypeGraph :: Monad m => [TopLevelDeclaration] -> GrLangT u m [TopLevelDeclaration]
compileTypeGraph decls = throwingPendingErrors (compile decls)
  where
    compile []                           = return []
    compile (DeclNodeType n : decls)     = suspendErrors (addNodeType n) >> compile decls
    compile (DeclEdgeType e s t : decls) = suspendErrors (addEdgeType e s t) >> compile decls
    compile (d : decls)                  = (d:) <$> compile decls

compileTopLevelDecl :: Monad m => TopLevelDeclaration -> GrLangT u m ()
compileTopLevelDecl (DeclNodeType n) = addNodeType n
compileTopLevelDecl (DeclEdgeType e s t) = addEdgeType e s t
compileTopLevelDecl (DeclGraph name graphDecls) = putValue name . VGraph =<< compileGraph graphDecls


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


