{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang.Compiler
  ( compileFile
  , compile
  , compileDecl
  , compileGraph
  , compileRule
  ) where

import           Control.Monad.Except           (ExceptT (..), mapExceptT)
import           Control.Monad.State
import           Data.Either                    (lefts, rights)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes)
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
import qualified Data.TypedGraph.Morphism       as TGraph
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.Value
import           Rewriting.DPO.TypedGraph       (Production (..))
import qualified Util.Map                       as Map
import           Util.Monad

compileFile :: (MonadIO m, MonadGrLang m) => FilePath -> ExceptT Error m ()
compileFile path = importIfNeeded_ path $
  loadModule (A Nothing path) >>= compile

compile :: (MonadIO m, MonadGrLang m) => [TopLevelDeclaration] -> ExceptT Error m ()
compile = mapMCollectErrors_ compileDecl

compileDecl :: (MonadIO m, MonadGrLang m) => TopLevelDeclaration -> ExceptT Error m ()
compileDecl (DeclNodeType n) = addNodeType n
compileDecl (DeclEdgeType e s t) = addEdgeType e s t
compileDecl (DeclGraph name graphDecls) =
  putValue name . VGraph =<< compileGraph graphDecls
compileDecl (DeclRule name ruleDecls) =
  putValue name . VRule =<< compileRule ruleDecls
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

grAllEdges :: GraphState -> [(GrEdge, EdgeType)]
grAllEdges state = (rights . Map.elems) (grNamedElements state) ++ grAnonymousEdges state

type GraphT m a = ExceptT Error (StateT GraphState m) a

evalGraphT :: MonadGrLang m => GraphState -> GraphT m a -> ExceptT Error m a
evalGraphT state = mapExceptT (`evalStateT` state)

emptyGraphState :: GraphState
emptyGraphState = GrSt Map.empty [] 0 0

getNode :: MonadGrLang m => Located Text -> GraphT m (GrNode, NodeType)
getNode (A loc name) = do
  existingElem <- gets (Map.lookup name . grNamedElements)
  let existingNode = getLeft =<< existingElem
  getOrError loc "node" name (pure existingNode) (nodeLocation . fst)
  where getLeft = either Just (const Nothing)

getElem :: MonadGrLang m => Located Text -> GraphT m GrElem
getElem (A loc name) = do
  existingElem <- gets (Map.lookup name . grNamedElements)
  getOrError loc "node or edge" name (pure existingElem) (either (nodeLocation . fst) (edgeLocation . fst))

assembleGraph :: MonadGrLang m => GraphT m GrGraph
assembleGraph = do
  tgraph <- getTypeGraph
  (nodes, edges) <- gets (Map.partitionEithers . grNamedElements)
  anonEdges <- gets grAnonymousEdges
  return $ TGraph.fromNodesAndEdges tgraph
    [ (node, nodeId ntype) | (node, ntype) <- Map.elems nodes ]
    [ (edge, edgeId etype) | (edge, etype) <- Map.elems edges ++ anonEdges ]

compileGraph :: MonadGrLang m => [GraphDeclaration] -> ExceptT Error m GrGraph
compileGraph = mapExceptT (`evalStateT` emptyGraphState) . compileGraph'

compileGraph' :: MonadGrLang m => [GraphDeclaration] -> GraphT m GrGraph
compileGraph' decls = mapMCollectErrors compileGraphDecl decls >> assembleGraph

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
        createEdge edgeType (nodeId src) (nodeId tgt) loc (Just name)
    MultipleTypes edges' ->
      forMCollectErrors_ edges' $ \(A loc (name, typeName)) -> do
        edgeType <- getEdgeType (A loc typeName) srcType tgtType
        createEdge edgeType (nodeId src) (nodeId tgt) loc name

createNode :: MonadGrLang m => NodeType -> Located Text -> GraphT m NodeId
createNode nodeType (A loc name) = do
  prevNode <- gets (Map.lookup name . grNamedElements)
  case prevNode of
    Just (Left (n, _)) -> reportAlreadyDefined loc "Node" name (nodeLocation n)
    Just (Right (e, _)) -> reportAlreadyDefined loc "An edge" name (edgeLocation e)
    Nothing -> do
      newId <- gets grFreeNodeId
      let node = Node newId (Just (Metadata (Just name) loc))
      modify $ \state -> state
        { grNamedElements = Map.insert name (Left (node, nodeType)) (grNamedElements state)
        , grFreeNodeId = newId + 1 }
      return newId

createEdge :: Monad m => EdgeType -> NodeId -> NodeId -> Maybe Location -> Maybe Text -> GraphT m EdgeId
createEdge edgeType src tgt loc (Just name) = do
  prevEdge <- gets (Map.lookup name . grNamedElements)
  case prevEdge of
    Just (Right (e,_)) -> reportAlreadyDefined loc "Edge" name (edgeLocation e)
    Just (Left (n,_)) -> reportAlreadyDefined loc "A node" name (nodeLocation n)
    Nothing -> do
      newId <- gets grFreeEdgeId
      let edge = Edge newId src tgt (Just $ Metadata (Just name) loc)
      modify $ \state -> state
        { grNamedElements = Map.insert name (Right (edge, edgeType)) (grNamedElements state)
        , grFreeEdgeId = newId + 1 }
      return newId

createEdge edgeType src tgt loc Nothing = do
  newId <- gets grFreeEdgeId
  let edge = Edge newId src tgt (Just $ Metadata Nothing loc)
  modify $ \state -> state
    { grAnonymousEdges = (edge, edgeType) : grAnonymousEdges state
    , grFreeEdgeId = newId + 1 }
  return newId


compileRule :: MonadGrLang m => [RuleDeclaration] -> ExceptT Error m GrRule
compileRule decls = do
  -- Compile the LHS graph from match declarations
  lhsState <- evalGraphT emptyGraphState $ mapMCollectErrors_ compileMatch decls >> get
  lhsGraph <- evalGraphT lhsState assembleGraph

  (lhsMappings, interfaceState) <- evalGraphT lhsState $ do
    mapMCollectErrors_ compileDelete decls
    preservedMapping <- gets $ \state ->
      let reflexive (Left (n, _))  = Left (nodeId n, nodeId n)
          reflexive (Right (e, _)) = Right (edgeId e, edgeId e)
      in map reflexive (Map.elems $ grNamedElements state) ++ map (reflexive . Right) (grAnonymousEdges state)
    clonedMapping <- concat <$> mapMCollectErrors compileClone decls
    (,) (preservedMapping ++ clonedMapping) <$> get
  interfaceGraph <- evalGraphT interfaceState assembleGraph
  let lhsMorph = TGraph.fromGraphsAndLists interfaceGraph lhsGraph (lefts lhsMappings) (rights lhsMappings)

  -- Add created elements to the interface graph
  rhsGraph <- evalGraphT interfaceState $ mapMCollectErrors_ compileCreate decls >> assembleGraph
  let rhsMorph = TGraph.makeInclusion interfaceGraph rhsGraph

  theNacs <- catMaybes <$> mapMCollectErrors (compileForbid lhsGraph lhsState) decls

  return (Production lhsMorph rhsMorph theNacs)
  where
    compileMatch (DeclMatch decls) = mapMCollectErrors_ compileGraphDecl decls
    compileMatch _                 = return ()

    -- Given a clone declaration, add the appropriate duplicate elements to the state
    -- and return a pair @(originalId,clonedIdsList)@.
    compileClone (DeclClone name clones) = do
      elem <- getElem name
      case elem of
        Left (node, typ) ->
          forMCollectErrors clones $ \cloneName -> do
          cloneId <- createNode typ cloneName
          return $ Left (cloneId, nodeId node)
        Right (edge, typ) ->
          forMCollectErrors clones $ \(A loc cloneName) -> do
          cloneId <- createEdge typ (sourceId edge) (targetId edge) loc (Just cloneName)
          return $ Right (cloneId, edgeId edge)
    compileClone _ = return []

    compileDelete (DeclDelete deleted) = forMCollectErrors_ deleted $ \(A loc name, mode) -> do
      elem <- getElem (A loc name)
      case elem of
        Left (node, _) -> do
          let isIncident (edge, _) = sourceId edge == nodeId node || targetId edge == nodeId node
          case mode of
            Isolated -> do
              incidentEdges <- gets (filter isIncident . grAllEdges)
              unless (null incidentEdges) . throwError loc . PP.fillSep $
                  PP.words "Cannot delete node" ++ pretty name : PP.words "without deleting its incident edges: "
                  ++ PP.punctuate ", " (map (pretty . edgeName . fst) incidentEdges)
            WithMatchedEdges ->
              modify $ \state -> state
                { grNamedElements = Map.filter (either (const True) (not . isIncident)) (grNamedElements state)
                , grAnonymousEdges = filter (not . isIncident) (grAnonymousEdges state)
                }
        Right _ -> return ()
      modify $ \state -> state
        { grNamedElements = Map.delete name (grNamedElements state) }
    compileDelete _ = return ()

    compileCreate (DeclCreate decls) = mapMCollectErrors_ compileGraphDecl decls
    compileCreate _                  = return ()

    compileForbid lhsGraph lhsState (DeclForbid _ decls) = do
      nacGraph <- evalGraphT lhsState (compileGraph' decls)
      return . Just $ TGraph.makeInclusion lhsGraph nacGraph
    compileForbid _ _ _ = return Nothing
