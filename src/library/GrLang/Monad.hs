{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module GrLang.Monad
  ( MonadGrLang(..)
  , ExceptT
  , importIfNeeded_
  , makeVisible
  , Result
  , Error
  , prettyError
  , throwError
  , getOrError
  , addNew
  , reportAlreadyDefined
  , GrLangT
  , GrLangState(..)
  , emptyState
  , initState
  , runGrLangT
  , evalGrLangT
  , evalGrLang
  , getValueContext
  ) where

import           Control.Monad.Except           (ExceptT (..), mapExceptT, runExceptT)
import qualified Control.Monad.Except           as ExceptT
import           Control.Monad.State
import           Data.DList                     (DList)
import qualified Data.DList                     as DList
import           Data.Functor.Identity
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Data.Text.Prettyprint.Doc      (Doc, Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc      as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP

import           Base.Annotation                (Annotated (..), Located, locatedDoc, locationOf)
import qualified Base.Annotation                as Ann
import           Base.Location
import qualified Data.Graphs                    as TypeGraph
import           Data.TypedGraph                (Edge (..), EdgeId, Node (..), NodeId)
import           GrLang.Value

type Error = DList (Located (Doc ()))
type Result a = Either Error a

prettyError :: Error -> Doc ()
prettyError = PP.vsep . map (PP.hang 2 . locatedDoc) . DList.toList

throwError :: Monad m => Maybe Location -> Doc () -> ExceptT Error m a
throwError loc msg = ExceptT.throwError $ DList.singleton (A loc msg)

class Monad m => MonadGrLang m where

  -- | Given the path to a module and an action that imports it, executes the
  -- action unless the module was already imported. Makes the imported module
  -- visible, as well as its transitive imports.
  --
  -- When running the import action, the only visible module will be the
  -- one being imported.
  importIfNeeded :: FilePath -> ExceptT Error m a -> ExceptT Error m (Maybe a)

  -- | Given the path to a module, check if it is was already imported.
  checkIfImported :: FilePath -> ExceptT Error m Bool

  -- | Given the path to a module, check if it is currently visible.
  checkIfVisible :: FilePath -> ExceptT Error m Bool

  -- | Given the path to a module, mark it as visible, _even when the path was not yet
  -- imported_.
  unsafeMakeVisible :: FilePath -> m ()

  -- | Obtain the current type graph
  getTypeGraph :: ExceptT Error m TypeGraph

  -- | Look up the node type that has the given name
  getNodeType :: Located Text -> ExceptT Error m NodeType

  -- | Look up the edge type that has the given name, source type and id type.
  getEdgeType :: Located Text -> NodeType -> NodeType -> ExceptT Error m EdgeType

  -- | Create a new node type with given name, returning its id. If a node type
  -- with the same name already exists, returns an error.
  addNodeType :: Located Text -> ExceptT Error m ()

  -- | Create a new edge type with given name, source type and target type,
  -- returning its id. If an edge type with the same name, source and target
  -- already exists, returns an error.
  addEdgeType :: Located Text -> Located Text -> Located Text -> ExceptT Error m ()

  -- | Obtain the value associated to the given name, or return an error if
  -- there is none.
  getValue :: Located Text -> ExceptT Error m Value

  -- | Bind the given value to the given name. If the name is already bound,
  -- return an error.
  putValue :: Located Text -> Value -> ExceptT Error m ()

instance MonadGrLang m => MonadGrLang (StateT s m) where
    importIfNeeded path action = do
      st <- get
      result <- lift' . importIfNeeded path $ do
        result <- lift $ runStateT (runExceptT action) st
        case result of
          (Left errs, _) -> ExceptT.throwError errs
          (Right x, st') -> return (x, st')
      case result of
        Nothing -> return Nothing
        Just (x, st') -> put st' >> return (Just x)

    checkIfImported = lift' . checkIfImported
    checkIfVisible = lift' . checkIfVisible
    unsafeMakeVisible = lift . unsafeMakeVisible
    getTypeGraph = lift' getTypeGraph
    getNodeType = lift' . getNodeType
    getEdgeType name src = lift' . getEdgeType name src
    addNodeType = lift' . addNodeType
    addEdgeType name src = lift' . addEdgeType name src
    getValue = lift' . getValue
    putValue name = lift' . putValue name

lift' :: Monad m => ExceptT Error m a -> ExceptT Error (StateT s m) a
lift' = mapExceptT lift

-- | Given the path to a module and an action that imports it, executes the
-- action unless the module was already imported. Makes the imported module
-- visible, as well as its transitive imports.
--
-- When running the import action, the only visible module will be the
-- one being imported.
importIfNeeded_ :: MonadGrLang m => FilePath -> ExceptT Error m a -> ExceptT Error m ()
importIfNeeded_ path action = void $ importIfNeeded path action

-- | Given the path to a module, mark it as visible. Fails when the path was not yet
-- imported.
makeVisible :: MonadGrLang m => Located FilePath -> ExceptT Error m ()
makeVisible (A loc srcPath) = do
  isImported <- checkIfImported srcPath
  if isImported
    then lift $ unsafeMakeVisible srcPath
    else throwError loc . PP.fillSep $
      ["Cannot", "make", "unimported", "module", PP.squotes (pretty srcPath), "visible"]


-- | A monad transformer for compiling or interpreting the GrLang.
-- It allows the user to store local state of type @u@.
newtype GrLangT m a =
  GrLangT { unGrLangT :: StateT GrLangState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

data GrLangState = GrLangState
  { typeGraph       :: TypeGraph
  , nodeTypes       :: Map Text NodeId
  , edgeTypes       :: Map (Text, NodeId, NodeId) EdgeId
  , valueContext    :: Map Text (Located Value)
  , importedModules :: Set FilePath
  , visibleModules  :: Set FilePath
  }

emptyState :: GrLangState
emptyState = GrLangState
  { typeGraph = TypeGraph.empty
  , nodeTypes = Map.empty
  , edgeTypes = Map.empty
  , valueContext = Map.empty
  , importedModules = Set.empty
  , visibleModules = Set.empty
  }

initState :: TypeGraph -> GrLangState
initState graph = emptyState
  { typeGraph = graph
  , nodeTypes = Map.fromList [ (nodeName n, nodeId n) | n <- TypeGraph.nodes graph ]
  , edgeTypes = Map.fromList [ ((edgeName e, sourceId e, targetId e), edgeId e) | e <- TypeGraph.edges graph ]
  }

-- | Run a GrLang computation with the given initial state.
runGrLangT :: Monad m => GrLangState -> GrLangT m a -> m (a, GrLangState)
runGrLangT st (GrLangT action) = runStateT action st

evalGrLangT :: Monad m => GrLangState -> ExceptT Error (GrLangT m) a -> m (Either Error a)
evalGrLangT st action = evalStateT (unGrLangT $ runExceptT action) st

evalGrLang :: GrLangState -> ExceptT Error (GrLangT Identity) a -> Either Error a
evalGrLang st action = runIdentity $ evalGrLangT st action

instance MonadIO m => MonadIO (GrLangT m) where
  liftIO = lift . liftIO

instance Monad m => MonadGrLang (GrLangT m) where

  importIfNeeded path importAction = do
    isImported <- lift . GrLangT $ gets (Set.member path . importedModules)
    if isImported
      then do
        lift . GrLangT . modify $ \state ->
          state { visibleModules = Set.insert path (visibleModules state) }
        return Nothing
      else do
        outerVisible <- lift . GrLangT $ gets visibleModules
        lift . GrLangT . modify $ \state ->
          state
            { importedModules = Set.insert path (importedModules state)
            , visibleModules = Set.singleton path }
        result <- importAction
        lift . GrLangT . modify $ \state -> state { visibleModules = Set.union outerVisible (visibleModules state) }
        return (Just result)

  checkIfImported srcPath = lift . GrLangT $ gets (Set.member srcPath . importedModules)

  checkIfVisible srcPath = lift . GrLangT $ gets (Set.member srcPath . visibleModules)

  unsafeMakeVisible srcPath = GrLangT . modify $ \state ->
    state { visibleModules = Set.insert srcPath (visibleModules state) }

  getTypeGraph = lift . GrLangT $ gets typeGraph

  getNodeType (A loc name) = getOrError loc "node type" name (lookupNodeType name) nodeLocation

  getEdgeType (A loc name) srcType tgtType =
    getOrError loc "edge type" (showEdgeType name srcType tgtType) (lookupEdgeType name srcType tgtType) edgeLocation

  addNodeType (A loc name) = do
    existingType <- lift $ lookupNodeType name
    addNew' loc "Node type" name (nodeLocation <$> existingType) $ \state ->
      let newId:_ = TypeGraph.newNodes (typeGraph state)
          metadata = Metadata (Just name) loc
      in state
        { typeGraph = TypeGraph.insertNodeWithPayload newId (Just metadata) (typeGraph state)
        , nodeTypes = Map.insert name newId (nodeTypes state) }

  addEdgeType (A loc name) srcName tgtName = do
    srcType <- getNodeType srcName
    tgtType <- getNodeType tgtName
    existingType <- lift $ lookupEdgeType name srcType tgtType
    addNew' loc "Edge type" (showEdgeType name srcType tgtType) (edgeLocation <$> existingType) $ \state ->
      let newId:_ = TypeGraph.newEdges (typeGraph state)
          metadata = Metadata (Just name) loc
          (Node srcId _, Node tgtId _) = (srcType, tgtType)
      in state
        { typeGraph = TypeGraph.insertEdgeWithPayload newId srcId tgtId (Just metadata) (typeGraph state)
        , edgeTypes = Map.insert (name, srcId, tgtId) newId (edgeTypes state) }

  getValue (A loc name) = Ann.drop <$>
    getOrError loc "value" name (Map.lookup name <$> getValueContext) locationOf
    where getValueContext = GrLangT $ gets valueContext

  putValue (A loc name) val = do
    prevVal <- lift . GrLangT $ gets (Map.lookup name . valueContext)
    addNew' loc "Value" name (locationOf <$> prevVal) $ \state ->
      state { valueContext = Map.insert name (A loc val) (valueContext state) }

lookupNodeType :: Monad m => Text -> GrLangT m (Maybe NodeType)
lookupNodeType name = GrLangT $ do
  tgraph <- gets typeGraph
  ntypes <- gets nodeTypes
  return $ (`TypeGraph.lookupNode` tgraph) =<< Map.lookup name ntypes

lookupEdgeType :: Monad m => Text -> NodeType -> NodeType -> GrLangT m (Maybe EdgeType)
lookupEdgeType name srcType tgtType = GrLangT $ do
  tgraph <- gets typeGraph
  etypes <- gets edgeTypes
  return $ (`TypeGraph.lookupEdge` tgraph)
    =<< Map.lookup (name, nodeId srcType, nodeId tgtType) etypes

getValueContext :: Monad m => ExceptT Error (GrLangT m) (Map Text (Located Value))
getValueContext = lift . GrLangT $ gets valueContext

addNew' :: Monad m => Maybe Location -> String -> Text -> Maybe (Maybe Location) -> (GrLangState -> GrLangState) -> ExceptT Error (GrLangT m) ()
addNew' loc kind name existingLocation addToState =
  addNew loc kind name existingLocation . lift . GrLangT . modify $ \state ->
    let state' = addToState state
        tgraph = typeGraph state'
    in state' { valueContext = fmap (fmap $ updateTypeGraph tgraph) (valueContext state') }

addNew :: Monad m => Maybe Location -> String -> Text -> Maybe (Maybe Location) -> ExceptT Error m a -> ExceptT Error m ()
addNew loc kind name existingLocation addToState =
  case existingLocation of
    Just loc' -> reportAlreadyDefined loc kind name loc'
    Nothing -> do
      _ <- addToState
      return ()

-- | Try to lookup a named element, throwing an error if there is none.
--
-- Receives, in this order: the location which caused the lookup, a string
-- describing the kind of value (e.g. "node or edge"), the name being looked up,
-- an action that looks up the name, and a function that extracts locations from
-- values of the kind being looked up.
getOrError :: MonadGrLang m => Maybe Location -> String -> Text -> m (Maybe a) -> (a -> Maybe Location) -> ExceptT Error m a
getOrError loc kind name getter getLocation = do
  result <- lift getter
  let undefError = PP.fillSep ["Undefined", pretty kind, PP.squotes (pretty name)]
  case result of
    Nothing -> throwError loc undefError
    Just x ->
      case getLocation x of
        Nothing -> return x
        Just (Location srcPath _) -> do
          isVisible <- checkIfVisible srcPath
          if isVisible
            then return x
            else throwError loc . PP.fillSep $
              undefError : PP.words "(you must import" ++ [PP.squotes (pretty srcPath) <> ")"]

showEdgeType :: Text -> GrNode -> GrNode -> Text
showEdgeType e src tgt = formatEdgeType e (nodeName src) (nodeName tgt)

reportAlreadyDefined :: Monad m => Maybe Location -> String -> Text -> Maybe Location -> ExceptT Error m a
reportAlreadyDefined loc kind name prevLoc = throwError loc . PP.fillSep $
  [ pretty kind, PP.squotes (pretty name), "was", "already", "defined" ]
  ++ case prevLoc of
        Nothing -> []
        Just loc' -> ["at" <+> pretty loc']
