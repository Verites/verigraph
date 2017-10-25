{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module GrLang.Monad
  ( GrLangT
  , runGrLangT
  , Error
  , prettyError
  , GrLangState
  , emptyState

  -- * Local state
  , withLocalState

  -- * Error handling
  , throwError
  , throwErrors
  , registerError
  , suspendErrors
  , throwingPendingErrors
  -- ** Utilities
  , getOrError
  , registerAlreadyDefined

  -- * Type Graph
  , getTypeGraph
  , lookupNodeType
  , lookupEdgeType
  , getNodeType
  , getEdgeType
  , addNodeType
  , addEdgeType

  -- * Value Context
  , getValue
  , putValue
  , getValueContext
  ) where

import           Control.Arrow             (first)
import           Control.Monad.Except      (ExceptT, runExceptT)
import qualified Control.Monad.Except      as ExceptT
import           Control.Monad.State
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as PP

import           Base.Annotation           (Annotated (..), Located, locatedDoc, locationOf)
import qualified Base.Annotation           as Ann
import           Base.Location
import           Data.DList                (DList)
import qualified Data.DList                as DList
import qualified Data.Graphs               as TypeGraph
import           Data.TypedGraph           (EdgeId, Node (..), NodeId)
import           GrLang.Value

-- | A monad transformer for compiling or interpreting the GrLang.
-- It allows the user to store local state of type @u@.
newtype GrLangT u m a =
  GrLangT { unGrLangT :: ExceptT (DList Error) (StateT (GrLangState, u) m) a }
  deriving (Functor, Applicative, Monad)

type Error = Located (Doc ())

prettyError :: Error -> Doc ()
prettyError = locatedDoc

instance Monad m => MonadState u (GrLangT u m) where
  state f = GrLangT (state f')
    where
      f' (st, u) =
        let (a, u') = f u
        in (a, (st, u'))

instance MonadIO m => MonadIO (GrLangT u m) where
  liftIO = GrLangT . liftIO

instance MonadTrans (GrLangT u) where
  lift = GrLangT . lift . lift

data GrLangState = St
  { typeGraph     :: TypeGraph
  , nodeTypes     :: Map Text NodeId
  , edgeTypes     :: Map (Text, NodeId, NodeId) EdgeId
  , valueContext  :: Map Text (Located Value)
  , pendingErrors :: DList Error
  }

emptyState :: GrLangState
emptyState = St TypeGraph.empty Map.empty Map.empty Map.empty DList.empty

data Void

-- | Run a GrLang computation with the given initial state. If the computation
-- fails, produce a list of errors.
runGrLangT :: Monad m => GrLangState -> GrLangT Void m a -> m (Either [Error] a)
runGrLangT st (GrLangT action) = do
  (result, (st', _)) <- runStateT (runExceptT action) (st, undefined)
  return $ case (result, DList.null (pendingErrors st')) of
    (Right val, True) -> Right val
    (Left errs, _) -> Left $ DList.toList (pendingErrors st' <> errs)

-- | Run a computation with additional local state, starting with the given
-- initial state. Discards the local state when the computation is finished.
withLocalState :: Monad m => u -> GrLangT u m a -> GrLangT s m a
withLocalState inner = GrLangT . ExceptT.mapExceptT changeState . unGrLangT
  where changeState action = do
          (prevState, outer) <- gets id
          (result, (newState, _)) <- lift $ runStateT action (prevState, inner)
          put (newState, outer)
          return result

-- | Register the given error and fail the computation.
--
-- Fails with the given error as well as all pending errors.
throwError :: Monad m => Maybe Location -> Doc () -> GrLangT u m a
throwError src descr = GrLangT $ do
  pending <- gets (pendingErrors . fst)
  ExceptT.throwError (pending `DList.snoc` A src descr)

-- | Register the given errors and fail the computation.
--
-- Fails with the given errors as well as all pending errors.
throwErrors :: Monad m => Maybe Location -> [Doc ()] -> GrLangT u m a
throwErrors src descrs = GrLangT $ do
  pending <- gets (pendingErrors . fst)
  ExceptT.throwError (pending <> DList.fromList [A src d | d <- descrs])
-- | Register the given error but don't fail the computation yet.
registerError :: Monad m => Maybe Location -> Doc () -> GrLangT u m ()
registerError src descr = GrLangT .  modify . first $ \state ->
  state { pendingErrors = pendingErrors state `DList.snoc` A src descr }

-- | Run the given computation but, if it fails, register its errors as pending
-- and return @Nothing@.
suspendErrors :: Monad m => GrLangT u m a -> GrLangT u m (Maybe a)
suspendErrors (GrLangT action) = GrLangT $
  ExceptT.catchError (Just <$> action) $ \errors -> do
    modify . first $ \state -> state { pendingErrors = pendingErrors state <> errors }
    return Nothing

-- | Run the given computation but, if it registers any pending errors,
-- force its failure.
--
-- When the resulting computation fails, it will also include pending errors
-- that were registered by previous computations. But it will only fail if it
-- registers /new/ errors, besides those that were already registered.
throwingPendingErrors :: Monad m => GrLangT u m a -> GrLangT u m a
throwingPendingErrors (GrLangT action) = GrLangT $ do
  prevErrors <- gets (pendingErrors . fst)
  modify . first $ \state -> state { pendingErrors = DList.empty }
  result <- action
  newErrors <- gets (pendingErrors . fst)
  modify . first $ \state -> state { pendingErrors = prevErrors }
  if DList.null newErrors
    then return result
    else ExceptT.throwError (prevErrors <> newErrors)

-- | Obtain the current type graph.
getTypeGraph :: Monad m => GrLangT u m TypeGraph
getTypeGraph = GrLangT $ gets (typeGraph . fst)

-- | Lookup the node type that has the given name.
lookupNodeType :: Monad m => Text -> GrLangT u m (Maybe NodeType)
lookupNodeType name = GrLangT $ do
  tgraph <- gets (typeGraph . fst)
  ntypes <- gets (nodeTypes . fst)
  return $ (`TypeGraph.lookupNode` tgraph) =<< Map.lookup name ntypes

-- | Lookup the edge type that has the given name, source type and target type.
lookupEdgeType :: Monad m => Text -> NodeType -> NodeType -> GrLangT u m (Maybe EdgeType)
lookupEdgeType name (Node srcId _) (Node tgtId _) = GrLangT $ do
  tgraph <- gets (typeGraph . fst)
  etypes <- gets (edgeTypes . fst)
  return $ (`TypeGraph.lookupEdge` tgraph) =<< Map.lookup (name, srcId, tgtId) etypes

-- | Get the node type that has the given name, or throw an error and fail if
-- none exists.
getNodeType :: Monad m => Located Text -> GrLangT u m NodeType
getNodeType (A loc name) =
  getOrError loc "node type" name (lookupNodeType name)

-- | Get the edge type that has the given name, source type and target type or
-- throw an error and fail if none exists.
getEdgeType :: Monad m => Located Text -> NodeType -> NodeType -> GrLangT u m EdgeType
getEdgeType (A loc name) srcType tgtType =
  getOrError loc "edge type" (showEdgeType name srcType tgtType) (lookupEdgeType name srcType tgtType)

-- | Create a new node type with given name. If such a type already exists, fails.
addNodeType :: Monad m => Located Text -> GrLangT u m ()
addNodeType (A loc name) = do
  existingType <- lookupNodeType name
  addNew loc "Node type" name (nodeLocation <$> existingType) $ \state ->
    let newId:_ = TypeGraph.newNodes (typeGraph state)
        metadata = Metadata (Just name) loc
    in state
      { typeGraph = TypeGraph.insertNodeWithPayload newId (Just metadata) (typeGraph state)
      , nodeTypes = Map.insert name newId (nodeTypes state) }

-- | Create a new edge type with given name, as well as source and target types.
-- If such a type already exists, fails.
--
-- Note: this allows multiple edge types with the same name, as long as they
-- have different source or target types.
addEdgeType :: Monad m => Located Text -> Located Text -> Located Text -> GrLangT u m ()
addEdgeType (A loc name) srcName tgtName = do
  srcType <- getNodeType srcName
  tgtType <- getNodeType tgtName
  existingType <- lookupEdgeType name srcType tgtType
  addNew loc "Edge type" (showEdgeType name srcType tgtType) (edgeLocation <$> existingType) $ \state ->
    let newId:_ = TypeGraph.newEdges (typeGraph state)
        metadata = Metadata (Just name) loc
        (Node srcId _, Node tgtId _) = (srcType, tgtType)
    in state
      { typeGraph = TypeGraph.insertEdgeWithPayload newId srcId tgtId (Just metadata) (typeGraph state)
      , edgeTypes = Map.insert (name, srcId, tgtId) newId (edgeTypes state) }

-- | Obtain the value associated to the given name.
lookupValue :: Monad m => Text -> GrLangT u m (Maybe Value)
lookupValue key = fmap Ann.drop . Map.lookup key <$> getValueContext

-- | Obtain the value associated to the given name, or fail if there is none.
getValue :: Monad m => Located Text -> GrLangT u m Value
getValue (A loc name) = getOrError loc "value" name (lookupValue name)

-- | Bind the given value to the given name. If the name is already bound,
-- fails.
putValue :: Monad m => Annotated (Maybe Location) Text -> Value -> GrLangT b m ()
putValue (A loc name) val = do
  prevVal <- GrLangT $ gets (Map.lookup name . valueContext . fst)
  addNew loc "Value" name (locationOf <$> prevVal) $ \state ->
    state { valueContext = Map.insert name (A loc val) (valueContext state) }

getValueContext :: Monad m => GrLangT u m (Map Text (Located Value))
getValueContext = GrLangT $ gets (valueContext . fst)

showEdgeType :: Text -> GrNode -> GrNode -> Text
showEdgeType e src tgt = formatEdgeType e (nodeName src) (nodeName tgt)

getOrError :: Monad m => Maybe Location -> String -> Text -> GrLangT u m (Maybe a) -> GrLangT u m a
getOrError loc kind name getter = do
  result <- getter
  case result of
    Just x -> return x
    Nothing -> throwError loc $ "Undefined" <+> pretty kind <+> PP.squotes (pretty name)

addNew :: Monad m => Maybe Location -> String -> Text -> Maybe (Maybe Location) -> (GrLangState -> GrLangState) -> GrLangT u m ()
addNew loc kind name existingLocation addToState =
  case existingLocation of
    Just loc' -> registerAlreadyDefined loc kind name loc'
    Nothing -> GrLangT $ modify (first addToState)

registerAlreadyDefined :: Monad m => Maybe Location -> String -> Text -> Maybe Location -> GrLangT u m ()
registerAlreadyDefined loc kind name prevLoc = registerError loc $
  pretty kind <+> PP.squotes (pretty name) <+> "already defined" <>
    case prevLoc of
      Nothing -> PP.emptyDoc
      Just loc' -> " at" <+> pretty loc'
