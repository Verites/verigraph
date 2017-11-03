{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang (initialize) where

import           Control.Monad
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.Reader
import           Control.Monad.Trans       (lift)
import           Data.Array.IO
import           Data.IORef
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.Prettyprint.Doc (Pretty (..))
import           Foreign.Lua               (Lua)
import qualified Foreign.Lua               as Lua

import           Base.Annotation           (Annotated (..))
import qualified Data.Graphs               as TypeGraph
import           Data.TypedGraph           (EdgeId, Node (..), NodeId)
import qualified GrLang.Compiler           as GrLang
import           GrLang.Monad              (MonadGrLang)
import qualified GrLang.Monad              as GrLang
import qualified GrLang.Parser             as GrLang
import           GrLang.Value
import qualified Image.Dot.TypedGraph      as Dot
import           Util.Lua

data GrLangState = GrLangState
  { typeGraph       :: IORef TypeGraph
  , nodeTypes       :: IORef (Map Text NodeId)
  , edgeTypes       :: IORef (Map (Text, NodeId, NodeId) EdgeId)
  , importedModules :: IORef (Set FilePath)
  , visibleModules  :: IORef (Set FilePath)
  , values          :: IOArray Int (Maybe Value)
  , numFreeValues   :: IORef Int
  , nextFreeValue   :: IORef (Maybe Int)
  , capacity        :: Int
  }

type LuaGrLang = ReaderT GrLangState Lua

get :: (MonadIO m, MonadReader GrLangState m) => (GrLangState -> IORef a) -> m a
get f = liftIO . readIORef =<< asks f

put :: (MonadIO m, MonadReader GrLangState m) => (GrLangState -> IORef a) -> a -> m ()
put f x = do
  ref <- asks f
  liftIO $ writeIORef ref x

modify :: (MonadIO m, MonadReader GrLangState m) => (GrLangState -> IORef a) -> (a -> a) -> m ()
modify f x = do
  ref <- asks f
  liftIO $ modifyIORef ref x

liftLua :: Lua a -> ExceptT GrLang.Error LuaGrLang a
liftLua = lift . lift

instance MonadGrLang (ReaderT GrLangState Lua) where
  importIfNeeded path importAction = do
    prevImported <- get importedModules
    if Set.member path prevImported
      then do
        modify importedModules (Set.insert path)
        return Nothing
      else do
        outerVisible <- get visibleModules
        put visibleModules (Set.singleton path)
        modify importedModules (Set.insert path)
        result <- importAction
        put visibleModules (Set.insert path outerVisible)
        return (Just result)

  checkIfImported srcPath = Set.member srcPath <$> get importedModules
  checkIfVisible srcPath = Set.member srcPath <$> get visibleModules
  unsafeMakeVisible srcPath = modify visibleModules (Set.insert srcPath)

  getTypeGraph = get typeGraph

  getNodeType (A loc name) = GrLang.getOrError loc "node type" name (lookupNodeType name) nodeLocation

  getEdgeType (A loc name) srcType tgtType =
    GrLang.getOrError loc "edge type" (showEdgeType name srcType tgtType) (lookupEdgeType name srcType tgtType) edgeLocation

  addNodeType (A loc name) = do
    existingType <- lift $ lookupNodeType name
    GrLang.addNew loc "Node type" name (nodeLocation <$> existingType) $ do
      newId:_ <- TypeGraph.newNodes <$> get typeGraph
      let metadata = Metadata (Just name) loc
      modify typeGraph $ TypeGraph.insertNodeWithPayload newId (Just metadata)
      modify nodeTypes $ Map.insert name newId

  addEdgeType (A loc name) srcName tgtName = do
    srcType <- GrLang.getNodeType srcName
    tgtType <- GrLang.getNodeType tgtName
    existingType <- lift $ lookupEdgeType name srcType tgtType
    GrLang.addNew loc "Edge type" (showEdgeType name srcType tgtType) (edgeLocation <$> existingType) $ do
      newId:_ <- TypeGraph.newEdges <$> get typeGraph
      let metadata = Metadata (Just name) loc
          (Node srcId _, Node tgtId _) = (srcType, tgtType)
      modify typeGraph $ TypeGraph.insertEdgeWithPayload newId srcId tgtId (Just metadata)
      modify edgeTypes $ Map.insert (name, srcId, tgtId) newId

  getValue (A _ name) = do
    liftLua $ Lua.getglobal (Text.unpack name)
    toGrLangValue (-1)

  putValue (A _ name) val = undefined
  {-
    putValue (A loc name) val = do
      prevVal <- GrLangT $ gets (Map.lookup name . valueContext)
      addNew loc "Value" name (locationOf <$> prevVal) $ \state ->
        state { valueContext = Map.insert name (A loc val) (valueContext state) }
    -}

showEdgeType :: Text -> GrNode -> GrNode -> Text
showEdgeType e src tgt = formatEdgeType e (nodeName src) (nodeName tgt)

toGrLangValue :: Lua.StackIndex -> ExceptT GrLang.Error LuaGrLang Value
toGrLangValue stackIdx = do
  gotTable <- liftLua $ Lua.istable stackIdx
  if not gotTable
    then GrLang.throwError Nothing $ "Value is not a table"
    else do
      liftLua $ Lua.getfield stackIdx indexKey
      result <- liftLua $ Lua.tointegerx (-1)
      liftLua $ Lua.pop 1
      case result of
        Nothing -> GrLang.throwError Nothing $ "Value has no index"
        Just memIndex' -> do
          let memIndex = fromEnum memIndex'
          arr <- lift $ asks values
          result <- liftIO $ readArray arr memIndex
          case result of
            Nothing -> GrLang.throwError Nothing $ "Value has unallocated index"
            Just val -> return val

lookupNodeType :: Text -> LuaGrLang (Maybe NodeType)
lookupNodeType name = do
  tgraph <- get typeGraph
  ntypes <- get nodeTypes
  return $ (`TypeGraph.lookupNode` tgraph) =<< Map.lookup name ntypes

lookupEdgeType :: Text -> NodeType -> NodeType -> LuaGrLang (Maybe EdgeType)
lookupEdgeType name srcType tgtType = do
  tgraph <- get typeGraph
  etypes <- get edgeTypes
  return $ (`TypeGraph.lookupEdge` tgraph)
    =<< Map.lookup (name, nodeId srcType, nodeId tgtType) etypes

initState :: Int -> IO GrLangState
initState maxValues = GrLangState
  <$> newIORef TypeGraph.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef (Set.singleton "<repl>")
  <*> newIORef (Set.singleton "<repl>")
  <*> newArray (0, maxValues-1) Nothing
  <*> newIORef maxValues
  <*> newIORef (Just 0)
  <*> pure maxValues

allocateGrLang :: Value -> ExceptT GrLang.Error LuaGrLang Int
allocateGrLang value = do
  freeIdx <- get nextFreeValue
  case freeIdx of
    Nothing -> GrLang.throwError Nothing "Out of GrLang memory"
    Just idx -> do
      arr <- asks values
      liftIO $ writeArray arr idx (Just value)
      freeVals <- get numFreeValues
      let freeVals' = freeVals - 1
      put numFreeValues freeVals'
      next <- if freeVals' < 1 then return Nothing else Just <$> findFreeValue (idx+1)
      put nextFreeValue next
      return idx
  where
    findFreeValue idx = do
      arr <- asks values
      val <- liftIO $ readArray arr idx
      case val of
        Nothing -> return idx
        Just _ -> findFreeValue (idx+1)

runGrLang' :: Lua.ToLuaStack a => GrLangState -> ExceptT GrLang.Error LuaGrLang a -> Lua Lua.NumResults
runGrLang' globalState action = do
  result <- runReaderT (runExceptT action) globalState
  case result of
    Left errs -> luaError (show $ GrLang.prettyError errs)
    Right val -> do
      Lua.push val
      return 1

initialize :: Lua ()
initialize = do
  globalState <- liftIO (initState 255)

  initGrLang globalState

  execLua
    " function catch_haskell(result, error_msg) \
    \   if result == \"_HASKELLERR\" then \
    \     error(error_msg) \
    \     return \
    \   else \
    \     return result \
    \   end \
    \ end "

  return ()

grLangNamingContext :: Dot.NamingContext Metadata Metadata ann
grLangNamingContext = Dot.Ctx
  { Dot.getNodeTypeName = \(n, _) -> pretty $ nodeTypeName n
  , Dot.getEdgeTypeName = \((s,_), e, (t,_)) -> pretty $ showEdgeType (edgeName e) s t
  , Dot.getNodeName = \_ (node, _, _) -> pretty (nodeName node)
  , Dot.getNodeLabel = \_ (node, ntype, _) ->
      Just $ pretty (nodeName node) <> ":" <> pretty (nodeTypeName ntype)
  , Dot.getEdgeLabel = \_ (_, edge, etype, _) ->
      Just $ case edgeExactName edge of
        Nothing -> pretty $ edgeName etype
        Just name -> (pretty name) <> ":" <> pretty (edgeName etype)
  }

initGrLang :: GrLangState -> Lua ()
initGrLang globalState = do
  createTable
    [ ("mt", createTable
        [ ("__tostring", Lua.pushHaskellFunction . runGrLang' globalState $
              show . pretty <$> toGrLangValue (-1)
          )
        , ("__eq", Lua.pushHaskellFunction $ \g1 g2 ->
              return $ (Map.lookup indexKey g1 :: Maybe Lua.LuaInteger) == Map.lookup indexKey g2 :: Lua Bool)
        , ("__index", createTable
            [ ("to_dot", Lua.pushHaskellFunction . runGrLang' globalState $ do
                  VGraph graph <- toGrLangValue 1
                  name <- liftLua $ Lua.tostring 2
                  return . show $ Dot.typedGraph grLangNamingContext (pretty $ Text.decodeUtf8 name) graph
              )
            ]
          )
        ]
      )
    , ("_addNodeType", Lua.pushHaskellFunction $ \name -> runGrLang' globalState $
          GrLang.addNodeType (A Nothing name)
      )
    , ("_addEdgeType", Lua.pushHaskellFunction $ \name srcName tgtName -> runGrLang' globalState $
          GrLang.addEdgeType (A Nothing name) (A Nothing srcName) (A Nothing tgtName)
      )
    , ("_getNodeTypes", Lua.pushHaskellFunction $
          (map Text.unpack . Map.keys <$> liftIO (readIORef $ nodeTypes globalState) :: Lua [String])
      )
    , ("_getEdgeTypes", Lua.pushHaskellFunction . runGrLang' globalState $ do
          types <- Map.keys <$> get edgeTypes
          forM types $ \(name, srcId, tgtId) -> do
            tgraph <- get typeGraph
            let Just srcType = TypeGraph.lookupNode srcId tgraph
                Just tgtType = TypeGraph.lookupNode tgtId tgraph
            return . Text.unpack $ formatEdgeType name (nodeName srcType) (nodeName tgtType)
      )
    , ("_parseGraph", Lua.pushHaskellFunction $ \string -> runGrLang' globalState $ do
          graph <- GrLang.compileGraph =<< GrLang.parseGraph "<repl>" (string :: String)
          idx <- allocateGrLang (VGraph graph)
          return (fromIntegral idx :: Lua.LuaInteger)
      )
    ]
  Lua.setglobal "GrLang"

  execLua
    " function GrLang.node_types() \
    \   return catch_haskell(GrLang._getNodeTypes()) \
    \ end \
    \ function GrLang.edge_types() \
    \   return catch_haskell(GrLang._getEdgeTypes()) \
    \ end \
    \ function node_type(name) \
    \   return catch_haskell(GrLang._addNodeType(name)) \
    \ end \
    \ function edge_type(name, srcName, tgtName) \
    \   return catch_haskell(GrLang._addEdgeType(name, srcName, tgtName)) \
    \ end \
    \ function graph(str) \
    \   local idx = catch_haskell(GrLang._parseGraph(str)) \
    \   local g = {index = idx} \
    \   setmetatable(g, GrLang.mt) \
    \   return g \
    \ end \
    \ function GrLang.mt.__index.view(graph) \
    \   local file_name = '/tmp/verigraph-dot' .. os.date() \
    \   local file = io.open(file_name, 'w') \
    \   file:write(graph:to_dot('')) \
    \   file:close() \
    \   os.execute('xdot \"' .. file_name .. '\"') \
    \ end "

indexKey :: String
indexKey = "index" :: String

createTable :: Foldable t => t (String, Lua ()) -> Lua ()
createTable contents = do
  Lua.createtable 0 (length contents)
  tableIdx <- Lua.gettop
  forM_ contents $ \(key, pushVal) -> do
    pushVal
    Lua.setfield tableIdx key
