{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang (initialize) where

import           Control.Monad
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import qualified Control.Monad.Except      as ExceptT
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

import           Abstract.Category
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
        modify visibleModules (Set.insert path)
        return Nothing
      else do
        outerVisible <- get visibleModules
        put visibleModules (Set.singleton path)
        modify importedModules (Set.insert path)
        result <- importAction `ExceptT.catchError` \err -> do
          modify importedModules (Set.delete path)
          put visibleModules (Set.insert path outerVisible)
          ExceptT.throwError err
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
    hasTable <- liftLua $ Lua.istable (-1)
    if not hasTable
      then GrLang.throwError Nothing "Value is not a table"
      else do
        liftLua $ Lua.getfield (-1) indexKey
        result <- liftLua $ Lua.tointegerx (-1)
        liftLua $ Lua.pop 2
        case result of
          Nothing -> GrLang.throwError Nothing "Value has no index"
          Just memIndex' -> lookupGrLangValue memIndex'

  putValue (A _ name) val = do
    memIdx <- allocateGrLang val
    liftLua $ do
      createTable [(indexKey, Lua.pushinteger $ toEnum memIdx)]
      tblIdx <- Lua.gettop
      Lua.getglobal (metatableFor val)
      Lua.setmetatable tblIdx
      Lua.setglobal (Text.unpack name)
    where
      metatableFor (VGraph _) = "Graph"
      metatableFor (VRule _)  = "Rule"
      metatableFor (VMorph _) = "Morphism"

indexKey :: String
indexKey = "index" :: String

showEdgeType :: Text -> GrNode -> GrNode -> Text
showEdgeType e src tgt = formatEdgeType e (nodeName src) (nodeName tgt)

lookupGrLangValue :: Lua.LuaInteger -> ExceptT GrLang.Error LuaGrLang Value
lookupGrLangValue memIndex' = do
  let memIndex = fromEnum memIndex'
  arr <- lift $ asks values
  result <- liftIO $ readArray arr memIndex
  case result of
    Nothing -> GrLang.throwError Nothing "Value has unallocated index"
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

freeGrLang :: Lua.LuaInteger -> ExceptT GrLang.Error LuaGrLang ()
freeGrLang idx' = do
  let idx = fromEnum idx'
  arr <- asks values
  liftIO $ writeArray arr idx Nothing
  modify numFreeValues (+1)
  modify nextFreeValue $ Just . maybe idx (min idx)

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

  execLua
    " package.path = package.path .. ';./src/repl/lua/?.lua' \
    \ require 'help' \
    \ require 'grlang' "

  initGrLang globalState

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
        Just name -> pretty name <> ":" <> pretty (edgeName etype)
  }

initGrLang :: GrLangState -> Lua ()
initGrLang globalState = do
  setNative "GrLang"
    [ ("getNodeTypes", Lua.pushHaskellFunction
          (map Text.unpack . Map.keys <$> liftIO (readIORef $ nodeTypes globalState) :: Lua [String])
      )
    , ("getEdgeTypes", Lua.pushHaskellFunction . runGrLang' globalState $ do
        types <- Map.keys <$> get edgeTypes
        forM types $ \(name, srcId, tgtId) -> do
          tgraph <- get typeGraph
          let Just srcType = TypeGraph.lookupNode srcId tgraph
              Just tgtType = TypeGraph.lookupNode tgtId tgraph
          return . Text.unpack $ formatEdgeType name (nodeName srcType) (nodeName tgtType)
      )
    , ("addNodeType", Lua.pushHaskellFunction $ \name -> runGrLang' globalState $
          GrLang.addNodeType (A Nothing name)
      )
    , ("addEdgeType", Lua.pushHaskellFunction $ \name srcName tgtName -> runGrLang' globalState $
          GrLang.addEdgeType (A Nothing name) (A Nothing srcName) (A Nothing tgtName)
      )
    , ("toString", Lua.pushHaskellFunction $ \idx -> runGrLang' globalState $
          show . pretty <$> lookupGrLangValue idx
      )
    , ("equals", Lua.pushHaskellFunction $ \idxA idxB -> runGrLang' globalState $ do
          valA <- lookupGrLangValue idxA
          valB <- lookupGrLangValue idxB
          return (valA == valB)
      )
    , ("deallocate", Lua.pushHaskellFunction $ \idx -> runGrLang' globalState $
          freeGrLang idx
      )
    , ("toDot", Lua.pushHaskellFunction $ \idx name -> runGrLang' globalState $ do
          VGraph graph <- lookupGrLangValue idx
          return . show $ Dot.typedGraph grLangNamingContext (pretty $ Text.decodeUtf8 name) graph
      )
    , ("compileFile", Lua.pushHaskellFunction $ \path -> runGrLang' globalState $
          GrLang.compileFile path
      )
    ]

  setNative "Graph"
    [ ("parse", Lua.pushHaskellFunction $ \string -> runGrLang' globalState $ do
          graph <- GrLang.compileGraph =<< GrLang.parseGraph "<repl>" (string :: String)
          idx <- allocateGrLang (VGraph graph)
          return (fromIntegral idx :: Lua.LuaInteger)
      ),
      ("identity", Lua.pushHaskellFunction $ \idx -> runGrLang' globalState $ do
          VGraph graph <- lookupGrLangValue idx
          idx <- allocateGrLang (VMorph $ identity graph)
          return (fromIntegral idx :: Lua.LuaInteger)
      )
    ]

  setNative "Morphism"
    [ ("parse", Lua.pushHaskellFunction $ \domIdx codIdx string -> runGrLang' globalState $ do
          VGraph dom <- lookupGrLangValue domIdx
          VGraph cod <- lookupGrLangValue codIdx
          morphism <- GrLang.compileMorphism Nothing dom cod =<< GrLang.parseMorphism "<repl>" (string :: String)
          idx <- allocateGrLang (VMorph morphism)
          return (fromIntegral idx :: Lua.LuaInteger)
      ),
      ("compose", Lua.pushHaskellFunction $ \idF idG -> runGrLang' globalState $ do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          idx <- allocateGrLang (VMorph $ f <&> g)
          return (fromIntegral idx :: Lua.LuaInteger)
      )
    ]

  setNative "Rule"
    [ ("parse", Lua.pushHaskellFunction $ \string -> runGrLang' globalState $ do
          rule <- GrLang.compileRule =<< GrLang.parseRule "<repl>" (string :: String)
          idx <- allocateGrLang (VRule rule)
          return (fromIntegral idx :: Lua.LuaInteger)
      )
      -- TODO: implement missing methods for rules
    ]
  where
    setNative className entries = do
      Lua.getglobal className
      tableIdx <- Lua.gettop
      createTable entries
      Lua.setfield tableIdx "native"
      Lua.pop 1

createTable :: Foldable t => t (String, Lua ()) -> Lua ()
createTable contents = do
  Lua.createtable 0 (length contents)
  tableIdx <- Lua.gettop
  forM_ contents $ \(key, pushVal) -> do
    pushVal
    Lua.setfield tableIdx key
