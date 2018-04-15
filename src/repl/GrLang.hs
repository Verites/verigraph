{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang (initialize) where

import           Control.Monad
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import qualified Control.Monad.Except       as ExceptT
import           Control.Monad.Reader
import           Control.Monad.Trans        (lift)
import           Data.Array.IO
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Text.Prettyprint.Doc  (Pretty (..))
import           Foreign.Lua                (FromLuaStack, Lua, ToLuaStack)
import qualified Foreign.Lua                as Lua

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Base.Annotation            (Annotated (..))
import qualified Data.Graphs                as TypeGraph
import           Data.TypedGraph            (EdgeId, Node (..), NodeId)
import qualified GrLang.Compiler            as GrLang
import           GrLang.Monad               (MonadGrLang)
import qualified GrLang.Monad               as GrLang
import qualified GrLang.Parser              as GrLang
import           GrLang.Value
import qualified Image.Dot.TypedGraph       as Dot
import           Util.Lua

data MemSpace a = MemSpace
  { cells         :: IOArray Int (Maybe a)
  , numFreeValues :: IORef Int
  , nextFreeValue :: IORef (Maybe Int)
  }

emptyMemSpace :: Int -> IO (MemSpace a)
emptyMemSpace capacity = MemSpace
  <$> newArray (0, capacity - 1) Nothing
  <*> newIORef capacity
  <*> newIORef (Just 0)

lookupMemSpace :: Lua.LuaInteger -> MemSpace a -> ExceptT GrLang.Error LuaGrLang a
lookupMemSpace idx' memSpace = do
  let idx = fromEnum idx'
  result <- liftIO $ readArray (cells memSpace) idx
  case result of
    Nothing -> GrLang.throwError Nothing "Value has unallocated index"
    Just val -> return val

allocateMemSpace :: a -> MemSpace a -> ExceptT GrLang.Error LuaGrLang Lua.LuaInteger
allocateMemSpace value memSpace = do
  freeIdx <- liftIO $ readIORef (nextFreeValue memSpace)
  case freeIdx of
    Nothing -> GrLang.throwError Nothing "Out of GrLang memory"
    Just idx -> liftIO $ do
      writeArray (cells memSpace) idx (Just value)
      freeVals <- readIORef (numFreeValues memSpace)
      let freeVals' = freeVals - 1
      writeIORef (numFreeValues memSpace) freeVals'
      next <- if freeVals' < 1 then return Nothing else Just <$> findFreeValue (idx+1)
      writeIORef (nextFreeValue memSpace) next
      return (fromIntegral idx)
  where
    findFreeValue idx = do
      val <- readArray (cells memSpace) idx
      case val of
        Nothing -> return idx
        Just _ -> findFreeValue (idx+1)

freeMemSpace :: Lua.LuaInteger -> MemSpace a -> ExceptT GrLang.Error LuaGrLang ()
freeMemSpace idx' memSpace = liftIO $ do
  let idx = fromEnum idx'
  writeArray (cells memSpace) idx Nothing
  modifyIORef (numFreeValues memSpace) (+1)
  modifyIORef (nextFreeValue memSpace) $ Just . maybe idx (min idx)

data GrLangState = GrLangState
  { typeGraph       :: IORef TypeGraph
  , nodeTypes       :: IORef (Map Text NodeId)
  , edgeTypes       :: IORef (Map (Text, NodeId, NodeId) EdgeId)
  , importedModules :: IORef (Set FilePath)
  , visibleModules  :: IORef (Set FilePath)
  , values          :: MemSpace Value
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

withMemSpace :: (GrLangState -> MemSpace a) -> (MemSpace a -> ExceptT GrLang.Error LuaGrLang b) -> ExceptT GrLang.Error LuaGrLang b
withMemSpace space action = asks space >>= action

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
    hasTable <- liftLua $ Lua.istable Lua.stackTop
    if not hasTable
      then GrLang.throwError Nothing "Value is not a table"
      else do
        liftLua $ Lua.getfield Lua.stackTop indexKey
        result <- liftLua $ Lua.tointegerx Lua.stackTop
        liftLua $ Lua.pop 2
        case result of
          Nothing -> GrLang.throwError Nothing "Value has no index"
          Just memIndex' -> lookupGrLangValue memIndex'

  putValue (A _ name) val = do
    memIdx <- allocateGrLang val
    liftLua $ do
      createTable [(indexKey, Lua.pushinteger memIdx)]
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
lookupGrLangValue = withMemSpace values . lookupMemSpace

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
  <*> emptyMemSpace maxValues

allocateGrLang :: Value -> ExceptT GrLang.Error LuaGrLang Lua.LuaInteger
allocateGrLang = withMemSpace values . allocateMemSpace

freeGrLang :: Lua.LuaInteger -> ExceptT GrLang.Error LuaGrLang ()
freeGrLang = withMemSpace values . freeMemSpace

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
    [ ("getNodeTypes", haskellFn0 globalState $ do
          types <- get nodeTypes
          return . map Text.unpack $ Map.keys types
      )
    , ("getEdgeTypes", haskellFn0 globalState $ do
          types <- Map.keys <$> get edgeTypes
          forM types $ \(name, srcId, tgtId) -> do
            tgraph <- get typeGraph
            let Just srcType = TypeGraph.lookupNode srcId tgraph
                Just tgtType = TypeGraph.lookupNode tgtId tgraph
            return . Text.unpack $ formatEdgeType name (nodeName srcType) (nodeName tgtType)
      )
    , ("addNodeType", haskellFn1 globalState $ \name ->
          GrLang.addNodeType (A Nothing name)
      )
    , ("addEdgeType", haskellFn3 globalState $ \name srcName tgtName ->
          GrLang.addEdgeType (A Nothing name) (A Nothing srcName) (A Nothing tgtName)
      )
    , ("toString", haskellFn1 globalState $ \idx ->
          show . pretty <$> lookupGrLangValue idx
      )
    , ("equals", haskellFn2 globalState $ \idxA idxB -> do
          valA <- lookupGrLangValue idxA
          valB <- lookupGrLangValue idxB
          return (valA == valB)
      )
    , ("deallocate", haskellFn1 globalState $ \idx ->
          freeGrLang idx
      )
    , ("toDot", haskellFn2 globalState $ \idx name -> do
          VGraph graph <- lookupGrLangValue idx
          return . show $ Dot.typedGraph grLangNamingContext (pretty $ Text.decodeUtf8 name) graph
      )
    , ("compileFile", haskellFn1 globalState $ \path ->
          GrLang.compileFile path
      )
    ]

  setNative "Graph"
    [ ("parse", haskellFn1 globalState $ \string -> do
          graph <- GrLang.compileGraph =<< GrLang.parseGraph "<repl>" (string :: String)
          allocateGrLang (VGraph graph)
      ),
      ("identity", haskellFn1 globalState $ \idx -> do
          VGraph graph <- lookupGrLangValue idx
          allocateGrLang (VMorph $ identity graph)
      )
    ]

  setNative "Morphism"
    [ ("parse", haskellFn3 globalState $ \domIdx codIdx string -> do
          VGraph dom <- lookupGrLangValue domIdx
          VGraph cod <- lookupGrLangValue codIdx
          morphism <- GrLang.compileMorphism Nothing dom cod =<< GrLang.parseMorphism "<repl>" (string :: String)
          allocateGrLang (VMorph morphism)
      ),
      ("compose", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          allocateGrLang (VMorph $ f <&> g)
      ),
      ("isMonic", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isMonic f)
      ),
      ("isEpic", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isEpic f)
      ),
      ("isIsomorphism", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isIsomorphism f)
      ),
      ("calculatePullback", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let (f', g') = calculatePullback f g
          (,,)
            <$> allocateGrLang (VGraph $ domain f')
            <*> allocateGrLang (VMorph f')
            <*> allocateGrLang (VMorph g')
      ),
      ("calculateInitialPushout", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          let (b, f', c) = calculateMInitialPushout f
          (,,,,)
            <$> allocateGrLang (VGraph $ domain b)
            <*> allocateGrLang (VGraph $ domain c)
            <*> allocateGrLang (VMorph b)
            <*> allocateGrLang (VMorph f')
            <*> allocateGrLang (VMorph c)
      ),
      ("subobjectIntersection", haskellFn2 globalState $ \idA idB -> do
          VMorph a <- lookupGrLangValue idA
          VMorph b <- lookupGrLangValue idB
          let c = subobjectIntersection a b
          (,)
            <$> (allocateGrLang . VGraph $ domain c)
            <*> (allocateGrLang . VMorph $ c)
      ),
      ("subobjectUnion", haskellFn2 globalState $ \idA idB -> do
          VMorph a <- lookupGrLangValue idA
          VMorph b <- lookupGrLangValue idB
          let c = subobjectUnion a b
          (,)
            <$> (allocateGrLang . VGraph $ domain c)
            <*> (allocateGrLang . VMorph $ c)
      )
    ]

  setNative "Rule"
    [ ("parse", haskellFn1 globalState $ \string -> do
          rule <- GrLang.compileRule =<< GrLang.parseRule "<repl>" (string :: String)
          allocateGrLang (VRule rule)
      )
    , ("getLeftObject", haskellFn1 globalState $ \idRule -> do
          VRule rule <- lookupGrLangValue idRule
          allocateGrLang (VGraph $ leftObject rule)
      )
    , ("getRightObject", haskellFn1 globalState $ \idRule -> do
          VRule rule <- lookupGrLangValue idRule
          allocateGrLang (VGraph $ rightObject rule)
      )
    , ("getInterface", haskellFn1 globalState $ \idRule -> do
          VRule rule <- lookupGrLangValue idRule
          allocateGrLang (VGraph $ interfaceObject rule)
      )
    , ("getLeftMorphism", haskellFn1 globalState $ \idRule -> do
          VRule rule <- lookupGrLangValue idRule
          allocateGrLang (VMorph $ leftMorphism rule)
      )
    , ("getRightMorphism", haskellFn1 globalState $ \idRule -> do
          VRule rule <- lookupGrLangValue idRule
          allocateGrLang (VMorph $ rightMorphism rule)
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

haskellFn0 :: ToLuaStack a => GrLangState -> ExceptT GrLang.Error LuaGrLang a -> Lua ()
haskellFn0 globalState f = pushFunction (runGrLang' globalState f)

haskellFn1 ::
  (FromLuaStack a, ToLuaStack b) =>
  GrLangState -> (a -> ExceptT GrLang.Error LuaGrLang b) -> Lua ()
haskellFn1 globalState f = pushFunction (runGrLang' globalState . f)

haskellFn2 ::
  (FromLuaStack a, FromLuaStack b, ToLuaStack c) =>
  GrLangState -> (a -> b -> ExceptT GrLang.Error LuaGrLang c) -> Lua ()
haskellFn2 globalState f = pushFunction (\x y -> runGrLang' globalState (f x y))

haskellFn3 ::
  (FromLuaStack a, FromLuaStack b, FromLuaStack c, ToLuaStack d) =>
  GrLangState -> (a -> b -> c -> ExceptT GrLang.Error LuaGrLang d) -> Lua ()
haskellFn3 globalState f = pushFunction (\x y z -> runGrLang' globalState (f x y z))

createTable :: Foldable t => t (String, Lua ()) -> Lua ()
createTable contents = do
  Lua.createtable 0 (length contents)
  tableIdx <- Lua.gettop
  forM_ contents $ \(key, pushVal) -> do
    pushVal
    Lua.setfield tableIdx key
