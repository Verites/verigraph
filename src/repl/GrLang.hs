{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module GrLang (initialize) where

import           Control.Arrow                  ((***))
import           Control.Monad
import           Control.Monad.Except           (ExceptT (..), runExceptT)
import qualified Control.Monad.Except           as ExceptT
import           Control.Monad.Reader
import           Control.Monad.Trans            (lift)
import           Data.Array.IO
import qualified Data.ByteString                as BS
import           Data.IORef
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Proxy
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Text.Prettyprint.Doc      (Pretty (..))
import           Foreign.Lua                    (FromLuaStack, Lua, ToHaskellFunction)
import qualified Foreign.Lua                    as Lua
import qualified Foreign.Lua.Util               as Lua

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Base.Annotation                (Annotated (..))
import qualified Data.Graphs                    as TypeGraph
import           Data.TypedGraph                (Edge (..), EdgeId, Node (..), NodeId)
import qualified Data.TypedGraph                as TGraph
import qualified Data.TypedGraph.Morphism       as TGraph
import qualified GrLang.Compiler                as GrLang
import           GrLang.Monad                   (MonadGrLang)
import qualified GrLang.Monad                   as GrLang
import qualified GrLang.Parser                  as GrLang
import           GrLang.Value
import qualified Image.Dot.TypedGraph           as Dot
import           Util.Lua
import           XML.GGXReader                  as GGX
import Paths_verigraph (getDataFileName)

data MemSpace a = MemSpace
  { cells         :: IOArray Int (Maybe a)
  , maxFreeValues :: Int
  , numFreeValues :: IORef Int
  , nextFreeIndex :: IORef (Maybe Int)
  }

emptyMemSpace :: Int -> IO (MemSpace a)
emptyMemSpace capacity = MemSpace
  <$> newArray (0, capacity - 1) Nothing
  <*> pure capacity
  <*> newIORef capacity
  <*> newIORef (Just 0)

lookupMemSpace :: Lua.LuaInteger -> MemSpace a -> ExceptT GrLang.Error LuaGrLang a
lookupMemSpace idx' memSpace = do
  let idx = fromEnum idx'
  result <- liftIO $ readArray (cells memSpace) idx
  case result of
    Nothing -> GrLang.throwError Nothing "Value has unallocated index"
    Just val -> return val

isEmptyMemSpace :: MemSpace a -> ExceptT GrLang.Error LuaGrLang Bool
isEmptyMemSpace memSpace =
  (maxFreeValues memSpace ==) <$> liftIO (readIORef $ numFreeValues memSpace)

putMemSpace :: Lua.LuaInteger -> a -> MemSpace a -> ExceptT GrLang.Error LuaGrLang ()
putMemSpace idx' val memSpace = do
  let idx = fromEnum idx'
  liftIO $ writeArray (cells memSpace) idx (Just val)

allocateMemSpace :: a -> MemSpace a -> ExceptT GrLang.Error LuaGrLang Lua.LuaInteger
allocateMemSpace value memSpace = do
  idx <- getFreeIndex memSpace
  liftIO $ do
    writeArray (cells memSpace) idx (Just value)
    freeVals <- readIORef (numFreeValues memSpace)
    let freeVals' = freeVals - 1
    writeIORef (numFreeValues memSpace) freeVals'
    writeIORef (nextFreeIndex memSpace) =<<
      if freeVals' < 1
        then return Nothing
        else Just <$> findNextFreeIndex (idx+1)
    return (fromIntegral idx)
  where
    getFreeIndex memSpace = do
      freeIdx <- liftIO $ readIORef (nextFreeIndex memSpace)
      case freeIdx of
        Just idx -> return idx
        Nothing -> do
          liftLua $ Lua.gc Lua.GCCOLLECT 0
          freeIdx <- liftIO $ readIORef (nextFreeIndex memSpace)
          case freeIdx of
            Just idx -> return idx
            Nothing -> GrLang.throwError Nothing "Out of GrLang memory"

    findNextFreeIndex idx = do
      val <- readArray (cells memSpace) idx
      case val of
        Nothing -> return idx
        Just _ -> findNextFreeIndex (idx+1)

freeMemSpace :: Lua.LuaInteger -> MemSpace a -> ExceptT GrLang.Error LuaGrLang ()
freeMemSpace idx' memSpace = liftIO $ do
  let idx = fromEnum idx'
  writeArray (cells memSpace) idx Nothing
  modifyIORef (numFreeValues memSpace) (+1)
  modifyIORef (nextFreeIndex memSpace) $ Just . maybe idx (min idx)

data GrLangState = GrLangState
  { typeGraph       :: IORef TypeGraph
  , nodeTypes       :: IORef (Map Text NodeId)
  , edgeTypes       :: IORef (Map (Text, NodeId, NodeId) EdgeId)
  , importedModules :: IORef (Set FilePath)
  , visibleModules  :: IORef (Set FilePath)
  , values          :: MemSpace Value
  , iterLists       :: MemSpace [[Value]]
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

memSpacesAreEmpty :: ExceptT GrLang.Error LuaGrLang Bool
memSpacesAreEmpty = do
  areEmpty' <- (&&) <$> withMemSpace values isEmptyMemSpace <*> withMemSpace iterLists isEmptyMemSpace
  if areEmpty'
    then return True
    else do
      liftLua $ Lua.gc Lua.GCCOLLECT 0
      (&&) <$> withMemSpace values isEmptyMemSpace <*> withMemSpace iterLists isEmptyMemSpace

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

initState :: Int -> Int -> IO GrLangState
initState maxValues maxLists = GrLangState
  <$> newIORef TypeGraph.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef (Set.singleton "<repl>")
  <*> newIORef (Set.singleton "<repl>")
  <*> emptyMemSpace maxValues
  <*> emptyMemSpace maxLists

allocateGrLang :: Value -> ExceptT GrLang.Error LuaGrLang Lua.LuaInteger
allocateGrLang = withMemSpace values . allocateMemSpace . normalizeValue

freeGrLang :: Lua.LuaInteger -> ExceptT GrLang.Error LuaGrLang ()
freeGrLang = withMemSpace values . freeMemSpace

runGrLang' :: ToHaskellFunction (Lua a) =>
  GrLangState -> ExceptT GrLang.Error LuaGrLang a -> Lua Lua.NumResults
runGrLang' globalState action = do
  result <- runReaderT (runExceptT action) globalState
  case result of
    Left errs -> luaError (show $ GrLang.prettyError errs)
    Right val -> Lua.toHaskellFunction (return @Lua val)

morphConf :: MorphismsConfig GrMorphism
morphConf = MorphismsConfig monic

initialize :: Lua ()
initialize = do
  globalState <- liftIO (initState 256 64)

  execLuaFile =<< liftIO (getDataFileName "src/repl/lua/help.lua")
  execLuaFile =<< liftIO (getDataFileName "src/repl/lua/grlang.lua")
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
    , ("resetTypes", haskellFn0 globalState $ do
          areEmpty <- memSpacesAreEmpty
          unless areEmpty . GrLang.throwError Nothing $
            "Cannot reset types, some GrLang values still refer to the type graph."
          put typeGraph TypeGraph.empty
          put nodeTypes Map.empty
          put edgeTypes Map.empty
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
    , ("readGGX", haskellFn1 globalState $ \fileName -> do
          (grammar, _, _) <- liftIO $ GGX.readGrammar fileName False morphConf
          let makeValidIdentifier = Text.pack . GrLang.makeIdentifier . takeWhile (/='%')
          names <- liftIO $ Map.fromList . map (id *** makeValidIdentifier) <$> GGX.readNames fileName

          -- Construct a type graph with proper names and update the current type graph
          let tgraph = useGgxNamesOnTypeGraph (fmap correctTypeName names) $ TGraph.typeGraph (start grammar)
                where correctTypeName = Text.takeWhile (/='%')
          put typeGraph tgraph
          put nodeTypes $ Map.fromList [(nodeName n, nodeId n) | n <- TypeGraph.nodes tgraph ]
          put edgeTypes $ Map.fromList [((edgeName e, sourceId e, targetId e), edgeId e) | e <- TypeGraph.edges tgraph ]

          -- Return a table of productions indexed by name
          prods <- forM (productions grammar) $ \(name, prod) -> do
            idx <- allocateGrLang . addNamesFromTypes . useGgxNames names . updateTypeGraph tgraph $ VRule prod
            return (name, idx)
          return (Map.fromList prods)
      )
    ]

  setNative "HsListIterator"
    [ ("deallocate", haskellFn1 globalState $ \idx ->
          withMemSpace iterLists $ freeMemSpace idx
      )
    , ("hasNextItem", haskellFn1 globalState $ \idx ->
          not . null <$> withMemSpace iterLists (lookupMemSpace idx)
      )
    , ("getNextItem", haskellFn1 globalState $ \listIdx -> do
          list <- withMemSpace iterLists $ lookupMemSpace listIdx
          case list of
            [] -> return (0 :: Lua.NumResults)
            (vals : rest) -> do
              withMemSpace iterLists $ putMemSpace listIdx rest
              returnVals vals
      )
    ]

  setNative "Graph"
    [ ("parse", haskellFn1 globalState $ \string -> do
          graph <- GrLang.compileGraph =<< GrLang.parseGraph "<repl>" (string :: String)
          allocateGrLang (VGraph graph)
      )
    , ("identity", haskellFn1 globalState $ \idx -> do
          VGraph graph <- lookupGrLangValue idx
          allocateGrLang (VMorph $ identity graph)
      )
    , ("isInitial", haskellFn1 globalState $ \idx -> do
          VGraph graph <- lookupGrLangValue idx
          return (isInitial (Proxy @GrMorphism) graph)
      )
    , ("calculateCoproduct", haskellFn2 globalState $ \idG idH -> do
          VGraph g <- lookupGrLangValue idG
          VGraph h <- lookupGrLangValue idH
          let (jG, jH) = calculateCoproduct g h
          returnVals [VGraph (codomain jG), VMorph jG, VMorph jH]
      )
    , ("calculateProduct", haskellFn2 globalState $ \idG idH -> do
          VGraph g <- lookupGrLangValue idG
          VGraph h <- lookupGrLangValue idH
          let (pG, pH) = calculateProduct g h
          returnVals [VGraph (domain pG), VMorph pG, VMorph pH]
      )
    , ("findMorphisms", haskellFn3 globalState $ \kindStr idG idH -> do
          VGraph g <- lookupGrLangValue idG
          VGraph h <- lookupGrLangValue idH
          cls <- morphClassFromString kindStr
          withMemSpace iterLists .
            allocateMemSpace . map (\f -> [VMorph f]) $ findMorphisms cls g h
      )
    , ("findAllSubobjectsOf", haskellFn1 globalState $ \idx -> do
          VGraph g <- lookupGrLangValue idx
          withMemSpace iterLists .
            allocateMemSpace . map (\f -> [VGraph (domain f), VMorph f]) $
              findAllSubobjectsOf g
      )
    , ("findAllQuotientsOf", haskellFn1 globalState $ \idx -> do
          VGraph g <- lookupGrLangValue idx
          withMemSpace iterLists .
            allocateMemSpace . map (\f -> [VGraph (codomain f), VMorph f]) $
              findAllQuotientsOf g
      )
    , ("findJointSurjections", haskellFn4 globalState $ \idG kindStrG idH kindStrH -> do
          VGraph g <- lookupGrLangValue idG
          VGraph h <- lookupGrLangValue idH
          clsG <- morphClassFromString kindStrG
          clsH <- morphClassFromString kindStrH
          withMemSpace iterLists .
            allocateMemSpace . map (\(jg, jh) -> [VGraph (codomain jg), VMorph jg, VMorph jh]) $
              findJointSurjections (clsG, g) (clsH, h)
      )
    ]

  setNative "Morphism"
    [ ("parse", haskellFn3 globalState $ \domIdx codIdx string -> do
          VGraph dom <- lookupGrLangValue domIdx
          VGraph cod <- lookupGrLangValue codIdx
          morphism <- GrLang.compileMorphism Nothing dom cod =<< GrLang.parseMorphism "<repl>" (string :: String)
          allocateGrLang (VMorph morphism)
      )
    , ("compose", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          allocateGrLang (VMorph $ f <&> g)
      )
    , ("isMonic", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isMonic f)
      )
    , ("isEpic", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isEpic f)
      )
    , ("isIsomorphism", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          return (isIsomorphism f)
      )
    , ("calculatePullback", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let (f', g') = calculatePullback f g
          returnVals [VGraph (domain f'), VMorph f', VMorph g']
      )
    , ("calculatePushout", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let (f', g') = calculatePushout f g
          returnVals [VGraph (codomain f'), VMorph f', VMorph g']
      )
    , ("calculateEqualizer", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let e = calculateEqualizer f g
          returnVals [VGraph (domain e), VMorph e]
      )
    , ("calculateCoequalizer", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let e = calculateCoequalizer f g
          returnVals [VGraph (codomain e), VMorph e]
      )
    , ("hasPushoutComplementAlongM", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          return (hasPushoutComplementAlongM f g)
      )
    , ("calculatePushoutComplementAlongM", haskellFn2 globalState $ \idF idG -> do
          VMorph f <- lookupGrLangValue idF
          VMorph g <- lookupGrLangValue idG
          let (g', f') = calculatePushoutComplementAlongM f g
          returnVals [VGraph (codomain g'), VMorph g', VMorph f']
      )
    , ("calculateInitialPushout", haskellFn1 globalState $ \idx -> do
          VMorph f <- lookupGrLangValue idx
          let (b, f', c) = calculateMInitialPushout f
          returnVals [VGraph (domain b), VGraph (domain c), VMorph b, VMorph f', VMorph c]
      )
    , ("subobjectIntersection", haskellFn2 globalState $ \idA idB -> do
          VMorph a <- lookupGrLangValue idA
          VMorph b <- lookupGrLangValue idB
          let c = subobjectIntersection a b
          returnVals [VGraph (domain c), VMorph c]
      )
    , ("subobjectUnion", haskellFn2 globalState $ \idA idB -> do
          VMorph a <- lookupGrLangValue idA
          VMorph b <- lookupGrLangValue idB
          let c = subobjectUnion a b
          returnVals [VGraph (domain c), VMorph c]
      )
    , ("findJointSurjectionSquares", haskellFn4 globalState $ \kindStrF idF kindStrG idG -> do
        VMorph f <- lookupGrLangValue idF
        VMorph g <- lookupGrLangValue idG
        clsF <- morphClassFromString kindStrF
        clsG <- morphClassFromString kindStrG
        withMemSpace iterLists .
          allocateMemSpace . map (\(f', g') -> [VGraph (codomain f'), VMorph f', VMorph g']) $
            findJointSurjectionSquares (clsF, f) (clsG, g)
      )
    ]

  setNative "Cospan"
    [ ("findCospanCommuters", haskellFn3 globalState $ \kindStr idF idG -> do
        VMorph f <- lookupGrLangValue idF
        VMorph g <- lookupGrLangValue idG
        cls <- morphClassFromString kindStr
        withMemSpace iterLists .
          allocateMemSpace . map (\h -> [VMorph h]) $ findCospanCommuters cls f g
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
    ]
  where
    setNative className entries = do
      Lua.getglobal className
      tableIdx <- Lua.gettop
      createTable entries
      Lua.setfield tableIdx "native"
      Lua.pop 1

returnVals :: [Value] -> ExceptT GrLang.Error LuaGrLang Lua.NumResults
returnVals vals = do
  mapM_ (liftLua . Lua.push <=< allocateGrLang) vals
  return . fromIntegral $ length vals

morphClassFromString :: Lua.OrNil BS.ByteString -> ExceptT GrLang.Error LuaGrLang (MorphismClass GrMorphism)
morphClassFromString strOrNil = case Lua.toMaybe strOrNil of
  Nothing -> return anyMorphism
  Just "all" -> return anyMorphism
  Just "monic" -> return monic
  Just "epic" -> return epic
  Just "'iso" -> return iso
  Just kind -> GrLang.throwError Nothing $ "Invalid kind of morphism '" <> pretty (show kind) <> "'"

haskellFn0 :: ToHaskellFunction (Lua a) => GrLangState -> ExceptT GrLang.Error LuaGrLang a -> Lua ()
haskellFn0 globalState f = pushFunction (runGrLang' globalState f)

haskellFn1 ::
  (FromLuaStack a, ToHaskellFunction (Lua b)) =>
  GrLangState -> (a -> ExceptT GrLang.Error LuaGrLang b) -> Lua ()
haskellFn1 globalState f = pushFunction (runGrLang' globalState . f)

haskellFn2 ::
  (FromLuaStack a, FromLuaStack b, ToHaskellFunction (Lua c)) =>
  GrLangState -> (a -> b -> ExceptT GrLang.Error LuaGrLang c) -> Lua ()
haskellFn2 globalState f = pushFunction (\x y -> runGrLang' globalState (f x y))

haskellFn3 ::
  (FromLuaStack a, FromLuaStack b, FromLuaStack c, ToHaskellFunction (Lua d)) =>
  GrLangState -> (a -> b -> c -> ExceptT GrLang.Error LuaGrLang d) -> Lua ()
haskellFn3 globalState f = pushFunction (\x y z -> runGrLang' globalState (f x y z))

haskellFn4 ::
  (FromLuaStack a, FromLuaStack b, FromLuaStack c, FromLuaStack d, ToHaskellFunction (Lua e)) =>
  GrLangState -> (a -> b -> c -> d -> ExceptT GrLang.Error LuaGrLang e) -> Lua ()
haskellFn4 globalState f = pushFunction (\w x y z -> runGrLang' globalState (f w x y z))

createTable :: Foldable t => t (String, Lua ()) -> Lua ()
createTable contents = do
  Lua.createtable 0 (length contents)
  tableIdx <- Lua.gettop
  forM_ contents $ \(key, pushVal) -> do
    pushVal
    Lua.setfield tableIdx key

useGgxNamesOnTypeGraph :: Map String Text -> TypeGraph -> TypeGraph
useGgxNamesOnTypeGraph names graph = TypeGraph.fromNodesAndEdges
  [ Node n (makeInfo n) | Node n _ <- TypeGraph.nodes graph ]
  [ Edge e s t (makeInfo e) | Edge e s t _ <- TypeGraph.edges graph ]
  where makeInfo id = Just $ Metadata (Map.lookup ("I" ++ show id) names) Nothing

useGgxNames :: Map String Text -> Value -> Value
useGgxNames names (VGraph g) =
  VGraph . useGgxNamesOnGraph names $ g
useGgxNames names (VMorph f) =
  VMorph . useGgxNamesOnMorphism names $ f
useGgxNames names (VRule (Production l r ns)) =
  VRule $ Production (useGgxNamesOnMorphism names l) (useGgxNamesOnMorphism names r) (map (useGgxNamesOnMorphism names) ns)

useGgxNamesOnGraph :: Map String Text -> GrGraph -> GrGraph
useGgxNamesOnGraph names graph =
  TGraph.fromNodesAndEdges (TGraph.typeGraph graph)
    [ (Node n (makeInfo n), nodeId ntype) | (Node n _, ntype, _) <- TGraph.nodesInContext graph ]
    [ (Edge e s t (makeInfo e), edgeId etype) | (_, Edge e s t _, etype, _) <- TGraph.edgesInContext graph ]
  where makeInfo elemId = Just $ Metadata (Map.lookup ("I" ++ show elemId) names) Nothing

useGgxNamesOnMorphism :: Map String Text -> GrMorphism -> GrMorphism
useGgxNamesOnMorphism names (TGraph.TypedGraphMorphism dom cod morph) =
  TGraph.TypedGraphMorphism (useGgxNamesOnGraph names dom) (useGgxNamesOnGraph names cod) morph
