{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where


import           Control.Monad
import           Control.Monad.List
import           Control.Monad.Reader
import           Data.Maybe                 (fromMaybe, isJust)
-- import Control.Monad.IO.Class
import           Control.DeepSeq
import           Data.IORef
import qualified Data.List                  as List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Proxy
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           System.CPUTime
import           System.Directory
import           System.Environment         as Env
import           System.FilePath            as FilePath
import           System.IO
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.List.NonEmpty as NonEmpty


import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.Finitary
import           Abstract.Category.FindMorphism
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           GrLang.Value
import           Util.Monad
import Base.Location (Position, Location)
import Data.Relation (Relation)
import Data.Graphs (Graph, Node, NodeId, Edge, EdgeId)
import           Data.Graphs.Morphism (GraphMorphism)
import Data.TypedGraph.Morphism (TypedGraphMorphism)
import           XML.GGXReader              as GGX
import qualified Dag

instance NFData NodeId
instance NFData EdgeId
instance NFData a => NFData (Relation a)
instance (NFData n) => NFData (Node n)
instance (NFData e) => NFData (Edge e)
instance (NFData n, NFData e) => NFData (Graph n e)
instance (NFData n, NFData e) => NFData (GraphMorphism n e)
instance (NFData n, NFData e) => NFData (TypedGraphMorphism n e)
instance NFData Metadata
instance NFData Location
instance NFData Position


main :: IO ()
main = do
  grammarDir <- fromMaybe "grammars" <$> Env.lookupEnv "GRAMMAR_DIR"
  outputDir <- fromMaybe "output" <$> Env.lookupEnv "OUTPUT_DIR"

  grammars <- List.sortOn fst <$> loadGrammars grammarDir

  runExpM grammars outputDir conflictExp runExperiment
  runExpM grammars outputDir disablingExp runExperiment


loadGrammars :: FilePath -> IO [(String, Grammar GrMorphism)]
loadGrammars dir = do
  grammarFiles <- filter (".ggx" `List.isSuffixOf`) <$> listDirectory dir
  forM grammarFiles $ \fileName -> do
    (grammar, _, _) <- GGX.readGrammar (dir </> fileName) False morphConf
    let grammarName = FilePath.takeBaseName fileName
    return (grammarName, grammar)

data Config = Conf
  { confGrammars :: [(String, Grammar GrMorphism)]
  , hasStarted   :: Bool
  , checkedPairs :: Set (String, String)
  , experiment   :: Experiment
  , outFile      :: Handle
  , logFile      :: Handle
  , totalPairs   :: Int
  , currPairs    :: IORef Int
  , currResults  :: IORef (Map String Integer)
  }

data Experiment = Exp
  { expName          :: String
  , outColumns       :: [String]
  , pairsForGrammar  :: Grammar GrMorphism -> Int
  , shouldCheckRules :: String -> String -> Bool
  , checkRules       :: GrRule -> GrRule -> ListT ExpM (Span GrMorphism)
  }

shouldCheckRules' :: String -> String -> Experiment -> Bool
shouldCheckRules' n1 n2 exp = shouldCheckRules exp n1 n2

type ExpM = ReaderT Config IO

incrResult :: String -> Integer -> ExpM ()
incrResult name val = do
  results <- asks currResults
  liftIO $ modifyIORef results (Map.insertWith (+) name val)

runExpM :: [(String, Grammar GrMorphism)] -> String -> Experiment -> ExpM a -> IO a
runExpM grammars outputDir experiment action = do
  let outFile = outputDir </> expName experiment <.> "csv"
      logFile = outputDir </> expName experiment <.> "log"
  (started, checked) <- loadPrevResults outFile (outColumns experiment)

  let totalPairs = sum $ map (pairsForGrammar experiment . snd) grammars
  currPairs <- newIORef 0
  currResults <- newIORef Map.empty

  withFile outFile AppendMode $ \outFile ->
    withFile logFile AppendMode $ \logFile ->
      let conf = Conf grammars started checked experiment outFile logFile (totalPairs - Set.size checked) currPairs currResults
      in runReaderT action conf

loadPrevResults :: Foldable t => FilePath -> t a -> IO (Bool, Set (String, String))
loadPrevResults filePath cols = do
  createDirectoryIfMissing True (FilePath.takeDirectory filePath)
  exists <- doesFileExist filePath
  if exists then
    withFile filePath ReadMode $ \file -> do
      lines <- tail . lines <$> hGetContents file -- skip header
      let addLine fields checked
            | length fields == length cols + 3 = Set.insert (fields !! 1, fields !! 2) checked
            | otherwise = checked
      let checked = foldr (addLine . splitFields) Set.empty lines
      checked `deepseq` return (True, checked)
  else
    return (False, Set.empty)

splitFields :: String -> [String]
splitFields string = case break (==',') string of
  (x,[]) -> [x]
  (x,_:xs) -> x : splitFields xs

withHandle :: (MonadReader r m, MonadIO m) => (r -> a) -> (a -> IO b) -> m b
withHandle file action = liftIO . action =<< asks file

writeLine, write :: (MonadIO m, MonadReader r m) => (r -> Handle) -> String -> m ()
writeLine file line = withHandle file $ \f -> hPutStrLn f line >> hFlush f
write file str = withHandle file $ \f -> hPutStr f str >> hFlush f

runExperiment :: ExpM ()
runExperiment = do
  started <- asks hasStarted
  unless started $ do -- write header
    cols <- asks (outColumns . experiment)
    writeLine outFile . List.intercalate "," $
      "grammar" : "rule1" : "rule2" : cols

  grammars <- asks confGrammars
  forM_ grammars $ \(grammarName, grammar) -> do
    writeLine logFile $ "Testing grammar " ++ grammarName
    let rules = List.sortOn fst $ productions grammar
    forM_ rules $ \(name1, rule1) ->
      forM_ rules $ \(name2, rule2) -> do
        check <- asks (shouldCheckRules' name1 name2 . experiment)
        when check $ do
          prechecked <- asks (Set.member (name1, name2) . checkedPairs)
          if prechecked then
            writeLine logFile $ printf "\t%s %s [prev run]" name1 name2
          else do
            total <- asks totalPairs
            liftIO . (`modifyIORef` (+1)) =<< asks currPairs
            curr <- liftIO . readIORef =<< asks currPairs
            writeLine logFile $ printf "\t%s %s (%d/%d)" name1 name2 curr total
            liftIO . (`writeIORef` Map.empty) =<< asks currResults

            check <- asks (checkRules . experiment)
            let evalSpan x = force x `seq` x
            essences <- recordingTime "time_enum" . fmap evalSpan . runListT $ check rule1 rule2
            recordingTime "time_filter" $ checkIrred essences

            results <- liftIO . readIORef =<< asks currResults
            cols <- asks (outColumns . experiment)
            writeLine outFile . List.intercalate "," $
              grammarName : name1 : name2 : map (show . (\col -> Map.findWithDefault 0 col results)) cols

checkIrred :: [Span GrMorphism] -> ExpM ()
checkIrred essences = do 
  poset <- liftIO $ Dag.empty (length essences)
  mapM_ (\x -> liftIO $ Dag.insertPoset cmp x poset) essences

  cover <- liftIO $ Dag.lowerCover poset
  mapM_ checkIrred =<< liftIO (Dag.elemsWithAdj cover)
  where
    cmp a b = case findEssenceMorphism monic a b of
      Just f
        | isIsomorphism f -> Just EQ
        | otherwise -> Just LT
      Nothing -> case findEssenceMorphism monic b a of
        Just f -> Just GT
        Nothing -> Nothing

    checkIrred (x, ys)
      | length ys <= 1 = incrResult "irred_essences" 1
      | x `equalsUnionOf` ys = return ()
      | otherwise = incrResult "irred_essences" 1 >> incrResult "irred_large_cover" 1

    equalsUnionOf x ys = case unions ys of
      Nothing -> False
      Just u -> isJust $ findEssenceMorphism iso x u

    unions (x:xs) = foldr (\x y -> pairSubobjectUnion x =<< y) (Just x) xs

findEssenceMorphism cls (a1, a2) (b1, b2) =
  case filter (\h -> b2 <&> h == a2) $ findCospanCommuters cls a1 b1 of
    [] -> Nothing
    [h] -> Just h
    _ -> error "findEssenceMorphism: found multiple morphisms, which should be impossible"

conflictExp :: Experiment
conflictExp = Exp
  { expName = "conflict-stats"
  , outColumns = words "overlappings applicable critical_pairs conflict_essences irred_essences irred_large_cover time_enum time_filter"
  , pairsForGrammar = \grammar -> let n = length (productions grammar) in n * (n+1) `div` 2
  , shouldCheckRules = (<=)
  , checkRules = \rule1 rule2 -> do
      (m1, m2) <- pickFromList $ findJointSurjections (monic, leftObject rule1) (monic, leftObject rule2)
      lift $ incrResult "overlappings" 1

      guard (satisfiesGluingConditions morphConf rule1 m1 && satisfiesGluingConditions morphConf rule2 m2)
      lift $ incrResult "applicable" 1

      let (p2, p1) = calculatePullback m1 m2
      let (q12, _) = calculatePullback (leftMorphism rule1) p1
      let (q21, _) = calculatePullback (leftMorphism rule2) p2
      let (_, _, c1) = calculateMInitialPushout q12
      let (_, _, c2) = calculateMInitialPushout q21
      let c = subobjectUnion c1 c2
      guard . not $ isInitial (Proxy @GrMorphism) (domain c)
      lift $ incrResult "critical_pairs" 1

      guard (isIsomorphism c)
      lift $ incrResult "conflict_essences" 1
      return (p1 <&> c, p2 <&> c)
  }

disablingExp :: Experiment
disablingExp = Exp
  { expName = "disabling-stats"
  , outColumns = words "upper_overlappings lower_overlappings upper_applicable disabling_candidates disabling_essences irred_essences irred_large_cover time_enum time_filter"
  , pairsForGrammar = \grammar -> let n = length (productions grammar) in n * n
  , shouldCheckRules = \_ _ -> True
  , checkRules = \rule1 rule2 -> do
      let (_,l1',cl1) = calculateMInitialPushout (leftMorphism rule1)
          rule1' = Production l1' (identity $ domain l1') []

      (m1', m2') <- pickFromList $ findJointSurjections (monic, leftObject rule1') (monic, leftObject rule2)
      lift $ incrResult "upper_overlappings" 1

      guard (satisfiesGluingConditions morphConf rule1' m1')
      guard (satisfiesGluingConditions morphConf rule2  m2')
      lift $ incrResult "upper_applicable" 1

      let (p2, p1) = calculatePullback m1' m2'
      let (q12, _) = calculatePullback l1' p1
      let (_, _, c) = calculateMInitialPushout q12
      guard . not $ isInitial (Proxy @GrMorphism) (domain c)
      guard (isIsomorphism c)
      lift $ incrResult "disabling_candidates" 1
      let (e1, e2) = (cl1 <&> p1 <&> c, p2 <&> c)

      let allLowerMatches = findJointSurjectionSquares (monic, e1) (monic, e2)
          isApplicable (m2, m1) = satisfiesGluingConditions morphConf rule1 m1 && satisfiesGluingConditions morphConf rule2 m2

      case break isApplicable allLowerMatches of
        (nonApplicable, []) -> do
          lift . incrResult "lower_overlappings" . fromIntegral $ length nonApplicable
          mzero
        (nonApplicable, _:_) -> do
          lift . incrResult "lower_overlappings" . fromIntegral $ length nonApplicable + 1
          lift $ incrResult "disabling_essences" 1
          return (e1, e2)
  }

recordingTime :: String -> ExpM a -> ExpM a
recordingTime fieldName action = do
  initTime <- lift getCPUTime
  result <- action
  endTime <- lift getCPUTime
  incrResult fieldName (endTime - initTime)
  return result

morphConf :: MorphismsConfig GrMorphism
morphConf = MorphismsConfig monic

