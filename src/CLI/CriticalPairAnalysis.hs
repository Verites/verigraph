module CLI.CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Data.Matrix               hiding ((<|>))
import qualified Graph.GraphGrammar        as GG
import           Options.Applicative
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data Options = Options
  { outputFile   :: Maybe String
  , analysisType :: AnalysisType
  }

data AnalysisType = Both | Conflicts | Dependencies | None deriving (Eq)

options :: Parser Options
options = Options
  <$> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> cpAnalysisType

cpAnalysisType :: Parser AnalysisType
cpAnalysisType =
      flag' Conflicts
        ( long "conflicts-only"
          <> help "Restrict to Critical Pair analysis")
  <|> flag' Dependencies
        ( long "dependencies-only"
          <> help "Restrict to Critical Sequence analysis")
  <|> pure Both


calculateConflicts :: AnalysisType -> Bool
calculateConflicts flag = flag `elem` [Both,Conflicts]

calculateDependencies :: AnalysisType -> Bool
calculateDependencies flag = flag `elem` [Both,Dependencies]

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = not $ injectiveMatchesOnly globalOpts
        action = analysisType opts
        writer = defWriterFun nacInj onlyInj action
        rules = map snd (GG.rules gg)
        puMatrix = pairwiseCompare (allProduceUse nacInj onlyInj) rules
        ddMatrix = pairwiseCompare (allDeliverDelete nacInj onlyInj) rules
        udMatrix = pairwiseCompare (allDeleteUse nacInj onlyInj) rules
        pfMatrix = pairwiseCompare (allProduceForbid nacInj onlyInj) rules
        peMatrix = pairwiseCompare (allProdEdgeDelNode nacInj onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix
        dependenciesMatrix = liftMatrix2 (++) puMatrix ddMatrix

        conflicts = [ "Delete-Use:"
                , show (length <$> udMatrix)
                , ""
                , "Produce-Forbid:"
                , show (length <$> pfMatrix)
                , ""
                , "Produce Edge Delete Node:"
                , show (length <$> peMatrix)
                , "All Conflicts:"
                , show (length <$> conflictsMatrix)
                , ""]

        dependencies = [ "Produce Use Dependency:"
                   , show (length <$> puMatrix)
                   , ""
                   , "Deliver Delete Dependency:"
                   , show (length <$> ddMatrix)
                   , ""
                   , "All Dependencies:"
                   , show (length <$> dependenciesMatrix)
                   , ""]

    case outputFile opts of
      Just file -> writer gg ggName names file
      Nothing -> mapM_
                 putStrLn $
                 (if calculateConflicts action then conflicts else [])
                 ++ (if calculateDependencies action then dependencies else [])
                 ++ ["Done!"]
    
    --print $ head (GG.sndOrderRules gg)

defWriterFun :: Bool -> Bool -> AnalysisType
             ->(GG.GraphGrammar a b -> String
             -> [(String,String)] -> String -> IO ())
defWriterFun nacInj inj t = case t of
                   Conflicts    -> GW.writeConflictsFile nacInj inj
                   Dependencies -> GW.writeDependenciesFile nacInj inj
                   Both         -> GW.writeConfDepFile nacInj inj
                   None         -> GW.writeGrammarFile

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

-- | Combine two matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix2 :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
liftMatrix2 f ma mb = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) ->
    compare (items !! (i-1)) (items !! (j-1))
