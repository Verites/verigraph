module CLI.CriticalPairAnalysis
  ( CPOpts
  , cpOpts
  , runCPAnalysis
  ) where

import           CLI.GlobalOptions

import           Abstract.Morphism
import           Abstract.Valid
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Control.Monad             (forM_, when)
import qualified Data.List                 as L
import           Data.Matrix               hiding ((<|>))
import           Graph.ConcurrentRules
import qualified Graph.GraphGrammar        as GG
import qualified Graph.GraphMorphism       as GM
import           Graph.GraphRule
import           Options.Applicative
import qualified Text.XML.HXT.Core         as HXT
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data CPOpts = CPOpts
  { outputFile   :: Maybe String
  , analysisType :: CPAnalysisType
  }

data CPAnalysisType = Both | Conflicts | Dependencies | None deriving (Eq)

cpOpts :: Parser CPOpts
cpOpts = CPOpts
  <$> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> cpAnalysisType

cpAnalysisType :: Parser CPAnalysisType
cpAnalysisType =
      flag' Conflicts
        ( long "conflicts-only"
          <> help "Restrict to Critical Pair analysis")
  <|> flag' Dependencies
        ( long "dependencies-only"
          <> help "Restrict to Critical Sequence analysis")
  <|> pure Both


calculateConflicts :: CPAnalysisType -> Bool
calculateConflicts flag = flag `elem` [Both,Conflicts]

calculateDependencies :: CPAnalysisType -> Bool
calculateDependencies flag = flag `elem` [Both,Dependencies]

runCPAnalysis :: GlobalOptions -> CPOpts -> IO ()
runCPAnalysis globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = injectiveMatchesOnly globalOpts
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

defWriterFun :: Bool -> Bool -> CPAnalysisType
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
