import           Abstract.Morphism
import           Abstract.Valid
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Control.Monad (when, forM_)
import qualified Data.List as L
import           Data.Matrix hiding ((<|>))
import           Graph.ConcurrentRules
import qualified Graph.GraphGrammar as GG
import qualified Graph.GraphMorphism as GM
import           Graph.GraphRule
import           Options.Applicative
import qualified Text.XML.HXT.Core as HXT
import qualified XML.GGXReader as XML
import qualified XML.GGXWriter as GW

data VerigraphOpts = Opts
  { inputFile :: String
  , outputFile :: Maybe String
  , injectiveMatchesOnly :: Bool
  , injectiveNacSatisfaction :: Bool
  , analysis :: Analysis
  , verbose :: Bool }

data Analysis = Both | Conflicts | Dependencies | None deriving (Eq)

verigraphOpts :: Parser VerigraphOpts
verigraphOpts = Opts
  <$> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
  <*> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> flag False True
    ( long "inj-matches-only"
    <> help "Restrict the analysis to injective matches only")
  <*> flag False True
    ( long "inj-nac-satisfaction"
    <> help ("Restrict the analysis of NAC satisfaction to injective " ++
            "morphisms between the NAC graph and the instance graph"))
  <*> (flag' Conflicts
    ( long "conflicts-only"
    <> help "Restrict to Critical Pair analysis")
      <|> flag' Dependencies
        ( long "dependencies-only"
        <> help "Restrict to Critical Sequence analysis")
        <|> pure Both)
  <*> flag False True
    ( long "verbose"
    <> short 'v'
    <> help "Print detailed information")

calculateConflicts :: Analysis -> Bool
calculateConflicts flag = flag `elem` [Both,Conflicts]

calculateDependencies :: Analysis -> Bool
calculateDependencies flag = flag `elem` [Both,Dependencies]

execute :: VerigraphOpts -> IO ()
execute opts = do
    gg <- XML.readGrammar (inputFile opts)
    ggName <- XML.readGGName (inputFile opts)
    names <- XML.readNames (inputFile opts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction opts
        onlyInj = injectiveMatchesOnly opts
        action = analysis opts
        writer = defWriterFun nacInj onlyInj action
        rules = map snd (GG.rules gg)
        puMatrix = pairwiseCompare (allProduceUse nacInj onlyInj) rules
        ddMatrix = pairwiseCompare (allDeliverDelete nacInj onlyInj) rules
        udMatrix = pairwiseCompare (allDeleteUse nacInj onlyInj) rules
        pfMatrix = pairwiseCompare (allProduceForbid nacInj onlyInj) rules
        peMatrix = pairwiseCompare (allProdEdgeDelNode nacInj onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix
        dependenciesMatrix = liftMatrix2 (\x y -> x ++ y) puMatrix ddMatrix

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

defWriterFun :: Bool -> Bool -> Analysis
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

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> verigraphOpts)
      ( fullDesc
      <> progDesc "Run critical pair analysis on a given graph grammar.")
