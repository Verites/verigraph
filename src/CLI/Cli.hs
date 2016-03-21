import Control.Monad (when, forM_)
import Data.Matrix
import qualified Data.List as L
import Options.Applicative

import Graph.GraphRule (GraphRule)
import CriticalPairs.CriticalPairs (CriticalPair)
import qualified CriticalPairs.CriticalPairs as CP
import qualified XML.GGXReader as XML
import qualified Graph.GraphGrammar as GG
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphRule as GR
import qualified Abstract.Morphism as M
import qualified Text.XML.HXT.Core as HXT
import qualified XML.GGXWriter as GW

data VerigraphOpts = Opts
  { inputFile :: String
  , outputFile :: Maybe String
  , injectiveMatchesOnly :: Bool
  , verbose :: Bool }

verigraphOpts :: Parser VerigraphOpts
verigraphOpts = Opts
  <$> strArgument
    ( metavar "INPUT_FILE"
    <> help "GGX file defining the graph grammar")
  <*> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> help ("CPX file that will receive the critical pairs of the grammar " ++
             "(if absent, the numbers of conflicts will be printed to stdout)")))
  <*> flag False True
    ( long "injective-matches-only"
    <> help "Restrict the analysis to injective matches only")
  <*> flag False True
    ( long "verbose"
    <> short 'v'
    <> help "Print detailed information")

execute :: VerigraphOpts -> IO ()
execute opts = do
    gg <- readGrammar opts
    names <- getNames (inputFile opts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let onlyInj = injectiveMatchesOnly opts
        rules = map snd (GG.rules gg)
        udMatrix = pairwiseCompare (CP.allDeleteUse onlyInj) rules
        pfMatrix = pairwiseCompare (CP.allProduceForbid onlyInj) rules
        peMatrix = pairwiseCompare (CP.allProdEdgeDelNode onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix

    case outputFile opts of
      Just file -> GW.writeCpxFile onlyInj gg names file
      Nothing -> mapM_ putStrLn
        [ "Delete-Use:"
        , show (length <$> udMatrix)
        , ""
        , "Produce-Forbid:"
        , show (length <$> pfMatrix)
        , ""
        , "Produce Edge Delete Node:"
        , show (length <$> peMatrix)
        , "All Conflicts:"
        , show (length <$> conflictsMatrix)
        , ""
        , "Done!"
        ]


getNames :: String -> IO [(String,String)]
getNames fileName = do
  names <- XML.readNames fileName
  nacNames <- XML.readNacNames fileName
  return (head names ++ concat nacNames)

readGrammar :: VerigraphOpts -> IO (GG.GraphGrammar a b)
readGrammar conf = do
  let fileName = inputFile conf
  parsedTypeGraph <- XML.readTypeGraph fileName
  parsedRules <- XML.readRules fileName

  let rulesNames = map (\((x,_,_,_),_) -> x) parsedRules
  when (verbose conf) $ do
    putStrLn "\nRules:"
    forM_ rulesNames $ \name ->
      putStrLn ('\t' : name)
    putStrLn ""

  let rules = map (XML.instantiateRule (head parsedTypeGraph)) parsedRules
  let typeGraph = M.codomain . M.domain . GR.left $ head rules

  let initGraph = GM.empty typeGraph typeGraph
  return $ GG.graphGrammar initGraph (zip rulesNames rules)

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) ->
    compare (items !! (i-1)) (items !! (j-1))

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> verigraphOpts)
      ( fullDesc
      <> progDesc "Run critical pair analysis on a given graph grammar")
