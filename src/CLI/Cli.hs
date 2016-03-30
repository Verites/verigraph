import Control.Monad (when, forM_)
import Data.Matrix hiding ((<|>))
import qualified Data.List as L
import Options.Applicative
import Abstract.Valid

import Graph.GraphRule (GraphRule)
import CriticalPairs.CriticalPairs (CriticalPair)
import qualified CriticalPairs.CriticalPairs as CP
import qualified CriticalPairs.CriticalSequence as CS
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
  , injectiveNacSatisfaction :: Bool
  , analysis :: Analysis
  , verbose :: Bool }

data Analysis = Both | Conflicts | Dependencies deriving (Eq)

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

calculateConflicts flag = flag `elem` [Both,Conflicts]
calculateDependencies flag = flag `elem` [Both,Dependencies]

execute :: VerigraphOpts -> IO ()
execute opts = do
    gg <- readGrammar opts
    names <- getNames (inputFile opts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction opts
        onlyInj = injectiveMatchesOnly opts
        calc = analysis opts
        writer = case calc of
                   Conflicts    -> GW.writeConflictsFile
                   Dependencies -> GW.writeDependenciesFile
                   Both         -> GW.writeConfDepFile
        rules = map snd (GG.rules gg)
        puMatrix = pairwiseCompare (CS.allProduceUse nacInj onlyInj) rules
        ddMatrix = pairwiseCompare (CS.allDeliverDelete nacInj onlyInj) rules
        udMatrix = pairwiseCompare (CP.allDeleteUse nacInj onlyInj) rules
        pfMatrix = pairwiseCompare (CP.allProduceForbid nacInj onlyInj) rules
        peMatrix = pairwiseCompare (CP.allProdEdgeDelNode nacInj onlyInj) rules
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
      Just file -> writer nacInj onlyInj gg names file
      Nothing -> mapM_
                 putStrLn $
                 (if calculateConflicts calc then conflicts else [])
                 ++ (if calculateDependencies calc then dependencies else [])
                 ++ ["Done!"]

getNames :: String -> IO [(String,String)]
getNames fileName = do
  names <- XML.readNames fileName
  nacNames <- XML.readNacNames fileName
  return (head names ++ concat nacNames)

readGrammar :: VerigraphOpts -> IO (GG.GraphGrammar a b)
readGrammar conf = do
  let fileName = inputFile conf
  
  parsedTypeGraphs <- XML.readTypeGraph fileName
  let parsedTypeGraph = case parsedTypeGraphs of
                         []    -> error "error, type graph not found"
                         ptg:_ -> ptg
  _ <- parsedTypeGraph `seq` return ()
  
  parsedRules <- XML.readRules fileName

  let rulesNames = map (\((x,_,_,_),_) -> x) parsedRules
  when (verbose conf) $ do
    putStrLn "\nRules:"
    forM_ rulesNames $ \name ->
      putStrLn ('\t' : name)
    putStrLn ""
  
  let rules = map (XML.instantiateRule parsedTypeGraph) parsedRules
  
  _ <- (case False `elem` (map valid rules) of
          True  -> error "some rule is not valid"
          False -> []) `seq` return ()       
  
  let typeGraph = M.codomain . M.domain . GR.left $ head rules
      initGraph = GM.empty typeGraph typeGraph
  
  return $ GG.graphGrammar initGraph (zip rulesNames rules)

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
