module Main (main) where

import           Options.Applicative
import           GlobalOptions

import           Abstract.Valid
import           Abstract.Morphism
import           Abstract.DPO
import           Abstract.DPO.StateSpace as StateSpace
import           TypedGraph.Graph
import           TypedGraph.Morphism
import           TypedGraph.GraphRule
import           TypedGraph.GraphGrammar
import qualified Dot
import qualified XML.GGXReader             as XML

import           Control.Monad
import qualified Data.IntMap as IntMap
import           System.IO
import           System.Exit
import           Text.PrettyPrint.Leijen (hPutDoc)


main :: IO ()
main =
  execParser opts >>= execute

  where
    opts =
      info (helper <*> allOptions)
           ( fullDesc
           <> progDesc "Expands the state space induced by the given graph grammar. Explores from all graphs defined in the given grammar file.")


data Options = Options
  { maxDepth :: Int
  , outDir :: Maybe FilePath
  }


options :: Parser Options
options = Options
  <$> option auto
    ( long "max-depth" <> short 'd'
    <> help "Distance from the start graph after which the search stops.")
  <*> optional (strOption $
        long "output-dir" <> short 'o'
        <> help "Directory where the output files should be written")


allOptions :: Parser (GlobalOptions, Options)
allOptions =
  (,)
  <$> globalOpts
  <*> options


execute :: (GlobalOptions, Options) -> IO ()
execute (globalOpts, options) =
  do
    grammar <- XML.readGrammar (inputFile globalOpts)
    graphs <- XML.readGraphs (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    forM_ graphs $ \(name, graph) ->
      unless (valid graph) $ do
        putStrLn ("Invalid graph '" ++ name ++ "'")
        exitFailure

    forM_ (rules grammar) $ \(name, rule) ->
      unless (valid rule) $ do
        putStrLn ("Invalid rule '" ++ name ++ "'")
        exitFailure


    let stateSpace = exploreStateSpace (dpoConfig globalOpts) (maxDepth options) grammar graphs
    let exploredStates = IntMap.toList (states stateSpace)

    let namingContext = makeNamingContext names
    let fileFor graphName =
          case outDir options of
            Nothing -> graphName ++ ".dot"
            Just dir -> dir ++ "/" ++ graphName ++ ".dot"

    withFile (fileFor "stateSpace") WriteMode $ \file ->
      hPutDoc file (Dot.printStateSpace stateSpace)

    forM_ exploredStates $ \(id, graph) -> do
      let name = show id
      withFile (fileFor name) WriteMode $ \file ->
        hPutDoc file (Dot.printTypedGraph namingContext name graph)

    return ()


makeNamingContext :: [(String, String)] -> Dot.NamingContext
makeNamingContext assocList =
  let
    normalizeId id =
      "I" ++ show id

    nameForId id =
      case lookup id assocList of
        Nothing ->
          error $ "Name for '" ++ id ++ "' not found."

        Just name ->
          takeWhile (/= '%') name
  in
    Dot.Ctx (nameForId . normalizeId) (nameForId . normalizeId)


type NamedProduction = (String, GraphRule () ())

type NamedPredicate = (String, GraphRule () ())

exploreStateSpace :: DPOConfig -> Int -> GraphGrammar () () -> [(String, TypedGraph () ())] -> StateSpace (TypedGraphMorphism () ())
exploreStateSpace config maxDepth grammar graphs =
  let
    (productions, predicates) =
      splitPredicates (rules grammar)

    search =
      mapM_ (depthSearch maxDepth . snd) graphs

    initialSpace =
      StateSpace.empty config (map snd productions)
  in
    execStateSpaceBuilder search initialSpace

-- | Separates the rules that change nothing (which are considered predicates)
-- from those that have some effect (which are considered productions).
splitPredicates :: [(String, GraphRule () ())] -> ([NamedProduction], [NamedPredicate])
splitPredicates [] =
  ([], [])

splitPredicates ((name, rule) : rest) =
  let
    (productions, predicates) =
      splitPredicates rest
  in
    if isomorphism (left rule) && isomorphism (right rule) then
      (productions, (name, rule):predicates)
    else
      ((name, rule):productions, predicates)
