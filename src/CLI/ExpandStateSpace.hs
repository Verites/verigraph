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
import qualified Data.List as List
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
  , drawTo :: Maybe FilePath
  }


options :: Parser Options
options = Options
  <$> option auto
    ( long "max-depth" <> short 'd'
    <> help "Distance from the start graph after which the search stops.")
  <*> optional (strOption $
        long "draw-output-dir" <> short 'o'
        <> help "Create .dot files for the state space in the given directory.")


allOptions :: Parser (GlobalOptions, Options)
allOptions =
  (,)
  <$> globalOpts
  <*> options


execute :: (GlobalOptions, Options) -> IO ()
execute (globalOpts, options) =
  do
    grammar <- XML.readGrammar (inputFile globalOpts)
    ensureAllValid (rules grammar) $ \name -> "Invalid rule '" ++ name ++ "'"

    graphs <- XML.readGraphs (inputFile globalOpts)
    ensureAllValid graphs $ \name -> "Invalid graph '" ++ name ++ "'"

    let stateSpace = exploreStateSpace (dpoConfig globalOpts) (maxDepth options) grammar graphs

    names <- XML.readNames (inputFile globalOpts)
    let namingContext = makeNamingContext names

    case drawTo options of
      Nothing ->
        return ()

      Just dir ->
        drawStateSpace dir namingContext stateSpace

    return ()


-- | Tests if all given graphs are valid. If one isn't, print a message and exit.
ensureAllValid :: Valid a => [(String, a)] -> (String -> String) -> IO ()
ensureAllValid items errorMessage =
  let
    invalid =
      filter (not . valid . snd) items
  in
    unless (List.null invalid) $ do
      mapM_ (putStrLn . errorMessage . fst) invalid
      exitFailure


-- | Creates .dot files for the given state space in the given directory.
--
-- Uses the naming context for labeling nodes and edges of instance graphs with their types.
drawStateSpace :: FilePath -> Dot.NamingContext -> StateSpace (TypedGraphMorphism a b) -> IO ()
drawStateSpace dir namingContext stateSpace =
  do
    withFile (fileFor "stateSpace") WriteMode $ \file ->
      hPutDoc file (Dot.printStateSpace stateSpace)

    forM_ (IntMap.toList $ states stateSpace) $ \(idx, (graph, _)) -> do
      let name = show idx
      withFile (fileFor name) WriteMode $ \file ->
        hPutDoc file (Dot.printTypedGraph namingContext name graph)
  where
    fileFor name = dir ++ "/" ++ name ++ ".dot"


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
      StateSpace.empty config (map snd productions) predicates
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
