module Main (main) where

import           Control.Monad
import qualified Data.IntMap                           as IntMap
import qualified Data.List                             as List
import           Data.Monoid                           ((<>))
import           Data.Text.Prettyprint.Doc             (Pretty (..))
import qualified Data.Text.Prettyprint.Doc             as PP
import           Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import           Options.Applicative
import           System.Exit
import           System.IO

import           Abstract.Category
import           Abstract.Rewriting.DPO                as DPO hiding (NamedProduction)
import           Abstract.Rewriting.DPO.StateSpace     as StateSpace
import           Base.Valid
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           GlobalOptions
import qualified Image.Dot.StateSpace                  as Dot
import qualified Image.Dot.TypedGraph                  as Dot
import qualified Logic.Ctl                             as Logic
import qualified Logic.Model                           as Logic
import           Rewriting.DPO.TypedGraph

import qualified XML.GGXReader                         as XML


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
  , drawTo   :: Maybe FilePath
  , formula  :: Maybe String
  }


options :: Parser Options
options = Options
  <$> option auto
    ( long "max-depth" <> short 'd'
    <> help "Distance from the start graph after which the search stops.")
  <*> optional (strOption $
        long "draw-output-dir" <> short 'o'
        <> help "Create .dot files for the state space in the given directory.")
  <*> optional (strArgument $
        metavar "CTL_FORMULA"
        <> help "CTL formula to be checked")

allOptions :: Parser (GlobalOptions, Options)
allOptions =
  (,)
  <$> globalOpts
  <*> options


execute :: (GlobalOptions, Options) -> IO ()
execute (globalOpts, options) =
  do
    let dpoConf = morphismsConf globalOpts

    (grammar,_,_) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (DPO.productions grammar)

    graphs <- XML.readGraphs (inputFile globalOpts)
    ensureValid $ validateNamed (\name -> "Graph '"++name++"'") graphs

    let
      (initialStates, stateSpace) =
        exploreStateSpace (morphismsConf globalOpts) (maxDepth options) grammar graphs

    case formula options of
      Nothing ->
        return ()

      Just exprText -> do
        expr <- parseExpressionOrExit exprText
        modelCheck (StateSpace.toKripkeStructure stateSpace) expr initialStates

    names <- XML.readNames (inputFile globalOpts)
    let namingContext = Dot.makeNamingContext names

    case drawTo options of
      Nothing ->
        return ()

      Just dir ->
        drawStateSpace dir namingContext stateSpace

    return ()


parseExpressionOrExit :: String -> IO Logic.Expr
parseExpressionOrExit text =
  case Logic.parseExpr "" text of
    Left err -> do
      putStrLn "Invalid CTL formula:"
      print err
      exitFailure

    Right expr ->
      return expr


modelCheck :: Logic.KripkeStructure String -> Logic.Expr -> [Int] -> IO ()
modelCheck model expr initialStates =
  let
    allGoodStates =
      Logic.satisfyExpr' model expr

    (goodStates, badStates) =
      List.partition (`List.elem` allGoodStates) initialStates

    explainStates states msgIfEmpty msgIfNonEmpty =
      if List.null states then
        putStrLn msgIfEmpty
      else
        putStrLn msgIfNonEmpty >> mapM_ showState states

    showState index =
      putStrLn $ "\t" ++ show index
  in do
    explainStates
      goodStates
      "No initial states satisfy the formula!"
      "The following initial states satisfy the formula:"

    putStrLn ""

    explainStates
      badStates
      "All initial states satisfy the formula!"
      "The following initial states do NOT satisfy the formula:"


-- | Creates .dot files for the given state space in the given directory.
--
-- Uses the naming context for labeling nodes and edges of instance graphs with their types.
drawStateSpace :: FilePath -> Dot.NamingContext a b ann -> StateSpace (TypedGraphMorphism a b) -> IO ()
drawStateSpace dir namingContext stateSpace =
  do
    withFile (fileFor "stateSpace") WriteMode $ \file ->
      renderIO file . layout $ Dot.stateSpace stateSpace

    forM_ (IntMap.toList $ states stateSpace) $ \(idx, (graph, _)) -> do
      let name = show idx
      withFile (fileFor name) WriteMode $ \file ->
        renderIO file . layout $ Dot.typedGraph namingContext (pretty name) graph
  where
    fileFor name = dir ++ "/" ++ name ++ ".dot"
    layout = PP.layoutSmart PP.defaultLayoutOptions

type NamedProduction = (String, TypedGraphRule () ())

type NamedPredicate = (String, TypedGraphRule () ())

type Space = StateSpace (TypedGraphMorphism () ())


exploreStateSpace :: MorphismsConfig (TypedGraphMorphism () ()) -> Int -> Grammar (TypedGraphMorphism () ()) -> [(String, TypedGraph () ())] -> ([Int], Space)
exploreStateSpace conf maxDepth grammar graphs =
  let
    (productions, predicates) =
      splitPredicates (DPO.productions grammar)

    searchFrom (_, graph) =
      do
        (idx, _) <- putState graph
        depthSearch maxDepth graph
        return idx

    search =
      mapM searchFrom graphs

    initialSpace =
      StateSpace.empty conf (map snd productions) predicates
  in
    runStateSpaceBuilder search initialSpace


-- | Separates the rules that change nothing (which are considered predicates)
-- from those that have some effect (which are considered productions).
splitPredicates :: [(String, TypedGraphRule () ())] -> ([NamedProduction], [NamedPredicate])
splitPredicates [] =
  ([], [])

splitPredicates ((name, rule) : rest) =
  let
    (productions, predicates) =
      splitPredicates rest
  in
    if isIsomorphism (leftMorphism rule) && isIsomorphism (rightMorphism rule) then
      (productions, (name, rule):predicates)
    else
      ((name, rule):productions, predicates)
