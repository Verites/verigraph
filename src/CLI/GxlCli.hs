module Main (main) where

import           Data.Monoid              ((<>))
import           GHC.Conc                 (numCapabilities)
import           Options.Applicative

import           Abstract.Category
import           Abstract.Rewriting.DPO   as DPO
import           Category.TypedGraph
import           GlobalOptions
import           Rewriting.DPO.TypedGraph
import           Util
import qualified XML.GGXWriter            as GW
import qualified XML.GPRReader.GXLReader  as GPR

main :: IO ()
main =
  execParser opts >>= execute

  where
    opts =
      info (helper <*> allOptions)
           ( fullDesc
           <> progDesc "Loads the given graph grammar in the Groove Productions System (.gps).")


data Options = Options
  { outputFile        :: Maybe String
  , criticalPairs     :: Bool
  , criticalSequences :: Bool
  , essentialFlag     :: Bool
  }

options :: Parser Options
options = Options
  <$> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> flag False True
    ( long "cpa"
    <> help "Runs the Critical Pairs analysis")
  <*> flag False True
    ( long "csa"
    <> help "Runs the Critical Sequences analysis")
  <*> flag False True
    ( long "essential"
    <> help "Compute the Essential Critical Pairs analysis (Warning: not fully supported yet)")

allOptions :: Parser (GlobalOptions, Options)
allOptions =
  (,)
  <$> globalOpts
  <*> options

execute :: (GlobalOptions, Options) -> IO ()
execute (globalOpts, options) =
  do
    (fstOrderGrammar,names) <- GPR.readGrammar (inputFile globalOpts)

    putStrLn $ "number of cores: " ++ show numCapabilities ++ "\n"
    putStrLn "Loading the graph grammar..."
    putStrLn ""

    let ggName = GPR.readGGName (inputFile globalOpts)
        dpoConf = morphismsConf globalOpts
        action
          | criticalPairs options     = Conflicts
          | criticalSequences options = Dependencies
          | otherwise                 = None

        essentialCP = essentialFlag options
        writer = defWriterFun essentialCP dpoConf action

        namedRules = DPO.productions fstOrderGrammar
        rules = map snd namedRules

        -- creates an empty second-order grammar for the writer function
        typeGraph = codomain (leftObject (head rules))
        emptySndOrderGrammar = grammar (emptyGraphRule typeGraph) [] []

    let analysis = printAnalysis essentialCP action dpoConf rules
    case outputFile options of
      Just file ->
        do
          putStrLn "Warning: exporting conflicts/dependencies to .cpx not fully supported."
          writer (fstOrderGrammar,emptySndOrderGrammar) ggName names file
      Nothing -> analysis

    putStrLn ""
    putStrLn "Bye!"

    return ()

defWriterFun :: Bool -> MorphismsConfig (TypedGraphMorphism a b) -> AnalysisType
             -> GW.Grammars a b -> String
             -> [(String,String)] -> String -> IO ()
defWriterFun essential conf t =
  case t of
    Conflicts    -> GW.writeConflictsFile essential conf
    Dependencies -> GW.writeDependenciesFile conf
    _            -> GW.writeGrammarFile
