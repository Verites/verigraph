module CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Control.Monad                         (when)
import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           GHC.Conc                              (numCapabilities)
import           Options.Applicative

import           Abstract.Rewriting.DPO
import           Analysis.Interlevel.EvolutionarySpans
import           Analysis.Interlevel.InterLevelCP
import           Category.TypedGraph                   (TypedGraphMorphism)
import           GlobalOptions
import           Rewriting.DPO.TypedGraphRule
import           Util
import           Util.List
import qualified XML.GGXReader                         as XML
import qualified XML.GGXWriter                         as GW

data Options = Options
  { outputFile    :: Maybe String
  , sndOrder      :: Bool
  , essentialFlag :: Bool
  , analysisType  :: AnalysisType
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
  <*> cpOrder
  <*> essentialCP
  <*> cpAnalysisType

cpOrder :: Parser Bool
cpOrder = flag False True
    ( long "snd-order"
    <> help "Set the analysis to the second-order rules")

essentialCP :: Parser Bool
essentialCP = flag False True
    ( long "essential"
    <> help "Compute the Essential Critical Pairs analysis (Warning: not fully supported yet)")

cpAnalysisType :: Parser AnalysisType
cpAnalysisType =
      flag' Conflicts
        ( long "conflicts-only"
          <> help "Restrict to Critical Pair analysis")
  <|> flag' Dependencies
        ( long "dependencies-only"
          <> help "Restrict to Critical Sequence analysis")
  <|> pure Both

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts

    putStrLn $ "number of cores: " ++ show numCapabilities ++ "\n"

    (fstOrderGG, sndOrderGG, printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let action = analysisType opts
        essentialCP = essentialFlag opts
        secondOrder = sndOrder opts
        writer = defWriterFun essentialCP secondOrder dpoConf action

        namedFstOrdRules = productions fstOrderGG
        namedSndOrdRules = productions sndOrderGG
        fstOrdRules = map snd namedFstOrdRules
        sndOrdRules = map snd namedSndOrdRules

        dpoConf' = toSndOrderMorphismsConfig dpoConf
        interlevelWithoutCounting = Set.fromList 
          [ (x,y)
            | sndOrdRule <- namedSndOrdRules
            , fstOrdRule <- namedFstOrdRules
            , (x,y,_,_) <- interLevelCP dpoConf' sndOrdRule fstOrdRule ]
        evoConflicts = allEvolSpans dpoConf' namedSndOrdRules


    putStrLn $ "only injective matches morphisms: " ++ show (arbitraryMatches globalOpts)
    putStrLn ""

    when secondOrder $ mapM_ putStrLn (XML.printMinimalSafetyNacsLog printNewNacs)

    when essentialCP $ putStrLn "Warning: essential critical pairs not fully supported"
    putStrLn ""

    let fstOrderAnalysis = printAnalysis essentialCP action dpoConf fstOrdRules
        sndOrderAnalysis = printAnalysis essentialCP action dpoConf' sndOrdRules
    case outputFile opts of
      Just file ->
        do
          putStrLn "Warning: exporting conflicts/dependencies to .cpx not fully supported."
          writer (fstOrderGG, sndOrderGG) ggName names file
      Nothing ->
        if secondOrder
          then sndOrderAnalysis
          else fstOrderAnalysis

    when secondOrder $
      mapM_ putStrLn $
        "Inter-level Critical Pairs Analysis" :
        "This log shows a list of (first-order rule, second-order rule) that are in conflict:" :
        [show interlevelWithoutCounting] --map printILCP interlevelCPs

    putStrLn ""

    when secondOrder $
      mapM_ putStrLn $
        "Evolutionary Spans Inter-level CP:" :
        "This log shows pairs of (second-order rule, second-order rule, number of FolFol, number of ConfFol, number of FolConf, number of ConfConf)" :
        printEvoConflicts evoConflicts

    putStrLn ""
    putStrLn "Critical Pair Analysis done!"

-- | Inter-level CP to Strings
--printILCP :: (String, String, Int, InterLevelCP a b) -> String
--printILCP (fstName, sndName, idx, _) =
--  fstName ++ " " ++ sndName ++ " (id:" ++ show idx ++ ") ... (conflict omitted)"

-- | Evolutionary Spans to Strings
printEvoConflicts :: [(String, String, [EvoSpan a b])] -> [String]
printEvoConflicts = map printOneEvo
  where
    fst (y,_,_) = y
    snd (_,y,_) = y
    thd (_,_,y) = y

    printOneEvo e = "(" ++ fst e ++ ", " ++ snd e ++ ", " ++
                       show (printConf (False,False) (thd e)) ++ ", " ++
                       show (printConf (True,False) (thd e)) ++ ", " ++
                       show (printConf (False,True) (thd e)) ++ ", " ++
                       show (printConf (True,True) (thd e)) ++ ")"
    printConf str evos = countElement str (map cpe evos)

defWriterFun :: Bool -> Bool -> MorphismsConfig (TypedGraphMorphism a b) -> AnalysisType
             -> GW.Grammars a b -> String
             -> [(String,String)] -> String -> IO ()
defWriterFun essential secondOrder conf t =
  case (secondOrder,t) of
    (False, Conflicts)    -> GW.writeConflictsFile essential conf
    (False, Dependencies) -> GW.writeDependenciesFile conf
    (False, Both)         -> GW.writeConfDepFile essential conf
    (True, Conflicts)     -> GW.writeSndOderConflictsFile conf'
    (True, Dependencies)  -> GW.writeSndOderDependenciesFile conf'
    (True, Both)          -> GW.writeSndOderConfDepFile conf'
    (_, None)             -> GW.writeGrammarFile
  where conf' = toSndOrderMorphismsConfig conf
