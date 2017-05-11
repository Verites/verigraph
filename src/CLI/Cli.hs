module Main (main) where

import           Data.Monoid          ((<>))
import           GlobalOptions
import           Options.Applicative

import qualified ApplySndOrderRules   as ASO
import qualified ConcurrentRules      as CR
import qualified CriticalPairAnalysis as CPA
import qualified ParallelIndependence as PI
import qualified Processes            as PR
import qualified RunBenchs            as RB
import qualified SavePreInputs        as SPI

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Software specification and verification tool based on graph rewriting")

execute :: (GlobalOptions, Command) -> IO ()
execute (globalOpts, CPAnalysis opts)           = CPA.execute globalOpts opts
execute (globalOpts, ApplySndOrderRules opts)   = ASO.execute globalOpts opts
execute (globalOpts, ConcurrentRules opts)      = CR.execute globalOpts opts
execute (globalOpts, ParallelIndependence opts) = PI.execute globalOpts opts
execute (globalOpts, Processes opts)            = PR.execute globalOpts opts
execute (globalOpts, SavePreInputs opts)        = SPI.execute globalOpts opts
execute (globalOpts, RunBenchs opts)            = RB.execute globalOpts opts

options :: Parser (GlobalOptions, Command)
options = (\cmd opts -> (opts, cmd)) <$> commands <*> globalOpts

data Command =
    CPAnalysis CPA.Options
  | ApplySndOrderRules ASO.Options
  | ConcurrentRules CR.Options
  | ParallelIndependence PI.Options
  | Processes PR.Options
  | SavePreInputs SPI.Options
  | RunBenchs RB.Options

commands :: Parser Command
commands = subparser (cpAnalysis <> secondOrder <> concurrentRule <> parallelIndependence <> process <> preInputs <> runBenchs)
  where
    cpAnalysis = command "analysis" . fmap CPAnalysis $ info (helper <*> CPA.options)
      ( fullDesc <> progDesc "Run critical pair analysis on the input grammar")

    secondOrder = command "snd-order" . fmap ApplySndOrderRules $ info (helper <*> ASO.options)
      ( fullDesc <> progDesc "Apply all second order rules in all possible matches with the first order rules")

    concurrentRule = command "concurrent-rule" . fmap ConcurrentRules $ info (helper <*> CR.options)
      ( fullDesc <> progDesc "Generate concurrent rules for the input grammar and rule sequences")

    parallelIndependence = command "parallel-ind" . fmap ParallelIndependence $ info (helper <*> PI.options)
      ( fullDesc <> progDesc "Check parallel independence (through pullbacks) between all rules on the input grammar")

    process = command "process" . fmap Processes $ info (helper <*> PR.options)
      ( fullDesc <> progDesc "Generate graph processes for the input grammar and rule sequences")

    preInputs = command "pre-inputs" . fmap SavePreInputs $ info (helper <*> SPI.options)
      ( fullDesc <> progDesc "Save pre inputs for run-tests, can generate epi pairs or matches from rules to initial graph")

    runBenchs = command "run-benchs" . fmap RunBenchs $ info (helper <*> RB.options)
      ( fullDesc <> progDesc "Read a file with pre-calculated inputs and get execution times.")
