module Main (main) where

import           Options.Applicative
import           CLI.GlobalOptions

import qualified CLI.ConcurrentRules       as CR
import qualified CLI.CriticalPairAnalysis  as CPA

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Software specification and verification tool based on graph rewriting")

execute :: (GlobalOptions, Command) -> IO ()
execute (globalOpts, CPAnalysis opts) = CPA.execute globalOpts opts
execute (globalOpts, ConcurrentRules opts) = CR.execute globalOpts opts

options :: Parser (GlobalOptions, Command)
options = (\cmd opts -> (opts, cmd)) <$> commands <*> globalOpts

data Command =
    CPAnalysis CPA.Options
  | ConcurrentRules CR.Options

commands :: Parser Command
commands = subparser (cpAnalysis <> concurrentRule)
  where
    cpAnalysis = command "analysis" . fmap CPAnalysis $ info (helper <*> CPA.options)
      ( fullDesc <> progDesc "Run critical pair analysis on the input grammar")

    concurrentRule = command "concurrent-rule" . fmap ConcurrentRules $ info (helper <*> CR.options)
      ( fullDesc <> progDesc "Generate concurrent rules for the input grammar and rule sequences")
