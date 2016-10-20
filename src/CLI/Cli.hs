module Main (main) where

import           GlobalOptions
import           Options.Applicative

import qualified ApplySndOrderRules   as ASO
import qualified ConcurrentRules      as CR
import qualified CriticalPairAnalysis as CPA
import qualified PBComparison         as PB

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Software specification and verification tool based on graph rewriting")

execute :: (GlobalOptions, Command) -> IO ()
execute (globalOpts, CPAnalysis opts)         = CPA.execute globalOpts opts
execute (globalOpts, ApplySndOrderRules opts) = ASO.execute globalOpts opts
execute (globalOpts, ConcurrentRules opts)    = CR.execute globalOpts opts
execute (globalOpts, PBComparison opts)       = PB.execute globalOpts opts

options :: Parser (GlobalOptions, Command)
options = (\cmd opts -> (opts, cmd)) <$> commands <*> globalOpts

data Command =
    CPAnalysis CPA.Options
  | ApplySndOrderRules ASO.Options
  | ConcurrentRules CR.Options
  | PBComparison PB.Options

commands :: Parser Command
commands = subparser (cpAnalysis <> secondOrder <> concurrentRule <> pbComparison)
  where
    cpAnalysis = command "analysis" . fmap CPAnalysis $ info (helper <*> CPA.options)
      ( fullDesc <> progDesc "Run critical pair analysis on the input grammar")

    secondOrder = command "snd-order" . fmap ApplySndOrderRules $ info (helper <*> ASO.options)
      ( fullDesc <> progDesc "Apply all second order rules in all possible matches with the first order rules")

    concurrentRule = command "concurrent-rule" . fmap ConcurrentRules $ info (helper <*> CR.options)
      ( fullDesc <> progDesc "Generate concurrent rules for the input grammar and rule sequences")

    pbComparison = command "pb" . fmap PBComparison $ info (helper <*> PB.options)
      ( fullDesc <> progDesc "pbComparison")
