import           Abstract.Morphism
import           Abstract.Valid
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Control.Monad             (forM_, when)
import qualified Data.List                 as L
import           Data.Matrix               hiding ((<|>))
import qualified Graph.GraphGrammar        as GG
import qualified Graph.GraphMorphism       as GM
import           Graph.GraphRule
import           Options.Applicative
import qualified Text.XML.HXT.Core         as HXT
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

import qualified CLI.ConcurrentRules       as CR
import qualified CLI.CriticalPairAnalysis  as CPA
import           CLI.GlobalOptions

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Software specification and verification tool based on graph rewriting")

execute :: (GlobalOptions, Command) -> IO ()
execute (globalOpts, CPAnalysis opts) = CPA.runCPAnalysis globalOpts opts
execute (globalOpts, ConcurrentRules opts) = CR.runConcurrentRules globalOpts opts

options :: Parser (GlobalOptions, Command)
options = (\cmd opts -> (opts, cmd)) <$> commands <*> globalOpts

data Command =
    CPAnalysis CPA.CPOpts
  | ConcurrentRules CR.CROpts

commands :: Parser Command
commands = subparser (cpAnalysis <> concurrentRule)
  where
    cpAnalysis = command "analysis" . fmap CPAnalysis $ info (helper <*> CPA.cpOpts)
      ( fullDesc <> progDesc "Run critical pair analysis on the input grammar")

    concurrentRule = command "concurrent-rule" . fmap ConcurrentRules $ info (helper <*> CR.crOpts)
      ( fullDesc <> progDesc "Generate concurrent rules for the input grammar and rule sequences")
