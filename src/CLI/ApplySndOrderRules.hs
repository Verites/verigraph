module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Control.Monad                           (when)
import           Data.Monoid                             ((<>))
import           Data.Text.Prettyprint.Doc               (Pretty (..))
import           Options.Applicative
import           System.IO                               (hPrint, stderr)

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Data.Graphs                             (Graph)
import           Data.TypedGraph.Morphism
import           GlobalOptions
import qualified Image.Dot.TypedGraph                    as Dot
import qualified Rewriting.DPO.TypedGraph                as GR
import           Rewriting.DPO.TypedGraphRule            (toSndOrderMorphismsConfig)
import qualified Rewriting.DPO.TypedGraphRule.Scheduling as SO
import           Util
import qualified XML.GGXWriter                           as GW

data SchedulingType = AsLongAsPossible | AllMatchesOneStep | Specific deriving (Eq, Show)

data Options = Options
  { outputFile :: String
  , scheduling :: SchedulingType
  , limitPar   :: Int
  , srcRule    :: Maybe String
  , tgtRule    :: Maybe String
  }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the new rules to the original graph grammar")
  <*> schedulingIn
  <*> option auto
    ( long "limit"
    <> metavar "(INT > 0)"
    <> action "int"
    <> showDefault
    <> value 5
    <> help "Input of 'as-long-as-possible', limit of rewritings")
  <*> optional (strOption
    ( long "from"
    <> metavar "2-rule"
    <> action "file"
    <> help "Input of 'specific', 2-rule to the second-order rewriting"))
  <*> optional (strOption
    ( long "to"
    <> metavar "rule"
    <> action "file"
    <> help "Input of 'specific', rule to be evolved by the second-order rewriting"))

schedulingIn :: Parser SchedulingType
schedulingIn =
      flag' AllMatchesOneStep
        ( long "one-step"
          <> help "Apply each match from 2-rules to rules once")
  <|> flag' AsLongAsPossible
        ( long "as-long-as-possible"
          <> help "Apply 'AsLongAsPossible' all second-order rules")
  <|> flag' Specific
        ( long "specific"
          <> help "Apply all matches from the given 2rule and rule")
  <|> pure AllMatchesOneStep

addEmptyFstOrderRule :: Graph (Maybe a) (Maybe b) -> [(String,GR.TypedGraphRule a b)] -> [(String,GR.TypedGraphRule a b)]
addEmptyFstOrderRule typegraph fstRules =
  if any (GR.nullGraphRule . snd) fstRules then
    fstRules
  else
    fstRulesPlusEmpty

  where
    fstRulesPlusEmpty = ("emptyRule", emptyFstOrderRule) : fstRules
    emptyFstOrderRule = GR.emptyGraphRule typegraph

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts
        schedType = scheduling opts
        srcMaybe = srcRule opts
        (Just src) = srcMaybe
        tgtMaybe = tgtRule opts
        (Just tgt) = tgtMaybe
        limit = limitPar opts
        printDot = True --flag to test the print to .dot functions

    (fstOrderGG, sndOrderGG, ggName, names) <- loadSndOrderGrammar globalOpts True

    -- It is adding an empty first-order rule as possible target rule,
    -- it allows the creation from "zero" of a new first-order rule.
    let sndOrderRules = productions sndOrderGG
        fstRulesPlusEmpty = addEmptyFstOrderRule (typeGraph fstOrderGG) (productions fstOrderGG)
        namingContext = Dot.makeNamingContext names

    case arbitraryMatches globalOpts of
      MonoMatches -> putStrLn "Only injective matches allowed."
      AnyMatches  -> putStrLn "Non-injective matches allowed."
    putStrLn ("Utilizing the scheduling: " ++ show schedType)

    let
      dpoConf' = toSndOrderMorphismsConfig dpoConf
      newRulesLog AsLongAsPossible = SO.asLongAsPossible dpoConf' sndOrderRules fstRulesPlusEmpty limit
      newRulesLog AllMatchesOneStep = SO.oneStep dpoConf' sndOrderRules fstRulesPlusEmpty
      newRulesLog Specific = SO.specific dpoConf' sndOrderRules fstRulesPlusEmpty src tgt
      (log, newRules_) = newRulesLog schedType
      newRules = (if schedType `elem` [AllMatchesOneStep,Specific] then productions fstOrderGG else []) ++ newRules_

    putStrLn log
    putStrLn ""

    let dots = [ Dot.sndOrderRule namingContext (pretty ruleName) rule | (ruleName, rule) <- productions sndOrderGG ]
    when printDot $ mapM_ (hPrint stderr) dots

    let newGG = fstOrderGG {productions = newRules}

    GW.writeGrammarFile (newGG,sndOrderGG) ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""

typeGraph :: Grammar (TypedGraphMorphism a b) -> Graph (Maybe a) (Maybe b)
typeGraph = codomain . start
