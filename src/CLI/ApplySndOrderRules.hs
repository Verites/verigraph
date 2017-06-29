module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Data.Monoid                   ((<>))
import           Options.Applicative

import           Abstract.Category.AdhesiveHLR
import           Abstract.Rewriting.DPO
import           Control.Monad                 (when)
import           Data.Graphs                   (Graph)
import           Data.TypedGraph.Morphism
import           GlobalOptions
import           Image.Dot
import qualified Rewriting.DPO.TypedGraph      as GR
import qualified Rewriting.DPO.TypedGraphRule.Scheduling  as SO
import qualified XML.GGXReader                 as XML
import qualified XML.GGXWriter                 as GW

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
    <> help ("Input of 'as-long-as-possible', limit of rewritings"))
  <*> optional (strOption
    ( long "from"
    <> metavar "2-rule"
    <> action "file"
    <> help ("Input of 'specific', 2-rule to the second-order rewriting")))
  <*> optional (strOption
    ( long "to"
    <> metavar "rule"
    <> action "file"
    <> help ("Input of 'specific', rule to be evolved by the second-order rewriting")))

schedulingIn :: Parser SchedulingType
schedulingIn =
      flag' AllMatchesOneStep
        ( long "one-step"
          <> help "Apply all matches from 2-rules to rules once.")
  <|> flag' AsLongAsPossible
        ( long "as-long-as-possible"
          <> help "Apply 'AsLongAsPossible' all second-order rules.")
  <|> flag' Specific
        ( long "specific"
          <> help "Apply all matches 'specific' from the given 2rule and rule.")
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
        printDot = False --flag to test the print to .dot functions

    (fstOrderGG, sndOrderGG, printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    putStrLn $ "injective satisfiability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    mapM_ putStrLn (XML.printMinimalSafetyNacsLog printNewNacs)

    -- It is adding an empty first-order rule as possible target rule,
    -- it allows the creation from "zero" of a new first-order rule.
    let sndOrderRules = productions sndOrderGG
        fstRulesPlusEmpty = addEmptyFstOrderRule (typeGraph fstOrderGG) (productions fstOrderGG)
        namingContext = makeNamingContext names

    putStrLn ""

    putStrLn ("Utilizing the scheduling: " ++ show schedType)

    let newRulesLog AsLongAsPossible = SO.asLongAsPossible dpoConf sndOrderRules fstRulesPlusEmpty limit
        newRulesLog AllMatchesOneStep = SO.oneStep dpoConf sndOrderRules fstRulesPlusEmpty
        newRulesLog Specific = SO.specific dpoConf sndOrderRules fstRulesPlusEmpty src tgt
        (log, newRules_) = newRulesLog schedType
        newRules = (if schedType `elem` [AllMatchesOneStep,Specific] then productions fstOrderGG else []) ++ newRules_

    putStrLn log
    putStrLn ""

    let dots = map (uncurry (printSndOrderRule namingContext)) (productions sndOrderGG)
    when printDot $ mapM_ print dots

    let newGG = fstOrderGG {productions = newRules}

    GW.writeGrammarFile (newGG,sndOrderGG) ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""

typeGraph :: Grammar (TypedGraphMorphism a b) -> Graph (Maybe a) (Maybe b)
typeGraph = codomain . start
