
module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.Category.AdhesiveHLR
import           Abstract.Rewriting.DPO
import           Control.Monad                 (when)
import           Data.Graphs                   (Graph)
import           Data.TypedGraph.Morphism
import qualified Rewriting.DPO.TypedGraph      as GR
import qualified Rewriting.DPO.TypedGraphRule  as SO


import           Data.Monoid                   ((<>))
import           GlobalOptions
import           Options.Applicative

import           Image.Dot
import qualified XML.GGXReader                 as XML
import qualified XML.GGXWriter                 as GW

newtype Options = Options
  { outputFile :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the new rules to the original graph grammar")

addEmptyFstOrderRule :: Graph (Maybe a) (Maybe b) -> [(String,GR.GraphRule a b)] -> [(String,GR.GraphRule a b)]
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
        printDot = False --flag to test the print to .dot functions

    (fstOrderGG, sndOrderGG, printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    putStrLn $ "injective satisfability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    mapM_ putStrLn (XML.printMinimalSafetyNacsLog printNewNacs)

    -- It is adding an empty first order rule as possible match target,
    -- it allows the creation from "zero" of a new second order rules.
    let fstRulesPlusEmpty = addEmptyFstOrderRule (typeGraph fstOrderGG) (productions fstOrderGG)
        newRules = SO.applySecondOrder (SO.applySndOrderRule dpoConf) fstRulesPlusEmpty (productions sndOrderGG)
        newGG = fstOrderGG {productions = productions fstOrderGG ++ newRules}
        namingContext = makeNamingContext names

    putStrLn ""

    let dots = map (uncurry (printSndOrderRule namingContext)) (productions sndOrderGG)
    when printDot $ mapM_ print dots

    GW.writeGrammarFile (newGG,sndOrderGG) ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""

typeGraph :: Grammar (TypedGraphMorphism a b) -> Graph (Maybe a) (Maybe b)
typeGraph = codomain . start
