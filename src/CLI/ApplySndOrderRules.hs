module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR

import           Graph.Graph             (Graph)
import qualified GraphGrammar.Core       as GG
import qualified SndOrder.Rule           as SO
import qualified TypedGraph.DPO.GraphRule    as GR
import           TypedGraph.Morphism

import           GlobalOptions
import           Options.Applicative

import qualified XML.GGXReader           as XML
import qualified XML.GGXWriter           as GW

data Options = Options
  { outputFile :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the new rules to the original graph grammar")

addEmptyFstOrderRule :: Graph a b -> [(String,GR.GraphRule a b)] -> [(String,GR.GraphRule a b)]
addEmptyFstOrderRule typegraph fstRules =
  if elem True (map (GR.nullGraphRule . snd) fstRules) then
    fstRules
  else
    fstRulesPlusEmpty

  where
    fstRulesPlusEmpty = ("emptyRule", emptyFstOrderRule) : fstRules
    emptyFstOrderRule = GR.emptyGraphRule typegraph

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts

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
    let fstRulesPlusEmpty = addEmptyFstOrderRule (typeGraph fstOrderGG) (GG.rules fstOrderGG)
        newRules = SO.applySecondOrder (SO.applySndOrderRule dpoConf) fstRulesPlusEmpty (GG.rules sndOrderGG)
        newGG = fstOrderGG {GG.rules = GG.rules fstOrderGG ++ newRules}

    putStrLn ""

    GW.writeGrammarFile (newGG,sndOrderGG) ggName names (outputFile opts)
    
    putStrLn "Done!"
    putStrLn ""

typeGraph :: GG.Grammar (TypedGraphMorphism a b) -> Graph a b
typeGraph = codomain . (GG.start)
