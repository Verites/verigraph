{-# LANGUAGE TypeFamilies #-}

module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.DPO
import           GlobalOptions
import           Options.Applicative
import qualified SndOrder.Rule           as SO
import qualified TypedGraph.GraphGrammar as GG
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

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = dpoConfig globalOpts

    (gg,printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    putStrLn $ "injective satisfability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    mapM_ putStrLn (XML.printMinimalSafetyNacsLog dpoConf printNewNacs)

    let newRules = SO.applySecondOrder (SO.applySndOrderRule dpoConf) (GG.rules gg) (GG.sndOrderRules gg)
        gg2 = gg {GG.rules = GG.rules gg ++ newRules}

    putStrLn ""

    GW.writeGrammarFile gg2 ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""
