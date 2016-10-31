{-# LANGUAGE TypeFamilies #-}

module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO

import           Abstract.Cocomplete
import           TypedGraph.Morphism

import           GlobalOptions
import           Graph.Graph             (Graph)
import           Options.Applicative
import qualified SndOrder.Morphism       as SO
import qualified SndOrder.Rule           as SO
import qualified TypedGraph.GraphGrammar as GG
import qualified TypedGraph.GraphRule    as GR
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
    let dpoConf = dpoConfig globalOpts

    (gg,printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
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
    let fstRulesPlusEmpty = addEmptyFstOrderRule (GG.typeGraph gg) (GG.rules gg)
        newRules = SO.applySecondOrder (SO.applySndOrderRule dpoConf) fstRulesPlusEmpty (GG.sndOrderRules gg)
        gg2 = gg {GG.rules = GG.rules gg ++ newRules}

    putStrLn ""
    
    let r2 = snd $ head (GG.sndOrderRules gg)
    print $ coproduct (domain (getLHS r2))
    
    --let r1 = snd $ head (GG.rules gg)
    
    --print $ domain (GR.getRHS r1)
    --print $ codomain (GR.getRHS r1)
    
    --print $ coproduct (GR.getRHS r1)
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""


coproduct :: GR.GraphRule a b -> (SO.RuleMorphism a b, SO.RuleMorphism a b)
coproduct a = calculateCoproduct a a
