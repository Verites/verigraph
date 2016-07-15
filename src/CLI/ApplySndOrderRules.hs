{-# LANGUAGE TypeFamilies #-}

module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.DPO
import           GlobalOptions
import qualified TypedGraph.GraphGrammar        as GG
import qualified SndOrder.Rule        as SO
import           Options.Applicative
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

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
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = arbitraryMatches globalOpts
        newNacs =
          map (\(n,r) -> let newRule = SO.addMinimalSafetyNacs nacInj r
                             tamNewNacs = length (nacs newRule)
                             tamNacs = length (nacs r) in
           ((n, newRule), (n, tamNewNacs - tamNacs)))
           (GG.sndOrderRules gg)
        addNacs = map fst newNacs
        printNewNacs = map snd newNacs
        --rule = snd (head (GG.sndOrderRules gg))
        newRules = SO.applySecondOrder (SO.applySndOrderRule nacInj onlyInj) (GG.rules gg) (GG.sndOrderRules gg)
        gg2 = GG.graphGrammar (GG.initialGraph gg) (GG.rules gg ++ newRules) addNacs--(GG.sndOrderRules gg)
        --gg2 = GG.graphGrammar (GG.initialGraph gg) (GG.rules gg) addNacs--(GG.sndOrderRules gg)
        --gg3 = GG.graphGrammar (GG.initialGraph gg) rulePairs []--(GG.sndOrderRules gg)
        --rul = snd (head (GG.sndOrderRules gg))
        --rulePairs = map (\(idx,(a,_)) -> (show idx, codomain a)) (zip [0..] (createPairs True ruleL ruleR))

    putStrLn $ "injective satisfability of nacs: " ++ show nacInj
    putStrLn $ "only injective matches morphisms: " ++ show onlyInj
    putStrLn ""

    mapM_
      putStrLn $
      ["Adding minimal safety nacs to second order rules"]
      ++ (if onlyInj == MonoMatches then [] else ["Warning, some nacs for non injective matches are not implemented"])
      ++ map (\(r,n) -> "Rule "++ r ++", added " ++ show n ++ " nacs") printNewNacs
      ++ ["All nacs added!",""]

    GW.writeGrammarFile gg2 ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""
