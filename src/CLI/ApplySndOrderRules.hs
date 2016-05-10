module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Abstract.AdhesiveHLR
import           Abstract.Morphism
import           Abstract.Valid
import qualified Graph.GraphGrammar        as GG
import qualified Graph.RuleMorphism        as SO
import qualified Graph.SndOrderRule        as SO
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
    <> help ("GGX file that will be written, adding the new rules to the original graph grammar"))

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = not $ injectiveMatchesOnly globalOpts
        --adding a rule
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ [("novaregra", codomain m')]) (GG.sndOrderRules gg)
        rules = map snd (GG.rules gg)
        
    -- testing import 2rule
        rule = snd (head (GG.sndOrderRules gg))
        leftRule = SO.left rule
        rightRule = SO.right rule
        ruleK = domain leftRule
        ruleL = codomain leftRule
        ruleR = codomain rightRule
        mat = head (SO.matchesSndOrder ruleL (rules!!1))
        (k,l') = poc mat leftRule
        (m',r') = po k rightRule
    
    --let x = codomain m'
    --print x
    --print $ valid x
    print "ASD"
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
