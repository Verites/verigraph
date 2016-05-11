module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Abstract.AdhesiveHLR
--import           Abstract.Morphism
--import           Abstract.Valid
import qualified Graph.GraphGrammar        as GG
import           Graph.GraphRule
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

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    let --nacInj = injectiveNacSatisfaction globalOpts
        --onlyInj = not $ injectiveMatchesOnly globalOpts
        newRules = applySndOrderRules (GG.rules gg) (GG.sndOrderRules gg)
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ newRules) (GG.sndOrderRules gg)
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
    
    putStrLn "Done!"
    putStrLn ""

applySndOrderRules :: [(String, GraphRule a b)] -> [(String, SO.SndOrderRule a b)] -> [(String, GraphRule a b)]
applySndOrderRules fstRules = concatMap (\r -> applySndOrderRuleListRules r fstRules)

applySndOrderRuleListRules :: (String, SO.SndOrderRule a b) -> [(String, GraphRule a b)] -> [(String, GraphRule a b)]
applySndOrderRuleListRules sndRule = concatMap (applySndOrderRule sndRule)

applySndOrderRule :: (String, SO.SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule (sndName,sndRule) (fstName,fstRule) = zip newNames newRules
  where
    newNames = map (\number -> fstName ++ "_" ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    leftRule = SO.left sndRule
    rightRule = SO.right sndRule
    matches = SO.matchesSndOrder (codomain leftRule) fstRule
    newRules = map
                 (\match ->
                   let (k,_)  = poc match leftRule
                       (m',_) = po k rightRule in
                       codomain m'
                   ) matches

