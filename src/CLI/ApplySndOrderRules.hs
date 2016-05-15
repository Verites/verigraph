module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
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
        onlyInj = if arbitraryMatches globalOpts then ALL else MONO
        newRules = applySndOrderRules onlyInj (GG.rules gg) (GG.sndOrderRules gg)
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ newRules) (GG.sndOrderRules gg)
        --rul = snd (head (GG.sndOrderRules gg))
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
    
    --print (domain (SO.right rul))
    
    putStrLn "Done!"
    putStrLn ""

applySndOrderRules :: PROP -> [(String, GraphRule a b)] -> [(String, SO.SndOrderRule a b)] -> [(String, GraphRule a b)]
applySndOrderRules prop fstRules = concatMap (\r -> applySndOrderRuleListRules prop r fstRules)

applySndOrderRuleListRules :: PROP -> (String, SO.SndOrderRule a b) -> [(String, GraphRule a b)] -> [(String, GraphRule a b)]
applySndOrderRuleListRules prop sndRule = concatMap (applySndOrderRule prop sndRule)

applySndOrderRule :: PROP -> (String, SO.SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule prop (sndName,sndRule) (fstName,fstRule) = zip newNames newRules
  where
    --a = matches!!0
    --b = SO.mappingLeft a
    --c = SO.mappingInterface a
    --d = SO.mappingRight a
    --e = SO.mappingLeft (SO.left sndRule)
    --(f,g) = poc a (SO.left sndRule)
    --(h,i) = po f (SO.right sndRule)
    --n = head (SO.nacs sndRule)
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    leftRule = SO.left sndRule
    rightRule = SO.right sndRule
    matches = SO.matchesSndOrder prop (codomain leftRule) fstRule
    gluing = filter (\m -> satsGluing False m leftRule) matches
    nacs = filter (satsNacs True False sndRule) gluing
    newRules = map
                 (\match ->
                   let (k,_)  = poc match leftRule
                       (m',_) = po k rightRule in
                       codomain m'
                   ) nacs
