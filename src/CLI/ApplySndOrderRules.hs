{-# LANGUAGE TypeFamilies #-}

module CLI.ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.EpiPairs            ()
import qualified Graph.GraphGrammar        as GG
import           Graph.GraphMorphism
import           Graph.GraphRule
import qualified Graph.RuleMorphism        as SO
import qualified Graph.SndOrderRule        as SO
import           Graph.TypedGraphMorphism
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
        testSndOrder = map (\(n,r) -> (n,SO.minimalSafetyNacs r)) (GG.sndOrderRules gg)
        rule = snd (head (GG.sndOrderRules gg))
        gg2 = GG.graphGrammar (GG.initialGraph gg) ((GG.rules gg) ++ newRules) testSndOrder--(GG.sndOrderRules gg)
        --gg3 = GG.graphGrammar (GG.initialGraph gg) rulePairs []--(GG.sndOrderRules gg)
        --rul = snd (head (GG.sndOrderRules gg))
        --rulePairs = map (\(idx,(a,_)) -> (show idx, codomain a)) (zip [0..] (createPairs True ruleL ruleR))
    
    GW.writeGrammarFile gg2 ggName names (outputFile opts)
    
    --print (GG.rules gg)
    --print (head (nacs rule))
    --print (map (\x -> codomain (left (codomain x))) (newNacsPairL rule))
    --print (length (newNacsPairL rule))
    --print (createPairs True (codomain (left rule)) (codomain (left rule)))
    --print (length (createPairs' True ruleL ruleL))
    --GW.writeGrammarFile gg3 ggName names "t.ggx"
    
    putStrLn "Done!"
    putStrLn ""

applySndOrderRules :: PROP -> [(String, GraphRule a b)] -> [(String, SO.SndOrderRule a b)] -> [(String, GraphRule a b)]
applySndOrderRules prop fstRules = concatMap (\r -> applySndOrderRuleListRules prop r fstRules)

applySndOrderRuleListRules :: PROP -> (String, SO.SndOrderRule a b) -> [(String, GraphRule a b)] -> [(String, GraphRule a b)]
applySndOrderRuleListRules prop sndRule = concatMap (applySndOrderRule prop sndRule)

applySndOrderRule :: PROP -> (String, SO.SndOrderRule a b) -> (String, GraphRule a b) -> [(String, GraphRule a b)]
applySndOrderRule prop (sndName,sndRule) (fstName,fstRule) = zip newNames newRules
  where
    newNames = map (\number -> fstName ++ "_" ++ sndName ++ "_" ++ show number) ([0..] :: [Int])
    leftRule = left sndRule
    rightRule = right sndRule
    mats = matches prop (codomain leftRule) fstRule
    gluing = filter (\m -> satsGluing False m leftRule) mats
    nacs = filter (satsNacs True False sndRule) gluing
    newRules = map
                 (\match ->
                   let (k,_)  = poc match leftRule
                       (m',_) = po k rightRule in
                       codomain m'
                   ) nacs
