module CLI.ConcurrentRules
  ( CROpts
  , crOpts
  , runConcurrentRules
  ) where

import           CLI.GlobalOptions

import           Analysis.ConcurrentRules
import qualified Graph.GraphGrammar        as GG
import           Graph.GraphRule
import           Options.Applicative
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data CROpts = CROpts
  { outputFile     :: String
  , generationType :: CRGenerationType
  }

data CRGenerationType = MaxConcurrentRule | AllConcurrentRules

data CREpiPairsType = AllEpiPairs | OnlyInjectiveEpiPairs deriving (Eq)

crOpts :: Parser CROpts
crOpts = CROpts
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the concurrent rules to the original graph grammar")
  <*> crGenerationType

crGenerationType :: Parser CRGenerationType
crGenerationType =
      flag' MaxConcurrentRule
        ( long "max-rule"
         <> help "Generate only the concurrent rule with maximum overlap between comatch and match")
  <|> flag' AllConcurrentRules
        ( long "all-rules"
          <> help "Generate concurrent rules for all possible overlaps between comatch and match")

runConcurrentRules :: GlobalOptions -> CROpts -> IO ()
runConcurrentRules globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)
    sequences <- XML.readSequences gg (inputFile globalOpts)
    let makeConcurrentRules = case generationType opts of
                                MaxConcurrentRule -> makeMaxConcurrentRule
                                AllConcurrentRules -> makeAllConcurrentRules
        injectiveOnly = injectiveMatchesOnly globalOpts
        newRules = concatMap (makeConcurrentRules injectiveOnly) sequences
        gg' = GG.graphGrammar (GG.initialGraph gg) (GG.rules gg ++ newRules)
    GW.writeGrammarFile gg' ggName names (outputFile opts)

makeAllConcurrentRules :: Bool -> (String, [GraphRule a b]) -> [(String, GraphRule a b)]
makeAllConcurrentRules injectiveOnly (baseName, sequence) = zipWith makeName (allConcurrentRules injectiveOnly sequence) [0..]
  where makeName rule idx = (baseName++"_"++show idx, rule)

makeMaxConcurrentRule :: Bool -> (String, [GraphRule a b]) -> [(String, GraphRule a b)]
makeMaxConcurrentRule injectiveOnly (baseName, sequence) = [(baseName, maxConcurrentRule injectiveOnly sequence)]
