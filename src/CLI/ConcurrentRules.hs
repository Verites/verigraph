module CLI.ConcurrentRules
  ( Options
  , options
  , execute
  ) where

import           CLI.GlobalOptions

import           Analysis.ConcurrentRules
import qualified Graph.GraphGrammar        as GG
import           Graph.GraphRule
import           Options.Applicative
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data Options = Options
  { outputFile     :: String
  , generationType :: CRGenerationType
  , concRulesbyDep :: CRDependencies
  }

data CRGenerationType = MaxConcurrentRule | AllConcurrentRules

data CREpiPairsType = AllEpiPairs | OnlyInjectiveEpiPairs deriving (Eq)

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the concurrent rules to the original graph grammar")
  <*> crGenerationType
  <*> crDependencies

crGenerationType :: Parser CRGenerationType
crGenerationType =
      flag' MaxConcurrentRule
        ( long "max-rule"
         <> help "Generate only the concurrent rule with maximum overlap between comatch and match")
  <|> flag' AllConcurrentRules
        ( long "all-rules"
          <> help "Generate concurrent rules for all possible overlaps between comatch and match")

crDependencies :: Parser CRDependencies
crDependencies =
      flag AllOverlapings OnlyDependency
        ( long "by-dependency"
         <> help "Generate only the concurrent rules by the dependencies between the two rules")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)
    sequences <- XML.readSequences gg (inputFile globalOpts)
    let makeConcurrentRules = case generationType opts of
                                MaxConcurrentRule -> makeMaxConcurrentRule
                                AllConcurrentRules -> makeAllConcurrentRules
        dependencies = concRulesbyDep opts
        injectiveOnly = injectiveMatchesOnly globalOpts
        nacInj = injectiveNacSatisfaction globalOpts
        newRules = concatMap (makeConcurrentRules dependencies nacInj injectiveOnly) sequences
        gg' = GG.graphGrammar (GG.initialGraph gg) (GG.rules gg ++ newRules) []
    GW.writeGrammarFile gg' ggName names (outputFile opts)

makeAllConcurrentRules :: CRDependencies -> Bool -> Bool -> (String, [GraphRule a b]) -> [(String, GraphRule a b)]
makeAllConcurrentRules dep nacInj injectiveOnly (baseName, sequence) = zipWith makeName (allConcurrentRules dep nacInj injectiveOnly sequence) [0::Int ..]
  where makeName rule idx = (baseName++"_"++show idx, rule)

makeMaxConcurrentRule :: CRDependencies -> Bool -> Bool -> (String, [GraphRule a b]) -> [(String, GraphRule a b)]
makeMaxConcurrentRule dep nacInj injectiveOnly (baseName, sequence) = [(baseName, maxConcurrentRule dep nacInj injectiveOnly sequence)]
