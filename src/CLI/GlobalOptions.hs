module GlobalOptions
  ( GlobalOptions
  , globalOpts
  , dpoConfig
  , verbose
  , inputFile
  ) where

import           Options.Applicative
import           Abstract.AdhesiveHLR (NacSatisfaction(..), MatchRestriction(..), DPOConfig(..))

data GlobalOptions = GOpts
  { arbitraryMatches         :: MatchRestriction
  , injectiveNacSatisfaction :: NacSatisfaction
  , verbose                  :: Bool
  , inputFile                :: String
  }


dpoConfig :: GlobalOptions -> DPOConfig
dpoConfig opts = DPOConfig (arbitraryMatches opts) (injectiveNacSatisfaction opts)


globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag MonoMatches AnyMatches
    ( long "all-matches"
    <> help "Set the matches for arbitrary morphisms")
  <*> flag MonoNacSatisfaction PartMonoNacSatisfaction
    ( long "inj-nac-satisfaction"
    <> help ("Restrict the analysis of NAC satisfaction to injective " ++
            "morphisms between the NAC graph and the instance graph"))
  <*> flag False True
    ( long "verbose" <> short 'v')
  <*> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
