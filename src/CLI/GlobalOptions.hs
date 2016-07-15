module GlobalOptions
  ( GlobalOptions
  , globalOpts
  , arbitraryMatches
  , injectiveNacSatisfaction
  , verbose
  , inputFile
  ) where

import           Options.Applicative
import           Abstract.AdhesiveHLR (NacSatisfaction(..), MatchRestriction(..))

data GlobalOptions = GOpts
  { arbitraryMatches         :: MatchRestriction
  , injectiveNacSatisfaction :: NacSatisfaction
  , verbose                  :: Bool
  , inputFile                :: String
  }

globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag AnyMatches MonoMatches
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
