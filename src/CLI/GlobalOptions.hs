module GlobalOptions
  ( GlobalOptions
  , globalOpts
  , dpoConfig
  , verbose
  , inputFile
  , useConstraints
  ) where

import           Abstract.AdhesiveHLR (DPOConfig (..), MatchRestriction (..),
                                       NacSatisfaction (..))
import           Options.Applicative

data GlobalOptions = GOpts
  { arbitraryMatches         :: MatchRestriction
  , injectiveNacSatisfaction :: NacSatisfaction
  , verbose                  :: Bool
  , inputFile                :: String
  , useConstraints           :: Bool
  }


dpoConfig :: GlobalOptions -> DPOConfig
dpoConfig opts = DPOConfig (arbitraryMatches opts) (injectiveNacSatisfaction opts)


globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag MonoMatches AnyMatches
    ( long "all-matches"
    <> help "Set the matches for arbitrary morphisms")
  <*> flag MonomorphicNAC PartiallyMonomorphicNAC
    ( long "partial-injective-nacs"
    <> help ("Restrict the analysis of NAC satisfaction to partially injective " ++
            "morphisms between the NAC graph and the instance graph"))
  <*> flag False True
    ( long "verbose" <> short 'v')
  <*> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
  <*> flag False True
    ( long "use-constraints"
    <> help "Activate use of Constraints [EXPERIMENTAL: Concurrent Rules Only]")

