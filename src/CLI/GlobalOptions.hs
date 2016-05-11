module CLI.GlobalOptions
  ( GlobalOptions
  , globalOpts
  , arbitraryMatches
  , injectiveNacSatisfaction
  , verbose
  , inputFile
  ) where

import           Options.Applicative

data GlobalOptions = GOpts
  { arbitraryMatches         :: Bool
  , injectiveNacSatisfaction :: Bool
  , verbose                  :: Bool
  , inputFile                :: String
  }

globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag False True
    ( long "all-matches"
    <> help "Set the matches for arbitrary morphisms")
  <*> flag False True
    ( long "inj-nac-satisfaction"
    <> help ("Restrict the analysis of NAC satisfaction to injective " ++
            "morphisms between the NAC graph and the instance graph"))
  <*> flag False True
    ( long "verbose" <> short 'v')
  <*> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
