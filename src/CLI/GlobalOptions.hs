module CLI.GlobalOptions
  ( GlobalOptions
  , globalOpts

  , injectiveMatchesOnly
  , injectiveNacSatisfaction
  , verbose
  , inputFile
  ) where

import           Options.Applicative

data GlobalOptions = GOpts
  { injectiveMatchesOnly     :: Bool
  , injectiveNacSatisfaction :: Bool
  , verbose                  :: Bool
  , inputFile                :: String
  }

globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag False True
    ( long "inj-matches-only"
    <> help "Restrict the analysis to injective matches only")
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


