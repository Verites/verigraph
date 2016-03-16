import Options.Applicative

data Sample = Sample
  { hello :: String
  , quiet :: Bool }

sample :: Parser Sample
sample = Sample
  <$> strOption
    ( long "hello"
    <> metavar "TARGET"
    <> help "Target for the greeting")
  <*> switch
    ( long "quiet"
    <> help "Whether to be quiet")

execute :: Sample -> IO()
execute (Sample h False) = putStrLn $ "Hello, " ++ h
execute _ = return ()

main :: IO()
main = execParser opts >>= execute
  where
    opts = info (helper <*> sample)
      ( fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )
