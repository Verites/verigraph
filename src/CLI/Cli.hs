import Data.Matrix
import Options.Applicative

import Graph.GraphRule (GraphRule)
import qualified CriticalPairs.CriticalPairs as CP
import qualified XML.GGXReader as XML

data VerigraphOpts = Opts
  { inputFile :: String
  , injectiveMatchesOnly :: Bool }

verigraphOpts :: Parser VerigraphOpts
verigraphOpts = Opts
  <$> strOption
    ( long "input-file"
    <> metavar "FILE"
    <> help "GGX file defining the graph grammar")
  <*> flag False True
    ( long "injective-matches-only"
    <> help "Restrict the analysis to injective matches only")

execute :: VerigraphOpts -> IO ()
execute opts = do
    rules <- readGrammar (inputFile opts)
    let onlyInj = injectiveMatchesOnly opts
        udMatrix = conflictMatrix (allDeleteUse onlyInj) rules
        pfMatrix = conflictMatrix (allProduceForbid onlyInj) rules
        peMatrix = conflictMatrix (allProdEdgeDelNode onlyInj) rules
        conflictsMatrix = udMatrix + pfMatrix + peMatrix
    print "Delete-Use"
    print udMatrix
    print "Produce-Forbid"
    print pfMatrix
    print "Produce Edge Delete Node"
    print peMatrix
    print "All Conflicts"
    print conflictsMatrix
  where allDeleteUse onlyInj r1 r2 = CP.allDeleteUse r1 r2 onlyInj
        allProduceForbid _ r1 r2 = CP.allProduceForbid r1 r2
        allProdEdgeDelNode onlyInj r1 r2 = CP.allProdEdgeDelNode r1 r2 onlyInj

readGrammar :: String -> IO [GraphRule a b]
readGrammar fileName = do
  typeGraph <- XML.readTypeGraph fileName
  rules <- XML.readRules fileName

  let rulesNames = map (\((x,_,_,_),_) -> x) rules
  print rulesNames

  return $ map (XML.instantiateRule (head typeGraph)) rules

conflictMatrix :: (GraphRule a b -> GraphRule a b -> [c]) -> [GraphRule a b] -> Matrix Int
conflictMatrix conflictsBetween rules =
  matrix (length rules) (length rules) $ \(i,j) ->
    let ruleI = rules !! (i-1)
        ruleJ = rules !! (j-1)
    in length (conflictsBetween ruleI ruleJ)

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> verigraphOpts)
      ( fullDesc
      <> progDesc "Run critical pair analysis on a given graph grammar")
