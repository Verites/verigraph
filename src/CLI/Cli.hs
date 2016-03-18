import Data.Matrix
import Options.Applicative

import Graph.GraphRule (GraphRule)
import qualified CriticalPairs.CriticalPairs as CP
import qualified XML.GGXReader as XML

data VerigraphOpts = Opts
  { inputFile :: String }

verigraphOpts :: Parser VerigraphOpts
verigraphOpts = Opts
  <$> strOption
    ( long "input-file"
    <> metavar "FILE"
    <> help "GGX file defining the graph grammar")

execute :: VerigraphOpts -> IO ()
execute opts = do
    rules <- readGrammar (inputFile opts)

    let udMatrix = conflictMatrix (allDeleteUse False) rules
        pfMatrix = conflictMatrix (allProduceForbid False) rules
        peMatrix = conflictMatrix (allProdEdgeDelNode False) rules
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
        allProduceForbid onlyInj r1 r2 = CP.allProduceForbid r1 r2
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
