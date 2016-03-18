import Data.Matrix
import Options.Applicative

import Graph.GraphRule (GraphRule)
import CriticalPairs.CriticalPairs (CriticalPair)
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
        udMatrix = pairwiseCompare (allDeleteUse onlyInj) rules
        pfMatrix = pairwiseCompare (allProduceForbid onlyInj) rules
        peMatrix = pairwiseCompare (allProdEdgeDelNode onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix

    print "Delete-Use"
    print (length <$> udMatrix)
    print "Produce-Forbid"
    print (length <$> pfMatrix)
    print "Produce Edge Delete Node"
    print (length <$> peMatrix)
    print "All Conflicts"
    print (length <$> conflictsMatrix)

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

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) ->
    compare (items !! (i-1)) (items !! (j-1))

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> verigraphOpts)
      ( fullDesc
      <> progDesc "Run critical pair analysis on a given graph grammar")
