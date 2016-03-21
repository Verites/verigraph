import Data.Matrix
import Options.Applicative

import Graph.GraphRule (GraphRule)
import CriticalPairs.CriticalPairs (CriticalPair)
import qualified CriticalPairs.CriticalPairs as CP
import qualified XML.GGXReader as XML
import qualified Graph.GraphGrammar as GG
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphRule as GR
import qualified Abstract.Morphism as M
import qualified Text.XML.HXT.Core as HXT
import qualified XML.GGXWriter as GW

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
    gg <- readGrammar (inputFile opts)
    names <- getNames (inputFile opts)

    let onlyInj = injectiveMatchesOnly opts
        rules = map snd (GG.rules gg)
        udMatrix = pairwiseCompare (allDeleteUse onlyInj) rules
        pfMatrix = pairwiseCompare (allProduceForbid onlyInj) rules
        peMatrix = pairwiseCompare (allProdEdgeDelNode onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix

    --print "Delete-Use"
    --print (length <$> udMatrix)
    --print "Produce-Forbid"
    --print (length <$> pfMatrix)
    --print "Produce Edge Delete Node"
    --print (length <$> peMatrix)
    --print "All Conflicts"
    --print (length <$> conflictsMatrix)
    
    GW.writeCpxFile onlyInj gg names "hellow.cpx"

  where allDeleteUse onlyInj r1 r2 = CP.allDeleteUse onlyInj r1 r2 
        allProduceForbid onlyInj r1 r2 = CP.allProduceForbid onlyInj r1 r2
        allProdEdgeDelNode onlyInj r1 r2 = CP.allProdEdgeDelNode onlyInj r1 r2 

getNames :: String -> IO [(String,String)]
getNames fileName = do
  names <- XML.readNames fileName
  nacNames <- XML.readNacNames fileName
  return $ (head names) ++ (concat nacNames)

readGrammar :: String -> IO (GG.GraphGrammar a b)
readGrammar fileName = do
  parsedTypeGraph <- XML.readTypeGraph fileName
  parsedRules <- XML.readRules fileName
  let rulesNames = map (\((x,_,_,_),_) -> x) parsedRules
  print rulesNames
  
  let rules = map (XML.instantiateRule (head parsedTypeGraph)) parsedRules
  let typeGraph = M.codomain $ M.domain $ GR.left $ (head rules)
  
  let initGraph = GM.empty typeGraph typeGraph  
  return $ GG.graphGrammar initGraph (zip rulesNames rules)

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
