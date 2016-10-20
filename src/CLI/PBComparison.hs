module PBComparison
  ( Options
  , options
  , execute
  ) where

import           Analysis.ParallelIndependent
import           Control.Monad                         (when)
import           Data.Matrix                           hiding ((<|>))
import           GlobalOptions
import           Options.Applicative
import qualified TypedGraph.GraphGrammar               as GG
import qualified XML.GGXReader                         as XML

data Options = Options
  { pbFlag       :: Bool
  , compar       :: Bool
  }

data AnalysisType = Both | Conflicts | Dependencies | None deriving (Eq)

options :: Parser Options
options = Options
  <$> pbf
  <*> comp

pbf :: Parser Bool
pbf = flag False True
    ( long "pb"
    <> help "PB")

comp :: Parser Bool
comp = flag False True
    ( long "comp"
    <> help "result comparison")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = dpoConfig globalOpts

    (gg,_) <- XML.readGrammar (inputFile globalOpts) False dpoConf
    
    let pb = pbFlag opts
        comp = compar opts
        rules = concatMap (replicate 8) $ map snd (GG.rules gg)
        analysisDU = pairwiseCompareUpper (isParallelIndependentDU dpoConf) rules
        analysisPB = pairwiseCompareUpper (isParallelIndependentPB dpoConf) rules
    
    putStrLn $ "PB flag: " ++ (show pb)
    putStrLn $ "Comp flag: " ++ (show comp)
    putStrLn $ "length rules: " ++ (show (length rules))
    putStrLn ""
    
    when comp $ print $ analysisPB == analysisDU
    when (not comp && not pb) $ print $ analysisDU
    when (not comp && pb) $ print $ analysisPB

pairwiseCompareUpper :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompareUpper compare items =
  matrix (length items) (length items) $ \(i,j) -> if i > j then False else
    compare (items !! (i-1)) (items !! (j-1))
