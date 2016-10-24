module ParallelIndependence
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
  { duFlag       :: Bool
  , siFlag       :: Bool
  }

options :: Parser Options
options = Options
  <$> duf
  <*> sif

duf :: Parser Bool
duf = flag False True
    ( long "delete-use"
    <> help "use delete-use instead pullbacks")

sif :: Parser Bool
sif = flag False True
    ( long "seq-ind"
    <> help "use sequentially independent instead parallel")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = dpoConfig globalOpts
        useConstrains = False

    (gg,_) <- XML.readGrammar (inputFile globalOpts) useConstrains dpoConf
    
    putStrLn "Warning: NACs are not considered in this analysis."
    putStrLn ""
    
    let du = duFlag opts
        comp = False -- flag to compare if deleteuse and pullbacks are generating the same results
        algorithm =
          case (siFlag opts) of
            False -> Parallel
            True -> Sequentially
        --rules = concatMap (replicate 1) $ map snd (GG.rules gg)
        rules = map snd (GG.rules gg)
        analysisDU = pairwiseCompareUpperReflected (isIndependent algorithm DeleteUse dpoConf) rules
        analysisPB = pairwiseCompareUpperReflected (isIndependent algorithm Pullback dpoConf) rules
    
    putStrLn $ "Delete-use flag: " ++ (show du)
    putStrLn $ "Comparison flag: " ++ (show comp)
    putStrLn $ "Length of the set of rules: " ++ (show (length rules))
    putStrLn ""
    
    when comp $ putStrLn $ "Check if pullback and delete-use algorithms result in the same matrix: " ++ show (analysisPB == analysisDU)
    
    when (not comp) $ putStrLn ("Matrix of all pairs of rules (True means this pair is "++ show algorithm ++ " Independent):")
    when (not comp && du) $ print $ analysisDU
    when (not comp && not du) $ print $ analysisPB

-- | Applies a function on the upper triangular matrix and reflects the result in the lower part
pairwiseCompareUpperReflected :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompareUpperReflected compare items = elementwise op m (transpose m)
  where
    op = \a b -> or [a,b]
    m = matrix (length items) (length items) $ \(i,j) ->
          if i > j
            then False
            else compare (items !! (i-1)) (items !! (j-1))
