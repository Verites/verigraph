module ParallelIndependence
  ( Options
  , options
  , execute
  ) where

import           Analysis.ParallelIndependent
import           Control.Monad                (when,unless)
import           Data.Matrix                  hiding ((<|>))
import           GlobalOptions
import qualified Grammar.Core            as GG
import           Options.Applicative
import qualified XML.GGXReader                as XML

data Options = Options
  { duFlag :: Bool
  , siFlag :: Bool
  , soFlag       :: Bool
  }

options :: Parser Options
options = Options
  <$> duf
  <*> sif
  <*> sof

duf :: Parser Bool
duf = flag False True
    ( long "delete-use"
    <> help "use delete-use instead pullbacks")

sif :: Parser Bool
sif = flag False True
    ( long "seq-ind"
    <> help "use sequentially independent instead parallel")

sof :: Parser Bool
sof = flag False True
    ( long "snd-order"
    <> help "use second-order rules instead first-order")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts
        useConstrains = False

    (fstOrdGG, sndOrdGG, _) <- XML.readGrammar (inputFile globalOpts) useConstrains dpoConf

    putStrLn "Warning: Produce-Forbid conflicts are not considered in this analysis."
    putStrLn ""

    let du = duFlag opts
        sndOrder = soFlag opts
        comp = False -- flag to compare if deleteuse and pullbacks are generating the same results
        algorithm =
          if (siFlag opts) then Sequentially else Parallel
        --rules = concatMap (replicate 1) $ map snd (GG.rules gg)
        rules1 = map snd (GG.rules fstOrdGG)
        rules2 = map snd (GG.rules sndOrdGG)
        analysisDU1 = pairwiseCompareUpperReflected (isIndependent algorithm DeleteUse dpoConf) rules1
        analysisPB1 = pairwiseCompareUpperReflected (isIndependent algorithm Pullback dpoConf) rules1
        analysisDU2 = pairwiseCompareUpperReflected (isIndependent algorithm DeleteUse dpoConf) rules2
        analysisPB2 = pairwiseCompareUpperReflected (isIndependent algorithm Pullback dpoConf) rules2

        (analysisDU,analysisPB) =
          case sndOrder of
            False -> (analysisDU1,analysisPB1)
            True -> (analysisDU2,analysisPB2)
    
    putStrLn $ "Second-order flag: " ++ (show sndOrder)
    putStrLn $ "Length of the set of first-order rules: " ++ (show (length rules1))
    putStrLn $ "Length of the set of second-order rules: " ++ (show (length rules2))
    putStrLn ""

    when comp $ putStrLn $ "Check if pullback and delete-use algorithms result in the same matrix: " ++ show (analysisPB == analysisDU)

    unless comp $ putStrLn ("Matrix of all pairs of rules (True means this pair is "++ show algorithm ++ " Independent):")
    when (not comp && du) $ print analysisDU
    when (not comp && not du) $ print analysisPB

-- | Applies a function on the upper triangular matrix and reflects the result in the lower part
pairwiseCompareUpperReflected :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompareUpperReflected compare items = elementwise op m (transpose m)
  where
    op = \a b -> or [a,b]
    m = matrix (length items) (length items) $ \(i,j) ->
          not (i > j) && compare (items !! (i-1)) (items !! (j-1))
