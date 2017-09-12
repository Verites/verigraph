module ParallelIndependence
  ( Options
  , options
  , execute
  ) where

import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import           Control.Monad                (unless, when)
import           Data.Matrix                  hiding ((<|>))
import           Data.Monoid                  ((<>))
import           GlobalOptions
import           Options.Applicative
import           Rewriting.DPO.TypedGraphRule
import qualified XML.GGXReader                as XML

data Options = Options
  { duFlag :: Bool
  , c1Flag :: Bool
  , siFlag :: Bool
  , soFlag :: Bool
  }

options :: Parser Options
options = Options
  <$> duf
  <*> c1f
  <*> sif
  <*> sof

duf :: Parser Bool
duf = flag False True
    ( long "delete-use"
    <> help "use delete-use instead pullbacks")

c1f :: Parser Bool
c1f = flag False True
    ( long "cond1"
    <> help "use cond1 instead pullbacks")

sif :: Parser Bool
sif = flag False True
    ( long "seq-ind"
    <> help "use sequentially independent instead parallel")

sof :: Parser Bool
sof = flag False True
    ( long "snd-order"
    <> help "use second-order productions instead first-order")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts
        useConstrains = False

    (fstOrdGG, sndOrdGG, _) <- XML.readGrammar (inputFile globalOpts) useConstrains dpoConf

    putStrLn "Warning: Produce-Forbid conflicts are not considered in this analysis."
    putStrLn ""

    let du = duFlag opts
        c1 = c1Flag opts
        sndOrder = soFlag opts
        comp = False -- flag to compare if deleteuse and pullbacks are generating the same results
        algorithm =
          if siFlag opts then Sequentially else Parallel
        --productions = concatMap (replicate 1) $ map snd (productions gg)
        rules1 = map snd (productions fstOrdGG)
        rules2 = map snd (productions sndOrdGG)
        analysisC11 = pairwiseCompareUpperReflected (isIndependent algorithm Cond1 dpoConf) rules1
        analysisPB1 = pairwiseCompareUpperReflected (isIndependent algorithm Cond2 dpoConf) rules1
        analysisDU1 = pairwiseCompareUpperReflected (isIndependent algorithm Cond3 dpoConf) rules1
        dpoConf' = toSndOrderMorphismsConfig dpoConf
        analysisC12 = pairwiseCompareUpperReflected (isIndependent algorithm Cond1 dpoConf') rules2
        analysisPB2 = pairwiseCompareUpperReflected (isIndependent algorithm Cond2 dpoConf') rules2
        analysisDU2 = pairwiseCompareUpperReflected (isIndependent algorithm Cond3 dpoConf') rules2

        (analysisDU,analysisPB,analysisC1) =
          if sndOrder then
            (analysisDU2,analysisPB2,analysisC12)
          else
            (analysisDU1,analysisPB1,analysisC11)


    putStrLn $ "Second-order flag: " ++ show sndOrder
    putStrLn $ "Length of the set of first-order productions: " ++ show (length rules1)
    putStrLn $ "Length of the set of second-order productions: " ++ show (length rules2)
    putStrLn ""

    when comp $ putStrLn $ "Check if pullback and delete-use algorithms result in the same matrix: " ++ show (analysisPB == analysisDU)

    unless comp $ putStrLn ("Matrix of all pairs of productions (True means this pair is "++ show algorithm ++ " Independent):")
    when (not comp && du) $ print analysisDU
    when (not comp && c1) $ print analysisC1
    when (not comp && not du && not c1) $ print analysisPB

-- | Applies a function on the upper triangular matrix and reflects the result in the lower part
pairwiseCompareUpperReflected :: (a -> a -> Bool) -> [a] -> Matrix Bool
pairwiseCompareUpperReflected compare items =
    elementwise (||) m (transpose m)
  where
    m = matrix (length items) (length items) $ \(i,j) ->
          (i <= j) && compare (items !! (i-1)) (items !! (j-1))
