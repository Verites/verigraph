module ParallelIndependence
  ( Options
  , options
  , execute
  ) where

import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import           Category.TypedGraphRule.JointlyEpimorphisms ()
import           Control.Monad                               (unless, when)
import           Data.Matrix                                 hiding ((<|>))
import           Data.Monoid                                 ((<>))
import           GlobalOptions
import           Options.Applicative
import           Rewriting.DPO.TypedGraph
import           Rewriting.DPO.TypedGraphRule
import qualified XML.GGXReader                               as XML

data Options = Options
  { siFlag   :: Bool
  , soFlag   :: Bool
  , calcCond :: Algorithm
  }

options :: Parser Options
options = Options
  <$> sif
  <*> sof
  <*> condition

sif :: Parser Bool
sif = flag False True
    ( long "seq-ind"
    <> help "use sequentially independent instead parallel")

sof :: Parser Bool
sof = flag False True
    ( long "snd-order"
    <> help "use second-order productions instead first-order")

condition :: Parser Algorithm
condition =
  abovePB
  <|> aboveM
  <|> belowPB
  <|> belowM
  <|> abovePBISO

abovePB :: Parser Algorithm
abovePB = flag' AbovePullback
  ( long "above-pullback"
  <> help "Above of rule, with pullbacks" )

aboveM :: Parser Algorithm
aboveM = flag' AboveMorphism
  ( long "above-morphism"
  <> help "Above of rule, with morphisms" )

belowPB :: Parser Algorithm
belowPB = flag' BelowPullback
  ( long "below-pullback"
  <> help "Below of rule, with pullbacks" )

belowM :: Parser Algorithm
belowM = flag' BelowMorphism
  ( long "below-morphism"
  <> help "Below of rule, with morphisms" )

abovePBISO :: Parser Algorithm
abovePBISO = flag' AbovePullbackIso
  ( long "above-pullback-iso"
  <> help "Above of rule, with pullbacks ans isomorphisms" )

execute :: GlobalOptions -> Options -> IO ()
execute globalOptions opts = do
    let dpoConf = morphismsConf globalOptions
        useConstrains = False

    (fstOrdGG, sndOrdGG, _) <- XML.readGrammar (inputFile globalOptions) useConstrains dpoConf

    putStrLn "Warning: Produce-Forbid conflicts are not considered in this analysis."
    putStrLn ""

    let sndOrder = soFlag opts
        comp = False-- flag to compare if deleteuse and pullbacks are generating the same results
        algorithm =
          if siFlag opts then Sequentially else Parallel
        rules1 = map snd (productions fstOrdGG) :: [TypedGraphRule () ()]
        rules2 = map snd (productions sndOrdGG) :: [SndOrderRule () ()]

        analysisAbovePullBackFST = pairwiseCompareUpperReflected (isIndependent algorithm AbovePullback    dpoConf) rules1
        analysisBelowPullBackFST = pairwiseCompareUpperReflected (isIndependent algorithm BelowPullback    dpoConf) rules1
        analysisAboveMorphismFST = pairwiseCompareUpperReflected (isIndependent algorithm AboveMorphism    dpoConf) rules1
        analysisBelowMorphismFST = pairwiseCompareUpperReflected (isIndependent algorithm BelowMorphism    dpoConf) rules1
        analysisAbovePbIsoFST    = pairwiseCompareUpperReflected (isIndependent algorithm AbovePullbackIso dpoConf) rules1

        analysisAbovePullBackSND = pairwiseCompareUpperReflected (isIndependent algorithm AbovePullback    dpoConf) rules2
        analysisBelowPullBackSND = pairwiseCompareUpperReflected (isIndependent algorithm BelowPullback    dpoConf) rules2
        analysisAboveMorphismSND = pairwiseCompareUpperReflected (isIndependent algorithm AboveMorphism    dpoConf) rules2
        analysisBelowMorphismSND = pairwiseCompareUpperReflected (isIndependent algorithm BelowMorphism    dpoConf) rules2
        analysisAbovePbIsoSND    = pairwiseCompareUpperReflected (isIndependent algorithm AbovePullbackIso dpoConf) rules2


        (analysisAbovePullBack, analysisBelowPullBack, analysisAboveMorphism, analysisBelowMorphism, analysisAbovePbIso ) =
          if sndOrder
          then (analysisAbovePullBackSND, analysisBelowPullBackSND, analysisAboveMorphismSND, analysisBelowMorphismSND, analysisAbovePbIsoSND )
          else (analysisAbovePullBackFST, analysisBelowPullBackFST, analysisAboveMorphismFST, analysisBelowMorphismFST, analysisAbovePbIsoFST )

    putStrLn $ "Second-order flag: " ++ show sndOrder
    putStrLn $ "Length of the set of first-order productions: " ++ show (length rules1)
    putStrLn $ "Length of the set of second-order productions: " ++ show (length rules2)
    putStrLn ""

    when comp $ putStrLn $
      "Check if pullback and delete-use algorithms result in the same matrix: " ++
      show ( analysisAbovePullBack == analysisAboveMorphism &&
             analysisAbovePullBack == analysisBelowPullBack &&
             analysisAbovePullBack == analysisBelowMorphism &&
             analysisAbovePullBack == analysisAbovePbIso )


    unless comp $ putStrLn ("Matrix of all pairs of productions (True means this pair is "++ show algorithm ++ " Independent):\n")
    unless comp $ case calcCond opts of
                    AbovePullback ->
                      print analysisAbovePullBack

                    AboveMorphism ->
                      print analysisAboveMorphism

                    BelowPullback ->
                      print analysisBelowPullBack

                    BelowMorphism ->
                      print analysisBelowMorphism

                    AbovePullbackIso ->
                      print analysisAbovePbIso


-- | Applies a function on the upper triangular matrix
pairwiseCompareUpperReflected :: (a -> a -> Int) -> [a] -> Matrix Int
pairwiseCompareUpperReflected compare items =
  matrix (length items) (length items) $
    \(i,j) -> if i <= j then compare (items !! (i-1)) (items !! (j-1)) else 0
