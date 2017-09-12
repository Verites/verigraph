module Util where

import           Control.Parallel                (par)
import           Data.Matrix

import           Abstract.Category.Adhesive
import           Abstract.Category.Finitary
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs

data AnalysisType = Both | Conflicts | Dependencies | None deriving (Eq)

calculateConflicts :: AnalysisType -> Bool
calculateConflicts flag = flag `elem` [Both,Conflicts]

calculateDependencies :: AnalysisType -> Bool
calculateDependencies flag = flag `elem` [Both,Dependencies]

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  fromList (length items) (length items)
    (parallelMap (uncurry compare) [(a,b) | a <- items, b <- items])

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
                       in r `par` r : parallelMap f xs
parallelMap _ _      = []

printAnalysis :: (E'PairCofinitary morph, DPO morph, MInitialPushout morph, Complete morph, Cocomplete morph) =>
  Bool -> AnalysisType -> MorphismsConfig morph -> [Production morph] -> IO ()
printAnalysis essential action dpoConf rules =
  let findAllEssentialProduceDangling _ _ _ = []
      findAllEssentialProduceForbid _ _ _ = []
      essentialConfMatrix = analysisMatrix dpoConf rules
        findAllEssentialDeleteUse findAllEssentialProduceDangling findAllEssentialProduceForbid
        "Essential Delete-Use" "Essential Produce-Dangling" "Essential Produce-Forbid" "Essential Conflicts"
      confMatrix = analysisMatrix dpoConf rules
        findAllDeleteUse findAllProduceDangling findAllProduceForbid
        "Delete-Use" "Produce-Dangling" "Produce-Forbid" "Conflicts"
      depMatrix = triDepMatrix
      triDepMatrix = analysisMatrix dpoConf rules
        findAllProduceUse findAllRemoveDangling findAllDeleteForbid
        "Produce-Use" "Remove-Dangling" "Deliver-Forbid" "Triggered Dependencies"

  in mapM_
       putStrLn $
       (case (essential, calculateConflicts action) of
         (True, True)  -> essentialConfMatrix
         (False, True) -> confMatrix
         _             -> []
       )
       ++ (if calculateDependencies action then depMatrix else [])

-- Receives functions and theirs names,
-- and returns they applicated to the rules
analysisMatrix :: MorphismsConfig morph -> [Production morph]
  -> (MorphismsConfig morph -> Production morph -> Production morph -> [cps])
  -> (MorphismsConfig morph -> Production morph -> Production morph -> [cps])
  -> (MorphismsConfig morph -> Production morph -> Production morph -> [cps])
  -> String -> String -> String -> String
  -> [String]
analysisMatrix dpoConf rules f1 f2 f3 n1 n2 n3 n4 =
  let f1Matrix = pairwiseCompare (f1 dpoConf) rules
      f2Matrix = pairwiseCompare (f2 dpoConf) rules
      f3Matrix = pairwiseCompare (f3 dpoConf) rules
      finalMatrix =
        liftMatrix3
          (\x y z -> x ++ y ++ z)
          f1Matrix f2Matrix f3Matrix
  in  [ n1 ++ ":"
      , show (length <$> f1Matrix)
      , ""
      , n2 ++ ":"
      , show (length <$> f2Matrix)
      , ""
      , n3 ++ ":"
      , show (length <$> f3Matrix)
      , ""
      , "All " ++ n4 ++ ":"
      , show (length <$> finalMatrix)
      , ""]
