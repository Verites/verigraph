module Abstract.DPO.Process
( Process(..)
, GenerateProcess(..))

where

import Abstract.Cocomplete
import Abstract.DPO.Core
import Abstract.DPO.Derivation
import Abstract.Morphism
import Data.List.NonEmpty (NonEmpty, fromList)

data Process m = Process
  { productions :: [Production m]
  , coreObject :: Obj m
  }

class (DPO m) => GenerateProcess m where
  retype :: (Derivation m, (m,m,m)) ->  Production m

  calculateProcess :: [Derivation m] -> Process m
  calculateProcess [] = error "Can not calculate process for empty list of derivations"
  calculateProcess ds =
    let gs = allCoproducts ds
        coEq = calcultateBottomColimit ds
        core = codomain coEq
        hs = reduce $ map (`compose` coEq) gs
        as = zip ds hs
     in Process (map retype as) core

calcultateBottomColimit :: (DPO m) => [Derivation m] -> m
calcultateBottomColimit ds = calculateNCoequalizer $ fromList [h1,h2,h3]
  where
    fs = sourcesCoproduct ds
    ls = getLeftBottomObjects ds
    rs = getRightBottomObjects ds
    gs = allCoproducts ds
    gs' = reduce gs
    (g1s, g2s, g3s) = groupMorphisms gs'
    h = induceSpanMorphism fs
    h1 = h $ zipWith compose ls g1s
    h2 = h g2s
    h3 = h $ zipWith compose rs g3s

getSources :: (DPO m) => [Derivation m] -> NonEmpty (Obj m)
getSources ds = fromList (getDObjects ds)

sourcesCoproduct :: (DPO m) => [Derivation m] -> [m]
sourcesCoproduct = calculateNCoproduct . getSources

getAll :: (DPO m) => [Derivation m] -> NonEmpty (Obj m)
getAll ds = fromList $ getAllBottomObjects ds

allCoproducts :: (DPO m) => [Derivation m] -> [m]
allCoproducts = calculateNCoproduct . getAll

groupMorphisms :: [(m,m,m)] -> ([m],[m],[m])
groupMorphisms [] = ([],[],[])
groupMorphisms fs = (f1,f2,f3)
  where
    f1 = concatMap (\(a,_,_) -> [a]) fs
    f2 = concatMap (\(_,b,_) -> [b]) fs
    f3 = concatMap (\(_,_,c) -> [c]) fs

reduce :: [m] -> [(m,m,m)]
reduce fs
  | length fs < 3 = []
  | otherwise = (head fs, fs !! 1, fs !! 2) : reduce (rest fs)
    where
      rest = drop 2
