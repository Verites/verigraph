module TypedGraph.GraphProcess
( singleProcess
, calculateProcess
, sourcesCoproduct
, allCoproducts
, groupMorphisms
, reduce
)

where

import Abstract.Cocomplete
import Abstract.DPO
import Abstract.DPO.Process
import Abstract.DPO.Derivation
import Abstract.Morphism
import qualified Data.List.NonEmpty as NE
import Graph.GraphMorphism
import TypedGraph.Graph ()
import TypedGraph.GraphRule ()
import TypedGraph.Morphism as TGM

singleProcess :: Derivation (TypedGraphMorphism a b) -> Process (TypedGraphMorphism a b)
singleProcess derivation =
  let d1 = dToG derivation
      d2 = dToH derivation
      (d1',d2') = calculatePushout d1 d2
      core = codomain d1'
      rule = production derivation
      oldL = getLHS rule
      oldR = getRHS rule
      mappingL = mapping oldL
      mappingR = mapping oldR
      m = match derivation
      h = comatch derivation
      newLType = compose (mapping m) (mapping d2')
      newRType = compose (mapping h) (mapping d1')
      newKType = compose mappingL newLType
      newL = buildTypedGraphMorphism newKType newLType mappingL
      newR = buildTypedGraphMorphism newKType newRType mappingR
      newProduction = buildProduction newL newR []
   in Process [newProduction] core

retype :: (Derivation (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retype (derivation, (g1,_,g3)) = newProduction
  where
    p = production derivation
    oldL = getLHS p
    oldR = getRHS p
    mappingL = mapping oldL
    mappingR = mapping oldR
    m = match derivation
    h = comatch derivation
    newLType = compose (mapping m) (mapping g1)
    newRType = compose (mapping h) (mapping g3)
    newKType = compose mappingL newLType -- change it to use cl2
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = buildProduction newL newR []

calculateProcess :: [Derivation (TypedGraphMorphism a b)] -> Process(TypedGraphMorphism a b)
calculateProcess [] = error "Can not calculate process for empty list of derivations"
calculateProcess ds =
  let fs = sourcesCoproduct ds
      ls = getLeftBottomObjects ds
      rs = getRightBottomObjects ds
      gs = allCoproducts ds
      gs' = reduce gs
      (g1s, g2s, g3s) = groupMorphisms gs'
      h = induceSpanMorphism fs
      h1 = h $ zipWith compose ls g1s
      h2 = h g2s
      h3 = h $ zipWith compose rs g3s
      coEq = calculateNCoequalizer $ NE.fromList [h1,h2,h3]
      core = codomain coEq
      hs = reduce $ map (`compose` coEq) gs
      as = zip ds hs
   in Process (map retype as) core

getSources :: [Derivation (TypedGraphMorphism a b)] -> NE.NonEmpty (GraphMorphism a b)
getSources ds = NE.fromList (getDObjects ds)

sourcesCoproduct :: [Derivation (TypedGraphMorphism a b)] -> [TypedGraphMorphism a b]
sourcesCoproduct = calculateNCoproduct . getSources

getAll :: [Derivation (TypedGraphMorphism a b)] -> NE.NonEmpty (GraphMorphism a b)
getAll ds = NE.fromList $ getAllBottomObjects ds

allCoproducts :: [Derivation (TypedGraphMorphism a b)] -> [TypedGraphMorphism a b]
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
