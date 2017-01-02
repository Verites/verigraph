{-# LANGUAGE FlexibleContexts #-}
module Analysis.Processes
( generateGraphProcess ) where

import Abstract.Cocomplete
import Abstract.DPO
import Abstract.Morphism
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromJust)
import Grammar.Core

generateGraphProcess :: (GenerateProcess m) => (String,[(String, Production m)],[ObjectFlow m]) -> [(String, Production m)]
generateGraphProcess (_,g,os) =
  let
    ruleNames = map fst g
    rs = map snd g --- rules
    fs = ksCoproduct rs
    gs = allCoproduct rs
    h = induceSpanMorphism fs
    (g1s, g2s, g3s) = groupMorphisms $ split gs
    h1 = h $ zipWith compose (getLefts rs) g1s
    h2 = h g2s
    h3 = h $ zipWith compose (getRights rs) g3s
    coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
    hm = map (`compose` coEq) gs
    hs1 = split hm -- colimit of the rules themselves


    -- colimit (based on coequalizers) with object flows
    partial = zip ruleNames hs1
    fst' (a,_,_) = a
    thd' (_,_,a) = a
    leftIOs = map (\o -> compose (snd $ spanMapping o) (fst' $ fromJust (lookup (consumer o) partial))) os
    rightIOs = map (\o -> compose (fst $ spanMapping o) (thd' $ fromJust (lookup (producer o) partial))) os
    objCop = objectFlowCoproduct os
    leftFamily = induceSpanMorphism objCop leftIOs
    rightFamily = induceSpanMorphism objCop rightIOs
    coreGraphMorphism = calculateCoequalizer leftFamily rightFamily
    hs2 = split $ map (`compose` coreGraphMorphism) hm

    newRules = if null os then map productionTyping (zip rs hs1) else map productionTyping (zip rs hs2)
    in zip ruleNames newRules

objectFlowCoproduct :: (DPO m) => [ObjectFlow m] -> [m]
objectFlowCoproduct [] = []
objectFlowCoproduct flows =
  let
    intersectionObjects = fromList $ map (domain . fst . spanMapping) flows
  in calculateNCoproduct intersectionObjects

getLefts :: [Production m] -> [m]
getLefts = map getLHS

getRights :: [Production m] -> [m]
getRights = map getRHS

split :: [m] -> [(m,m,m)]
split [] = []
split (a:b:c:ds) = (a,b,c) : split ds

ksCoproduct :: (DPO m) => [Production m] -> [m]
ksCoproduct = calculateNCoproduct . fromList . getKs

allCoproduct :: (DPO m) => [Production m] -> [m]
allCoproduct = calculateNCoproduct . fromList . getAllObjects

getKs :: (DPO m) => [Production m] -> [Obj m]
getKs = map (domain . getLHS)

getAllObjects :: (DPO m) => [Production m] -> [Obj m]
getAllObjects = foldr (\x -> (++) [(codomain . getLHS) x, (domain . getLHS) x, (codomain . getRHS) x]) []

-- used to group the morphisms into families
groupMorphisms :: [(m,m,m)] -> ([m],[m],[m])
groupMorphisms [] = ([],[],[])
groupMorphisms fs = (f1,f2,f3)
  where
    f1 = concatMap (\(a,_,_) -> [a]) fs
    f2 = concatMap (\(_,b,_) -> [b]) fs
    f3 = concatMap (\(_,_,c) -> [c]) fs
