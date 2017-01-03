{-# LANGUAGE FlexibleContexts #-}
module Analysis.Processes
( generateGraphProcess , calculateRulesColimit,  findConflictsAndDependencies ) where

import Abstract.AdhesiveHLR
import Abstract.Cocomplete
import Abstract.DPO
import Abstract.Morphism
import Analysis.DiagramAlgorithms
import Data.List.NonEmpty (fromList)
import Data.List (partition)
import Data.Maybe (fromJust)
import Grammar.Core

generateGraphProcess :: (GenerateProcess m) => (String,[(String, Production m)],[ObjectFlow m]) -> [(String, Production m)]
generateGraphProcess (_,g,os) =
  let
    colimit = calculateRulesColimit ("",g,os)
    ruleNames = map fst g
    newRules = map productionTyping (map forgetRuleName colimit)
    forgetRuleName (_,b,c) = (b,c)
  in zip ruleNames newRules

calculateRulesColimit :: (GenerateProcess m) => (String,[(String, Production m)],[ObjectFlow m]) -> [NamedRuleWithMatches m]
calculateRulesColimit (_,g,os) =
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
    leftIOs = map (\o -> compose (snd $ spanMapping o) (fst' $ fromJust (lookup (consumer o) partial))) os
    rightIOs = map (\o -> compose (fst $ spanMapping o) (thd' $ fromJust (lookup (producer o) partial))) os
    objCop = objectFlowCoproduct os
    leftFamily = induceSpanMorphism objCop leftIOs
    rightFamily = induceSpanMorphism objCop rightIOs
    coreGraphMorphism = calculateCoequalizer leftFamily rightFamily
    hs2 = split $ map (`compose` coreGraphMorphism) hm
  in if null os then zip3 ruleNames rs hs1 else zip3 ruleNames rs hs2

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
split _ = error "list of morphisms should have length divisible by 3"

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

findConflictsAndDependencies :: GenerateProcess m => [NamedRuleWithMatches m] -> [(String, String, String)]
findConflictsAndDependencies rulesWithMatches = findConflicts pairs ++ findDepependies pairs --concatMap createCritical pairs
  where
    pairs = [(a,b) | a <- rulesWithMatches, b <- rulesWithMatches]

findConflicts :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [(String, String, String)]
findConflicts pairs = map buildDU deleteUse ++ map buildPF produceForbid
  where
    conflictCandidates = filter (\(a,b) -> validLeftRewritings (getRule a) (getRule b) (getMatch a, getMatch b)) pairs
    (deleteUse,produceForbidCandidates) = partition (\(x,y) -> myDeleteUse (getRule x) (getMatch x, getMatch y)) conflictCandidates
    produceForbid = filter (\(x,y) -> myProduceForbid (getRule y) (getComatch x, getMatch y)) produceForbidCandidates
    buildDU (a,b) = (getName a, getName b, "DeleteUse")
    buildPF (a,b) = (getName a, getName b, "ProduceForbid")

findDepependies :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [(String, String, String)]
findDepependies pairs = map buildPU produceUse ++ map buildDF deleteForbid
  where
    dependencyCandidates = filter (\(a,b) -> validRightLeftRewritings (getRule a) (getRule b) (getComatch a, getMatch b)) pairs
    (produceUse,deleteForbidCandidates) = partition (\(x,y) -> myProduceUse (getRule x) (getComatch x, getMatch y)) dependencyCandidates
    deleteForbid = filter (\(x,y) -> myDeleteForbid (getRule y) (getMatch x, getMatch y)) deleteForbidCandidates
    buildPU (a,b) = (getName a, getName b, "ProduceUse")
    buildDF (a,b) = (getName a, getName b, "DeleteForbid")


createCritical :: GenerateProcess m => (NamedRuleWithMatches m, NamedRuleWithMatches m) -> [(String, String, String)]
createCritical (a,b) = deleteUse ++ produceForbid ++ produceUse ++ deleteForbid
  where
    deleteUse = [(getName a, getName b, "DeleteUse") | isDeleteUse' (getRule a) (getMatch a, getMatch b)]
    produceForbid = [(getName a, getName b, "ProduceForbid") | isProduceForbid' (getRule a) (getRule b) (getMatch a, getComatch a, getMatch b)]
    produceUse = [(getName a, getName b, "ProduceUse") | isProduceUse' (getRule a) (getComatch a, getMatch b)]
    deleteForbid = [(getName a, getName b, "DeleteForbid") | isDeleteForbid' (getRule a) (getRule b) (getMatch a, getComatch a, getMatch b)]

conf :: MorphismsConfig
conf = MorphismsConfig MonoMatches MonomorphicNAC

type NamedRuleWithMatches m = (String, Production m, (m,m,m))

getName :: NamedRuleWithMatches m -> String
getName = fst'

getRule :: NamedRuleWithMatches m -> Production m
getRule = snd'

getMatch :: NamedRuleWithMatches m -> m
getMatch = fst' . thd'

getComatch :: NamedRuleWithMatches m -> m
getComatch = thd' . thd'

validLeftRewritings :: GenerateProcess m => Production m -> Production m-> (m, m) -> Bool
validLeftRewritings p1 p2 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in satisfyRewritingConditions conf (p1,m1'') (p2,m2'')

validRightLeftRewritings :: GenerateProcess m => Production m -> Production m-> (m, m) -> Bool
validRightLeftRewritings p1 p2 (m1',m2) =
  let
    p1' = invertProduction conf p1 -- should we invert the rule or just test the gluing conditiond
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in satisfyRewritingConditions conf (p1', m1'') (p2,m2'')

myDeleteUse :: GenerateProcess m => Production m -> (m, m) -> Bool
myDeleteUse p1 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in isDeleteUse conf p1 (m1'',m2'')

myProduceUse :: GenerateProcess m => Production m -> (m, m) -> Bool
myProduceUse p1 (m1',m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in isProduceUse conf p1 (m1'',m2'')

myProduceForbid :: (GenerateProcess m) => Production m -> (m,m) -> Bool
myProduceForbid p2 (m1',m2) =
  let
    (_,m2'') = restrictMorphisms (m1',m2)
  in not (satisfiesNACs conf p2 m2'')

myDeleteForbid :: (GenerateProcess m) => Production m -> (m,m) -> Bool
myDeleteForbid p2 (m1,m2) =
  let
    (_,m2'') = restrictMorphisms (m1,m2)
  in not (satisfiesNACs conf p2 m2'')

isDeleteUse' :: GenerateProcess m => Production m -> (m, m) -> Bool
isDeleteUse' p1 (m1,m2)= isDeleteUse conf p1 (restrictMorphisms (m1,m2))

isProduceForbid' :: (GenerateProcess m) => Production m -> Production m -> (m,m,m) -> Bool
isProduceForbid' p1 p2 (m1,m1',m2) = not (isDeleteUse' p1 (m1,m2)) && auxiliarProduceForbid conf p2 (m1',m2)

-- | Auxiliary function to test for produce forbid, remove error in final version
auxiliarProduceForbid :: (GenerateProcess m) => MorphismsConfig -> Production m -> (m,m) -> Bool
auxiliarProduceForbid conf p2 (m1',m2) =
  let
    (_,m2'') = restrictMorphisms (m1',m2)
    result = satisfiesNACs conf p2 m2''
    teste = if satisfiesGluingConditions conf p2 m2'' then not result else error "should satisfy gluing conditions"
  in satisfiesNACs conf p2 m2''

isProduceUse' :: GenerateProcess m => Production m -> (m, m) -> Bool
isProduceUse' p1 (m1',m2) = isProduceUse conf p1 (restrictMorphisms (m1',m2))

isDeleteForbid' :: (GenerateProcess m) => Production m -> Production m -> (m,m,m) -> Bool
isDeleteForbid' p1 p2 (m1,m1',m2) = not (isProduceUse' p1 (m1',m2)) && auxiliarDeleteForbid conf p2 (m1,m2)

auxiliarDeleteForbid :: (GenerateProcess m) => MorphismsConfig -> Production m -> (m,m) -> Bool
auxiliarDeleteForbid conf p2 (m1,m2) =
  let
    (_,m2'') = restrictMorphisms (m1,m2)
    in satisfiesNACs conf p2 m2''

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

thd' :: (a,b,c) -> c
thd' (_,_,c) = c
