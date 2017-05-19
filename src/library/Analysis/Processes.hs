{-# LANGUAGE FlexibleContexts #-}
module Analysis.Processes
  ( generateGraphProcess
  , calculateRulesColimit
  , findConflictsAndDependencies
  ) where

import           Category.AdhesiveHLR
import           Category.DPO
import           Category.DPO.Process
import           Analysis.DiagramAlgorithms
import           Data.List                  (partition)

findConflictsAndDependencies :: GenerateProcess m => [NamedRuleWithMatches m] -> [Interaction]
findConflictsAndDependencies rulesWithMatches = findConflicts pairs ++ findDependencies pairs --concatMap createCritical pairs
  where
    pairs = [(a,b) | a <- rulesWithMatches, b <- rulesWithMatches]

findConflicts :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [Interaction]
findConflicts pairs = map buildDU deleteUse ++ map buildPF produceForbidOneNac
  where
    conflictCandidates = filter (\(a,b) -> validLeftRewritings (getRule a) (getRule b) (getMatch a, getMatch b)) pairs
    (deleteUse,produceForbidCandidates) = partition (\(x,y) -> isDeleteUse' (getRule x) (getMatch x, getMatch y)) conflictCandidates
    produceForbidOneNac = filter (\(x,(y,_)) -> isProduceForbid' (getRule y) (getComatch x, getMatch y)) (concatMap expandeProductions produceForbidCandidates)
    buildDU (a,b) = Interaction (getName a) (getName b) DeleteUse Nothing
    buildPF (a,(b,c)) = Interaction (getName a) (getName b) ProduceForbid (Just c)

findDependencies :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [Interaction]
findDependencies pairs = map buildPU produceUse ++ map buildDF deleteForbidOneNac
  where
    dependencyCandidates = filter (\(a,b) -> validRightLeftRewritings (getRule a) (getRule b) (getComatch a, getMatch b)) pairs
    (produceUse,deleteForbidCandidates) = partition (\(x,y) -> isProduceUse' (getRule x) (getComatch x, getMatch y)) dependencyCandidates
    deleteForbidOneNac = filter (\(x,(y,_)) -> isDeleteForbid' (getRule y) (getMatch x, getMatch y)) (concatMap expandeProductions deleteForbidCandidates)
    buildPU (a,b) = Interaction (getName a) (getName b) ProduceUse Nothing
    buildDF (a,(b,c)) = Interaction (getName a) (getName b) DeleteForbid (Just c)

expandNACs :: NamedRuleWithMatches m -> [(NamedRuleWithMatches m, Int)]
expandNACs (name,production,matches) = zip (map (buildNamedProduction (name,production,matches)) (getNACs production)) [0..]
  where
    buildNamedProduction (name,production,matches) n = (name, buildProduction (getLHS production) (getRHS production) [n], matches)

expandeProductions :: (NamedRuleWithMatches m, NamedRuleWithMatches m) -> [(NamedRuleWithMatches m, (NamedRuleWithMatches m, Int))]
expandeProductions (a,b) = map (\x -> (a,x)) (expandNACs b)

conf :: MorphismsConfig
conf = MorphismsConfig MonoMatches MonomorphicNAC

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

isDeleteUse' :: GenerateProcess m => Production m -> (m, m) -> Bool
isDeleteUse' p1 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in isDeleteUse conf p1 (m1'',m2'')

isProduceUse' :: GenerateProcess m => Production m -> (m, m) -> Bool
isProduceUse' p1 (m1',m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in isProduceUse conf p1 (m1'',m2'')

isProduceForbid' :: (GenerateProcess m) => Production m -> (m,m) -> Bool
isProduceForbid' p2 (m1',m2) =
  let
    (_,m2'') = restrictMorphisms (m1',m2)
  in not (satisfiesNACs conf p2 m2'')

isDeleteForbid' :: (GenerateProcess m) => Production m -> (m,m) -> Bool
isDeleteForbid' p2 (m1,m2) =
  let
    (_,m2'') = restrictMorphisms (m1,m2)
  in not (satisfiesNACs conf p2 m2'')
