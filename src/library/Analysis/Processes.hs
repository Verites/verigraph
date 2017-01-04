{-# LANGUAGE FlexibleContexts #-}
module Analysis.Processes
( generateGraphProcess
, calculateRulesColimit
, findConflictsAndDependencies ) where

import Abstract.AdhesiveHLR
import Abstract.DPO
import Abstract.DPO.Process
import Analysis.DiagramAlgorithms
import Data.List (partition)
import Grammar.Core

generateGraphProcess :: (GenerateProcess m) => (String,[(String, Production m)],[ObjectFlow m]) -> [(String, Production m)]
generateGraphProcess (_,g,os) =
  let
    colimit = calculateRulesColimit ("",g,os)
    ruleNames = map fst g
    newRules = map (productionTyping . forgetRuleName) colimit
    forgetRuleName (_,b,c) = (b,c)
  in zip ruleNames newRules

findConflictsAndDependencies :: GenerateProcess m => [NamedRuleWithMatches m] -> [(String, String, String)]
findConflictsAndDependencies rulesWithMatches = findConflicts pairs ++ findDepependies pairs --concatMap createCritical pairs
  where
    pairs = [(a,b) | a <- rulesWithMatches, b <- rulesWithMatches]

findConflicts :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [(String, String, String)]
findConflicts pairs = map buildDU deleteUse ++ map buildPF produceForbid
  where
    conflictCandidates = filter (\(a,b) -> validLeftRewritings (getRule a) (getRule b) (getMatch a, getMatch b)) pairs
    (deleteUse,produceForbidCandidates) = partition (\(x,y) -> isDeleteUse' (getRule x) (getMatch x, getMatch y)) conflictCandidates
    produceForbid = filter (\(x,y) -> isProduceForbid' (getRule y) (getComatch x, getMatch y)) produceForbidCandidates
    buildDU (a,b) = (getName a, getName b, "DeleteUse")
    buildPF (a,b) = (getName a, getName b, "ProduceForbid")

findDepependies :: GenerateProcess m => [(NamedRuleWithMatches m,NamedRuleWithMatches m)] -> [(String, String, String)]
findDepependies pairs = map buildPU produceUse ++ map buildDF deleteForbid
  where
    dependencyCandidates = filter (\(a,b) -> validRightLeftRewritings (getRule a) (getRule b) (getComatch a, getMatch b)) pairs
    (produceUse,deleteForbidCandidates) = partition (\(x,y) -> isProduceUse' (getRule x) (getComatch x, getMatch y)) dependencyCandidates
    deleteForbid = filter (\(x,y) -> isDeleteForbid' (getRule y) (getMatch x, getMatch y)) deleteForbidCandidates
    buildPU (a,b) = (getName a, getName b, "ProduceUse")
    buildDF (a,b) = (getName a, getName b, "DeleteForbid")

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
