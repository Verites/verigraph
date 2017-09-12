{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Analysis.Processes
  ( generateGraphProcess
  , calculateRulesColimit
  , findConflictsAndDependencies
  ) where

import           Data.List                                (partition)

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Abstract.Rewriting.DPO.Process

findConflictsAndDependencies :: GenerateProcess morph => [NamedRuleWithMatches morph] -> [Interaction]
findConflictsAndDependencies rulesWithMatches = findConflicts pairs ++ findDependencies pairs --concatMap createCritical pairs
  where
    pairs = [(a,b) | a <- rulesWithMatches, b <- rulesWithMatches]

findConflicts :: GenerateProcess morph => [(NamedRuleWithMatches morph,NamedRuleWithMatches morph)] -> [Interaction]
findConflicts pairs = map buildDU deleteUse ++ map buildPF produceForbidOneNac
  where
    conflictCandidates = filter (\(a,b) -> validLeftRewritings (getRule a) (getRule b) (getMatch a, getMatch b)) pairs
    (deleteUse,produceForbidCandidates) = partition (\(x,y) -> isDeleteUse' (getRule x) (getMatch x, getMatch y)) conflictCandidates
    produceForbidOneNac = filter (\(x,(y,_)) -> isProduceForbid' (getRule y) (getComatch x, getMatch y)) (concatMap expandeProductions produceForbidCandidates)
    buildDU (a,b) = Interaction (getName a) (getName b) DeleteUse Nothing
    buildPF (a,(b,c)) = Interaction (getName a) (getName b) ProduceForbid (Just c)

findDependencies :: GenerateProcess morph => [(NamedRuleWithMatches morph,NamedRuleWithMatches morph)] -> [Interaction]
findDependencies pairs = map buildPU produceUse ++ map buildDF deleteForbidOneNac
  where
    dependencyCandidates = filter (\(a,b) -> validRightLeftRewritings (getRule a) (getRule b) (getComatch a, getMatch b)) pairs
    (produceUse,deleteForbidCandidates) = partition (\(x,y) -> isProduceUse' (getRule x) (getComatch x, getMatch y)) dependencyCandidates
    deleteForbidOneNac = filter (\(x,(y,_)) -> isDeleteForbid' (getRule y) (getMatch x, getMatch y)) (concatMap expandeProductions deleteForbidCandidates)
    buildPU (a,b) = Interaction (getName a) (getName b) ProduceUse Nothing
    buildDF (a,(b,c)) = Interaction (getName a) (getName b) DeleteForbid (Just c)

expandNACs :: NamedRuleWithMatches morph -> [(NamedRuleWithMatches morph, Int)]
expandNACs (name,production,matches) = zip (map (buildNamedProduction (name,production,matches)) (nacs production)) [0..]
  where
    buildNamedProduction (name,production,matches) n = (name, production { nacs = [n] }, matches)

expandeProductions :: (NamedRuleWithMatches morph, NamedRuleWithMatches morph) -> [(NamedRuleWithMatches morph, (NamedRuleWithMatches morph, Int))]
expandeProductions (a,b) = map (\x -> (a,x)) (expandNACs b)

conf :: forall morph. Category morph => MorphismsConfig morph
conf = MorphismsConfig (monic @morph)

validLeftRewritings :: GenerateProcess morph => Production morph -> Production morph-> (morph,morph) -> Bool
validLeftRewritings p1 p2 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in satisfiesRewritingConditions conf p1 m1'' && satisfiesRewritingConditions conf p2 m2''

validRightLeftRewritings :: GenerateProcess morph => Production morph -> Production morph-> (morph,morph) -> Bool
validRightLeftRewritings p1 p2 (m1',m2) =
  let
    p1' = invertProduction conf p1 -- should we invert the rule or just test the gluing conditiond
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in satisfiesRewritingConditions conf p1' m1'' && satisfiesRewritingConditions conf p2 m2''

isDeleteUse' :: GenerateProcess morph => Production morph -> (morph,morph) -> Bool
isDeleteUse' p1 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in isDeleteUse conf p1 (m1'',m2'')

isProduceUse' :: GenerateProcess morph => Production morph -> (morph,morph) -> Bool
isProduceUse' p1 (m1',m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in isProduceUse conf p1 (m1'',m2'')

isProduceForbid' :: (GenerateProcess morph) => Production morph -> (morph,morph) -> Bool
isProduceForbid' p2 (m1',m2) =
  let
    (_,m2'') = restrictMorphisms (m1',m2)
  in not (satisfiesNACs conf p2 m2'')

isDeleteForbid' :: (GenerateProcess morph) => Production morph -> (morph,morph) -> Bool
isDeleteForbid' p2 (m1,m2) =
  let
    (_,m2'') = restrictMorphisms (m1,m2)
  in not (satisfiesNACs conf p2 m2'')
