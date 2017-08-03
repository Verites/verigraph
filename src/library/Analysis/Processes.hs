{-# LANGUAGE FlexibleContexts #-}
module Analysis.Processes
  ( generateGraphProcess
  , calculateRulesColimit
  , findConflictsAndDependencies
  ) where

import           Control.Monad

import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Abstract.Rewriting.DPO.Process
import           Util.Monad


findConflictsAndDependencies :: GenerateProcess cat morph => [NamedRuleWithMatches cat morph] -> cat [Interaction]
findConflictsAndDependencies rulesWithMatches = (++) <$> findConflicts pairs <*> findDependencies pairs
  where
    pairs = [(a,b) | a <- rulesWithMatches, b <- rulesWithMatches]

findConflicts :: GenerateProcess cat morph => [(NamedRuleWithMatches cat morph,NamedRuleWithMatches cat morph)] -> cat [Interaction]
findConflicts pairs = do
  conflictCandidates <- filterM (\(a,b) -> validLeftRewritings (getRule a) (getRule b) (getMatch a, getMatch b)) pairs
  (deleteUse, produceForbidCandidates) <- partitionM (\(x,y) -> isDeleteUse' (getRule x) (getMatch x, getMatch y)) conflictCandidates
  produceForbidOneNac <- filterM (\(x,(y,_)) -> isProduceForbid' (getRule y) (getComatch x, getMatch y)) (concatMap expandeProductions produceForbidCandidates)
  return $ map buildDU deleteUse ++ map buildPF produceForbidOneNac
  where
    buildDU (a,b) = Interaction (getName a) (getName b) DeleteUse Nothing
    buildPF (a,(b,c)) = Interaction (getName a) (getName b) ProduceForbid (Just c)

findDependencies :: GenerateProcess cat morph => [(NamedRuleWithMatches cat morph,NamedRuleWithMatches cat morph)] -> cat [Interaction]
findDependencies pairs = do
  dependencyCandidates <- filterM (\(a,b) -> validRightLeftRewritings (getRule a) (getRule b) (getComatch a, getMatch b)) pairs
  (produceUse,deleteForbidCandidates) <- partitionM (\(x,y) -> isProduceUse' (getRule x) (getComatch x, getMatch y)) dependencyCandidates
  deleteForbidOneNac <- filterM (\(x,(y,_)) -> isDeleteForbid' (getRule y) (getMatch x, getMatch y)) (concatMap expandeProductions deleteForbidCandidates)
  return $ map buildPU produceUse ++ map buildDF deleteForbidOneNac
  where
    buildPU (a,b) = Interaction (getName a) (getName b) ProduceUse Nothing
    buildDF (a,(b,c)) = Interaction (getName a) (getName b) DeleteForbid (Just c)

expandNACs :: NamedRuleWithMatches cat morph -> [(NamedRuleWithMatches cat morph, Int)]
expandNACs (name,production,matches) = zip (map (buildNamedProduction (name,production,matches)) (nacs production)) [0..]
  where
    buildNamedProduction (name,production,matches) n = (name, production { nacs = [n] }, matches)

expandeProductions :: (NamedRuleWithMatches cat morph, NamedRuleWithMatches cat morph) -> [(NamedRuleWithMatches cat morph, (NamedRuleWithMatches cat morph, Int))]
expandeProductions (a,b) = map (\x -> (a,x)) (expandNACs b)

{- --FIXME: restrict applicability of processes to monic matches? how?
conf :: MorphismsConfig
conf = MorphismsConfig Monomorphism MonomorphicNAC
-}

validLeftRewritings :: GenerateProcess cat morph => Production cat morph -> Production cat morph-> (morph,morph) -> cat Bool
validLeftRewritings p1 p2 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in satisfiesRewritingConditions p1 m1'' `andM` satisfiesRewritingConditions p2 m2''

validRightLeftRewritings :: GenerateProcess cat morph => Production cat morph -> Production cat morph-> (morph,morph) -> cat Bool
validRightLeftRewritings p1 p2 (m1',m2) = do
  p1' <- invertProduction p1 -- should we invert the rule or just test the gluing conditiond
  let (m1'',m2'') = restrictMorphisms (m1',m2)
  satisfiesRewritingConditions p1' m1'' `andM` satisfiesRewritingConditions p2 m2''

isDeleteUse' :: GenerateProcess cat morph => Production cat morph -> (morph,morph) -> cat Bool
isDeleteUse' p1 (m1,m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1,m2)
  in isDeleteUse p1 (m1'',m2'')

isProduceUse' :: GenerateProcess cat morph => Production cat morph -> (morph,morph) -> cat Bool
isProduceUse' p1 (m1',m2) =
  let
    (m1'',m2'') = restrictMorphisms (m1',m2)
  in isProduceUse p1 (m1'',m2'')

isProduceForbid' :: (GenerateProcess cat morph) => Production cat morph -> (morph,morph) -> cat Bool
isProduceForbid' p2 (m1',m2) =
  let
    (_,m2'') = restrictMorphisms (m1',m2)
  in not <$> satisfiesNACs p2 m2''

isDeleteForbid' :: (GenerateProcess cat morph) => Production cat morph -> (morph,morph) -> cat Bool
isDeleteForbid' p2 (m1,m2) =
  let
    (_,m2'') = restrictMorphisms (m1,m2)
  in not <$> satisfiesNACs p2 m2''
