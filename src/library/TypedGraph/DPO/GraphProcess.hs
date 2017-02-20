{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module TypedGraph.DPO.GraphProcess

( OccurenceGrammar (..)
, occurenceRelation
, filterRulesOccurenceRelation
, filterElementsOccurenceRelation
, generateOccurenceGrammar
, uniqueOrigin
, findOrder
, filterPotential
, findConcreteTrigger
, calculateNacRelations
)

where

import Abstract.AdhesiveHLR
import Abstract.DPO
import Abstract.DPO.Process
import Abstract.Morphism as M
import Analysis.DiagramAlgorithms
import Data.List as L hiding (union)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set as S
import Data.Tuple (swap)
import Equivalence.EquivalenceClasses
import Grammar.Core
import Graph.Graph (NodeId, EdgeId, Graph)
import qualified Graph.GraphMorphism as GM
import Util.Closures as C
import TypedGraph.DPO.GraphRule
import TypedGraph.Graph
import TypedGraph.Morphism as TGM

instance GenerateProcess (TypedGraphMorphism a b) where
  typing = retypeProduction
  productionTyping = retype
  restrictMorphisms = restrictMorphisms'
  restrictMorphism = restrictMorphism'

data OccurenceGrammar a b = OccurenceGrammar {
  singleTypedGrammar :: Grammar (TypedGraphMorphism a b)
, originalRulesWithMatches :: [NamedRuleWithMatches (TypedGraphMorphism a b)]
, doubleType :: TypedGraphMorphism a b
, originRelation :: Relation
, concreteRelation :: Relation
, restrictRelation :: AbstractRelation
}

initialGraph = start . singleTypedGrammar

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

type Relation = S.Set(RelationItem, RelationItem)
type AbstractRelation = S.Set ((RelationItem, RelationItem), (RelationItem, RelationItem))

uniqueOrigin :: [NamedProduction (TypedGraphMorphism a b)] -> Bool
uniqueOrigin rules = not (repeated createdList) && not (repeated deletedList)
  where
    creationAndDeletion = S.filter isRuleAndElement $ unions $ L.map creationAndDeletionRelation rules
    isCreated a = case a of
      (Rule _, _) -> True
      _ -> False
    (created, deleted) = S.partition isCreated creationAndDeletion
    createdList = S.toList $ S.map snd created
    deletedList = S.toList $ S.map fst deleted

findOrder :: Relation -> Maybe [RelationItem]
findOrder = tsort

repeated :: (Eq a) => [a] -> Bool
repeated [] = False
repeated (x:xs) = x `elem` xs || repeated xs

strictRelation :: [NamedProduction (TypedGraphMorphism a b)] -> Relation
strictRelation = unions . L.map creationAndDeletionRelation

isRuleAndElement :: (RelationItem, RelationItem) -> Bool
isRuleAndElement (a,b) = case (a,b) of
                      (Rule _, Rule _) -> False
                      (Rule _, _)      -> True
                      (_, Rule _)      -> True
                      _                -> False

occurenceRelation :: [NamedProduction (TypedGraphMorphism a b)] -> Relation
occurenceRelation rules =
  let
    b = strictRelation rules
    b' = creationAndPreservationRelation rules b
    b'' = preservationAndDeletionRelation rules b
  in buildTransitivity (unions [b,b',b''])

buildTransitivity :: Relation -> Relation
buildTransitivity = monadToSet . transitiveClosure . setToMonad

filterRulesOccurenceRelation :: Relation -> Relation
filterRulesOccurenceRelation = S.filter bothRules
  where
    bothRules (x,y) = case (x,y) of
                        (Rule _, Rule _) -> True
                        _                -> False

filterElementsOccurenceRelation :: Relation -> Relation
filterElementsOccurenceRelation = S.filter bothElements
  where
    bothElements (x,y) = case (x,y) of
                        (Rule _, _) -> False
                        (_, Rule _) -> False
                        _           -> True

createdElements :: Relation -> Set RelationItem
createdElements elementsRelation =
  let
    m = setToMonad elementsRelation
    c = relationImage m
    created = monadToSet c
   in created

generateOccurenceGrammar :: RuleSequence (TypedGraphMorphism a b) -> OccurenceGrammar a b
generateOccurenceGrammar sequence = OccurenceGrammar singleGrammar originalRulesWithMatches doubleType cdRelation relation empty
  where
    originalRulesWithMatches = calculateRulesColimit sequence -- TODO: unify this two functions
    newRules = generateGraphProcess sequence
    cdRelation = S.filter isRuleAndElement $ strictRelation newRules
    relation = occurenceRelation newRules
    created = createdElements . filterElementsOccurenceRelation $ relation
    doubleType = getLHS . snd . head $ newRules
    coreGraph = codomain . codomain $ doubleType
    startGraph = removeElements coreGraph created
    singleGrammar = grammar startGraph [] newRules

isNode :: RelationItem -> Bool
isNode x = case x of
           Node _ -> True
           _      -> False

removeElements :: Graph a b -> Set RelationItem -> TypedGraph a b
removeElements coreGraph elementsToRemove =
  let
    (n,e) = S.partition isNode elementsToRemove
    nodes = S.map (\(Node x) -> x) n
    edges = S.map (\(Edge x) -> x) e
  in S.foldr GM.removeNodeFromDomain (S.foldr GM.removeEdgeFromDomain (M.id coreGraph) edges) nodes

-- use with the retyped rules
creationAndDeletionRelation :: NamedProduction (TypedGraphMorphism a b) -> Relation
creationAndDeletionRelation (name,rule) =
  let
    ln = deletedNodes rule
    le = deletedEdges rule
    rn = createdNodes rule
    re = createdEdges rule
    nodesAndEdges = [(Node a, Node b) | a <- ln, b <- rn] ++ [(Edge a, Edge b) | a <- le, b <- re]
                 ++ [(Node a, Edge b) | a <- ln, b <- re] ++ [(Edge a, Node b) | a <- le, b <- rn]
    putRule rel = [(fst rel, Rule name), (Rule name, snd rel)]
    withRules = concatMap putRule nodesAndEdges
  in S.fromList $ nodesAndEdges ++ withRules

creationAndPreservationRelation :: [NamedProduction (TypedGraphMorphism a b)] -> Relation -> Relation
creationAndPreservationRelation rules cdRelation =
  let
    creationCase x = case fst x of
      Rule _ -> True
      _      -> False
    created = S.filter creationCase cdRelation
    result = L.map (relatedByCreationAndPreservation created) rules
  in S.unions result

relatedByCreationAndPreservation :: Relation -> NamedProduction (TypedGraphMorphism a b) -> Relation
relatedByCreationAndPreservation relation namedRule
  | S.null relation = S.empty
  | otherwise =
  let
    r = getElem relation
    rs = getTail relation
    name = getProductionName namedRule
    nodes = preservedNodes (getProduction namedRule)
    edges = preservedEdges (getProduction namedRule)
    related (_,c) nodes edges = case c of
                                Node x -> nodes `intersect` [x] /= []
                                Edge x -> edges `intersect` [x] /= []
                                _      -> False
  in if related r nodes edges then singleton (fst r, Rule name) else relatedByCreationAndPreservation rs namedRule

preservationAndDeletionRelation :: [NamedProduction (TypedGraphMorphism a b)] -> Relation -> Relation
preservationAndDeletionRelation rules cdRelation =
  let
    deletionCase x = case snd x of
      Rule _ -> True
      _      -> False
    deleting = S.filter deletionCase cdRelation
    result = L.map (relatedByPreservationAndDeletion deleting) rules
  in S.unions result

filterPotential :: [Interaction] -> Set Interaction
filterPotential conflictsAndDependencies =
  S.filter (\i -> interactionType i == ProduceForbid || interactionType i == DeleteForbid) $ fromList conflictsAndDependencies

type RuleWithMatches a b = (Production (TypedGraphMorphism a b), (TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b))

getUnderlyingDerivation :: RuleWithMatches a b -> TypedGraphMorphism a b -> Derivation (TypedGraphMorphism a b)
getUnderlyingDerivation (p1,(m1,_,_)) comatch =
  let
    (_,dToH1Candidate) = calculatePushoutComplement comatch (getRHS p1)
    dToH1 = reflectIdsFromCodomain dToH1Candidate
    gluing = compose (compose (getRHS p1) comatch) (invert dToH1)
    core = codomain m1
    dToG1Candidate = findCoreMorphism (codomain gluing) core
    (match,dToG1) = restrictMorphisms (m1,dToG1Candidate)
  in Derivation p1 match comatch gluing dToG1 dToH1

findMono :: TypedGraph a b -> TypedGraph a b -> TypedGraphMorphism a b
findMono a b =
  let
    monos = findMonomorphisms a b
  in if L.null monos then error "morphisms not found" else head monos

findCoreMorphism :: TypedGraph a b -> TypedGraph a b -> TypedGraphMorphism a b
findCoreMorphism dom core =
  let
    ns = L.map (\(n,_) -> (n,n)) (nodesWithType dom)
    es = L.map (\(a,_,_,_) -> (a,a)) (edgesWithType dom)
    initial = buildTypedGraphMorphism dom core (GM.empty (domain dom) (domain core))
  in L.foldr (uncurry updateEdgeRelation) (L.foldr (uncurry untypedUpdateNodeRelation) initial ns) es

calculateNacRelations :: OccurenceGrammar a b -> Set Interaction -> OccurenceGrammar a b
calculateNacRelations ogg is = newOgg
  where
    (deleteForbid,produceForbid) = S.partition (\i -> interactionType i == DeleteForbid) is
    isConcreteDeleteForbid (c, Rule r) = isInInitial (initialGraph ogg) c || happensBeforeAction (concreteRelation ogg) c r
    isDiscardedDeleteForbid (c, Rule r) = happensAfterAction (concreteRelation ogg) c r
  --  (concreteDF,potentialDF) = S.partition isConcreteDeleteForbid (S.map (findConcreteTrigger ogg) deleteForbid)
  --  (discarded,potential) = S.partition isDiscardedDeleteForbid potentialDF

    createProduceForbidItem = findConcreteTrigger ogg
    --isConcreteProduceForbid (Rule r, c)

    (dfs, absDfs) = calculateDeleteForbids ogg deleteForbid

    newOgg = OccurenceGrammar
              (singleTypedGrammar ogg)
              (originalRulesWithMatches ogg)
              (doubleType ogg)
              (originRelation ogg)
              (buildTransitivity (concreteRelation ogg `union` dfs)) -- do the reflexive and transitive Closure
              absDfs

calculateDeleteForbids :: OccurenceGrammar a b -> Set Interaction -> (Relation, AbstractRelation)
calculateDeleteForbids ogg dfs = (S.map toRelation concreteDF, S.map toAbstractRelation abstract)
  where
    cRelation = S.map swap $ S.filter isCreation (originRelation ogg)
    isConcreteDeleteForbid (i, t) = isInInitial (initialGraph ogg) t || happensBeforeAction (concreteRelation ogg) t (secondRule i)
    isDiscardedDeleteForbid (i, t) = happensAfterAction (concreteRelation ogg) t (secondRule i)
    (concreteDF,potentialDF) = S.partition isConcreteDeleteForbid (S.map (findConcreteTrigger ogg) dfs)
    (discarded,abstract) = S.partition isDiscardedDeleteForbid potentialDF
    toRelation (i, _) = (Rule (firstRule i), Rule (secondRule i))
    toAbstractRelation (i, c) = (toRelation (i, c), (Rule (secondRule i), findRule cRelation c))

isCreation (a,b) = case (a,b) of
                      (Rule _, Node _) -> True
                      (Rule _, Edge _) -> True
                      _                -> False

isDeletion (a,b) = case (a,b) of
                      (Node _, Rule _) -> True
                      (Edge _, Rule _) -> True
                      _                -> False

findRule :: Relation -> RelationItem -> RelationItem
findRule rel e = fromMaybe (error $ "there should be an action that related to " ++ show e) find
  where
    find = lookup e $ toList rel




findConcreteTrigger :: OccurenceGrammar a b -> Interaction -> (Interaction, RelationItem) -- check if the order is correct for produce forbids
findConcreteTrigger ogg interaction@(Interaction a1 a2 t nacIdx) =
  let
    originalRules = L.map (\(a,b,c) -> (a, (b,c))) (originalRulesWithMatches ogg)
    p1Candidate = fromJust $ lookup a1 originalRules
    p1 = if t == DeleteForbid then p1Candidate else invert p1Candidate
    p2 = fromJust $ lookup a2 originalRules
    triggeredNAC = getNACs (fst p2) !! fromJust nacIdx

    (r1',m2) = getOverlapping p1 p2
    d1 = getUnderlyingDerivation p1 r1'
    h21 = findH21 m2 (dToH d1)
    d1h21 = compose h21 (dToG d1)
    q21 = findMono (codomain triggeredNAC) (codomain d1h21)

    concreteTrigger = case getTrigger triggeredNAC of
      Node n -> Node (applyNodeUnsafe q21 n)
      Edge e -> Edge (applyEdgeUnsafe q21 e)
      _      -> error "this pattern shouldn't exist"
    invert (p1,(m1,k1,r1)) = (invertProduction conf p1, (r1,k1,m1))
   in (interaction, concreteTrigger)

getOverlapping :: RuleWithMatches a b -> RuleWithMatches a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
getOverlapping (_,(_,_,comatch)) (_,(match,_,_)) = restrictMorphisms (comatch, match)

getTrigger :: TypedGraphMorphism a b ->  RelationItem
getTrigger nac =
  let
    orphanNodes = orphanTypedNodes nac
    orphanEdges = orphanTypedEdges nac
  in if L.null orphanEdges then Node $ head orphanNodes else Edge $ head orphanEdges

isInInitial :: TypedGraph a b -> RelationItem -> Bool
isInInitial initial x = case x of
  Node n -> isJust $ GM.applyNode initial n
  Edge e -> isJust $ GM.applyEdge initial e
  _      -> error $ "case " ++ show x ++ "shouldn't occur"

happensBeforeAction :: Relation -> RelationItem -> String -> Bool
happensBeforeAction rel item name = member (item, Rule name) rel

happensAfterAction :: Relation -> RelationItem -> String -> Bool
happensAfterAction rel item name = member (Rule name,item) rel

conf :: MorphismsConfig
conf = MorphismsConfig MonoMatches MonomorphicNAC

findH21 :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
findH21 m2 d1 =
  let
    h21 = findAllPossibleH21 conf m2 d1
  in if Prelude.null h21 then error "aqui" else head h21

relatedByPreservationAndDeletion :: Relation -> NamedProduction (TypedGraphMorphism a b) -> Relation
relatedByPreservationAndDeletion relation namedRule
  | S.null relation = S.empty
  | otherwise =
  let
    r = getElem relation
    rs = getTail relation
    name = getProductionName namedRule
    nodes = preservedNodes (getProduction namedRule)
    edges = preservedEdges (getProduction namedRule)
    related (c,_) nodes edges = case c of
                                Node x -> nodes `intersect` [x] /= []
                                Edge x -> edges `intersect` [x] /= []
                                _      -> False
  in if related r nodes edges then singleton (Rule name, snd r) else relatedByPreservationAndDeletion rs namedRule

retypeProduction :: (Derivation (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retypeProduction (derivation, (g1,_,g3)) = newProduction
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
    newKType = compose mappingL newLType -- change it to use gluing and g2?
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = buildProduction newL newR []

retype :: (Production (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retype (p, (g1,g2,g3)) = newProduction
  where
    oldL = getLHS p
    oldR = getRHS p
    newKType = mapping g2
    newL = reflectIdsFromTypeGraph $ buildTypedGraphMorphism newKType (mapping g1) (mapping oldL)
    newR = reflectIdsFromTypeGraph $ buildTypedGraphMorphism newKType (mapping g3) (mapping oldR)
    newProduction = buildProduction newL newR []

restrictMorphisms' :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
restrictMorphisms' (a,b) = (removeOrphans a, removeOrphans b)
  where
    orphanNodes = orphanTypedNodes a `intersect` orphanTypedNodes b
    orphanEdges = orphanTypedEdges a `intersect` orphanTypedEdges b
    removeOrphans m = L.foldr removeNodeFromCodomain (L.foldr removeEdgeFromCodomain m orphanEdges) orphanNodes

--restrict :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
--restrict a b -> a
--  where
--    b' =

restrictMorphism' :: TypedGraphMorphism a b -> TypedGraphMorphism a b
restrictMorphism' a = removeOrphans
  where
    orphanNodes = orphanTypedNodes a
    orphanEdges = orphanTypedEdges a
    removeOrphans = L.foldr removeNodeFromCodomain (L.foldr removeEdgeFromCodomain a orphanEdges) orphanNodes
