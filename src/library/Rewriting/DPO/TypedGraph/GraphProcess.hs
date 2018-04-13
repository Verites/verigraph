{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rewriting.DPO.TypedGraph.GraphProcess

( DoublyTypedGrammar (..)
, occurrenceRelation
, generateDoublyTypedGrammar
, uniqueOrigin
, findConcreteTrigger
, calculateNacRelations
, strictRelation
, creationAndDeletionRelation
, getElements
, initialGraph
, finalGraph
, emptyRestrictions
)

where


import           Data.List                                                as L
import           Data.Maybe                                               (fromJust, fromMaybe,
                                                                            isJust)
import           Data.Tuple                                               (swap)
import           Data.Set                                                 (Set)
import qualified Data.Set                                                 as S

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.Derivation
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Abstract.Rewriting.DPO.Process                           hiding (productions)
import           Data.Graphs                                              (Graph)
import qualified Data.Graphs.Morphism                                     as GM
import           Data.Partition
import           Data.TypedGraph                                          hiding (Node, Edge)
import           Data.TypedGraph.Morphism                                 as TGM
import           Rewriting.DPO.TypedGraph
import           Rewriting.DPO.TypedGraph.GraphProcess.OccurrenceRelation
import           Util.Closures                                            as C
import           Util.List

instance GenerateProcess (TypedGraphMorphism a b) where
  typing = retypeProduction
  productionTyping = retype
  restrictMorphisms = restrictMorphisms'
  restrictMorphism = restrictMorphism'

data DoublyTypedGrammar a b = DoublyTypedGrammar {
  singleTypedGrammar       :: Grammar (TypedGraphMorphism a b) -- ^ The grammar typed over the double type graph
, originalRulesWithMatches :: [NamedRuleWithMatches (TypedGraphMorphism a b)] -- ^ The productions of the original grammar, together with their matches in the double type graph
, doubleType               :: TypedGraph a b -- ^ The double type graph, typed over the simple type graph
, originRelation           :: Relation -- ^ The relation that shows the rule that originated each element (node or edge) in the grammar
, concreteRelation         :: Relation -- ^ The occurrence relation of the grammar (existencial relation + concrete conflicts and dependencies induced by NACs)
, restrictRelation         :: AbstractRelation -- ^ The set of restrictions given by the abstract conflicts and dependencies
}

-- | Given an doubly typed grammar, it returns its initial graph
initialGraph :: DoublyTypedGrammar a b -> TypedGraph a b
initialGraph = start . singleTypedGrammar

-- | Given an doubly typed grammar, it returns its final graph
finalGraph :: DoublyTypedGrammar a b -> TypedGraph a b
finalGraph ogg = fromJust $ lookup "final" (reachableGraphs $ singleTypedGrammar ogg)

-- | Checks whether the restrict relation of an doubly typed grammar is empty
emptyRestrictions :: DoublyTypedGrammar a b -> Bool
emptyRestrictions = S.null . restrictRelation

-- | Given a rule sequence, it calculates its underlying doubly-typed graph grammar
generateDoublyTypedGrammar :: RuleSequence (TypedGraphMorphism a b) -> DoublyTypedGrammar a b
generateDoublyTypedGrammar sequence = DoublyTypedGrammar singleGrammar originalRulesWithMatches doubleType cdRelation relation S.empty
  where
    originalRulesWithMatches = calculateRulesColimit sequence -- TODO: unify this two functions
    newRules = generateGraphProcess sequence
    cdRelation = S.filter isRuleAndElement $ strictRelation newRules
    relation = occurrenceRelation newRules
    created = createdElements cdRelation
    deleted = deletedElements cdRelation
    doubleType = codomain $ getMatch $ head originalRulesWithMatches
    coreGraph = domain doubleType
    startGraph = removeElements coreGraph created
    finalGraph = removeElements coreGraph deleted
    singleGrammar = addReachableGraphs [("final",finalGraph)] (grammar startGraph [] newRules)

creationRelation :: DoublyTypedGrammar a b -> Relation
creationRelation ogg = S.map swap $ S.filter isCreation (originRelation ogg)

deletionRelation :: DoublyTypedGrammar a b -> Relation
deletionRelation ogg = S.filter isDeletion (originRelation ogg)

-- | Given an doubly typed grammar, it returns a tuple @(rs,es)@ where @rs@ is the set of rule names in this grammar
-- and @es@ is the set of elements that appear in the productions
getElements :: DoublyTypedGrammar a b -> (Set RelationItem, Set RelationItem)
getElements ogg =
  let
    (ns,rs) = unzip $ productions (singleTypedGrammar ogg)
    ruleNames = S.map Rule (S.fromList ns)
    elements = L.map getRuleItems rs
  in (ruleNames, S.unions elements)

calculateNacRelations :: DoublyTypedGrammar a b -> Set Interaction -> DoublyTypedGrammar a b
calculateNacRelations ogg is = newOgg
  where
    (deleteForbid,produceForbid) = S.partition (\i -> interactionType i == DeleteForbid) is
    (dfs, absDfs) = calculateDeleteForbids ogg deleteForbid
    (pfs, absPfs) = calculateProduceForbids ogg produceForbid

    newOgg = DoublyTypedGrammar
              (singleTypedGrammar ogg)
              (originalRulesWithMatches ogg)
              (doubleType ogg)
              (originRelation ogg)
              (buildTransitivity (concreteRelation ogg `S.union` dfs `S.union` pfs)) -- do the reflexive and transitive Closure
              (absDfs `S.union` absPfs)

calculateDeleteForbids :: DoublyTypedGrammar a b -> Set Interaction -> (Relation, AbstractRelation)
calculateDeleteForbids ogg dfs = (S.map toRelation concreteDF, S.map toAbstractRelation abstract)
  where
    isConcreteDeleteForbid (i, t) = isInGraph (initialGraph ogg) t || happensBeforeAction (concreteRelation ogg) t (secondRule i)
    isDiscardedDeleteForbid (i, t) = happensAfterAction (concreteRelation ogg) t (secondRule i)
    (concreteDF,potentialDF) = S.partition isConcreteDeleteForbid (S.map (findConcreteTrigger ogg) dfs)
    (_,abstract) = S.partition isDiscardedDeleteForbid potentialDF
    toRelation (i, _) = (Rule (secondRule i), Rule (firstRule i))
    toAbstractRelation (i, c) = (AbstractDeleteForbid, (findRule (creationRelation ogg) c, Rule (secondRule i)), toRelation (i, c))

calculateProduceForbids :: DoublyTypedGrammar a b -> Set Interaction -> (Relation, AbstractRelation)
calculateProduceForbids ogg pfs = (S.map toRelation concretePF, S.map toAbstractRelation abstract)
  where
    isConcreteProduceForbid (i,t) = neverDeleted t (deletionRelation ogg)
       && (happensBeforeAction (concreteRelation ogg) t (firstRule i) || not (happensAfterAction (concreteRelation ogg) t (firstRule i))) -- should it be the second rule?
    isAbstractProduceForbid (i,t) = present t (deletionRelation ogg) && not (relatedItens (concreteRelation ogg) (Rule (firstRule i), Rule (secondRule i)))
    (concretePF, potentialPF) = S.partition isConcreteProduceForbid (S.map (findConcreteTrigger ogg) pfs)
    (abstract, _) = S.partition isAbstractProduceForbid potentialPF
    toRelation (i, _) = (Rule (firstRule i), Rule (secondRule i))
    toAbstractRelation (i, c) = (AbstractProduceForbid, toRelation (i, c), (Rule (secondRule i), findRule (deletionRelation ogg) c))

-- | Given an doubly typed grammar and an interaction of the types ProduceForbid or DeleteForbid between two productions
-- it returns the interaction together with the element that triggered the NAC involved in this conflict or dependency
findConcreteTrigger :: DoublyTypedGrammar a b -> Interaction -> (Interaction, RelationItem) -- check if the order is correct for produce forbids
findConcreteTrigger ogg interaction@(Interaction a1 a2 t nacIdx) =
  let
    originalRules = L.map (\(a,b,c) -> (a, (b,c))) (originalRulesWithMatches ogg)
    p1Candidate = fromJust $ lookup a1 originalRules
    p1 = if t == DeleteForbid then p1Candidate else invert p1Candidate
    p2 = fromJust $ lookup a2 originalRules
    triggeredNAC = nacs (fst p2) !! fromJust nacIdx

    (r1',m2) = getOverlapping p1 p2
    d1 = getUnderlyingDerivation p1 r1'
    h21 = findH21 m2 (dToH d1)
    d1h21 = dToG d1 <&> h21
    q21 = findMono (codomain triggeredNAC) (codomain d1h21)

    concreteTrigger = case getTrigger triggeredNAC of
      Node n -> Node (applyNodeIdUnsafe q21 n)
      Edge e -> Edge (applyEdgeIdUnsafe q21 e)
      _      -> error "this pattern shouldn't exist"
    invert (p1,(m1,k1,r1)) = (invertProduction conf p1, (r1,k1,m1))
   in (interaction, concreteTrigger)

getTrigger :: TypedGraphMorphism a b ->  RelationItem
getTrigger nac =
  let
    orphanNodes = orphanTypedNodeIds nac
    orphanEdges = orphanTypedEdgeIds nac
  in if L.null orphanEdges then Node $ head orphanNodes else Edge $ head orphanEdges

uniqueOrigin :: [NamedTypedGraphRule a b] -> Bool
uniqueOrigin productions = not (hasRepeatedElem createdList) && not (hasRepeatedElem deletedList)
  where
    creationAndDeletion = S.filter isRuleAndElement $ S.unions $ L.map creationAndDeletionRelation productions
    isCreated a = case a of
      (Rule _, _) -> True
      _           -> False
    (created, deleted) = S.partition isCreated creationAndDeletion
    createdList = S.toList $ S.map snd created
    deletedList = S.toList $ S.map fst deleted

strictRelation :: [NamedTypedGraphRule a b] -> Relation
strictRelation = S.unions . L.map creationAndDeletionRelation

occurrenceRelation :: [NamedTypedGraphRule a b] -> Relation
occurrenceRelation productions =
  let
    b = strictRelation productions
    b' = creationAndPreservationRelation productions b
    b'' = preservationAndDeletionRelation productions b
  in buildTransitivity (S.unions [b,b',b''])

createdElements :: Relation -> Set RelationItem
createdElements elementsRelation =
  let
    m = setToMonad (filterCreationRelation elementsRelation)
    c = relationImage m
    created = monadToSet c
   in created

deletedElements :: Relation -> Set RelationItem
deletedElements elementsRelation =
  let
    m = setToMonad (filterDeletionRelation elementsRelation)
    c = relationDomain m
    deleted = monadToSet c
  in deleted

removeElements :: Graph (Maybe a) (Maybe b) -> Set RelationItem -> TypedGraph a b
removeElements coreGraph elementsToRemove =
  let
    (n,e) = S.partition isNode elementsToRemove
    nodes = S.map (\(Node x) -> x) n
    edges = S.map (\(Edge x) -> x) e
  in S.foldr GM.removeNodeFromDomainForced (S.foldr GM.removeEdgeFromDomain (identity coreGraph) edges) nodes

-- use with the retyped productions
creationAndDeletionRelation :: NamedTypedGraphRule a b -> Relation
creationAndDeletionRelation (name,rule) =
  let
    ln = deletedNodes rule
    le = deletedEdges rule
    rn = createdNodes rule
    re = createdEdges rule
    nodesAndEdges = [(Node a, Node b) | a <- ln, b <- rn] ++ [(Edge a, Edge b) | a <- le, b <- re]
                 ++ [(Node a, Edge b) | a <- ln, b <- re] ++ [(Edge a, Node b) | a <- le, b <- rn]
    elementsAndRule = [(Node a, Rule name) | a <- ln] ++ [(Edge a, Rule name) | a <- le]
                   ++ [(Rule name, Node a) | a <- rn] ++ [(Rule name, Edge a) | a <- re]
  in S.fromList $ nodesAndEdges ++ elementsAndRule

getRuleItems :: TypedGraphRule a b -> Set RelationItem
getRuleItems rule =
  let
    ns = S.fromList (deletedNodes rule ++ preservedNodes rule ++ createdNodes rule)
    es = S.fromList (deletedEdges rule ++ preservedEdges rule ++ createdEdges rule)
   in S.map Node ns `S.union` S.map Edge es

creationAndPreservationRelation :: [NamedTypedGraphRule a b] -> Relation -> Relation
creationAndPreservationRelation productions cdRelation =
  let
    creationCase x = case fst x of
      Rule _ -> True
      _      -> False
    created = S.filter creationCase cdRelation
    result = L.map (relatedByCreationAndPreservation created) productions
  in S.unions result

relatedByCreationAndPreservation :: Relation -> NamedTypedGraphRule a b -> Relation
relatedByCreationAndPreservation relation preservingRule
  | S.null relation = S.empty
  | otherwise =
  let
    r = getElem relation
    rs = getTail relation
    name = getProductionName preservingRule
    nodes = preservedNodes (getProduction preservingRule)
    edges = preservedEdges (getProduction preservingRule)
    related (_,c) nodes edges = case c of
                                Node x -> x `elem` nodes
                                Edge x -> x `elem` edges
                                _      -> False
  in if related r nodes edges then S.singleton (fst r, Rule name) else relatedByCreationAndPreservation rs preservingRule

preservationAndDeletionRelation :: [NamedTypedGraphRule a b] -> Relation -> Relation
preservationAndDeletionRelation productions cdRelation =
  let
    deletionCase x = case snd x of
      Rule _ -> True
      _      -> False
    deleting = S.filter deletionCase cdRelation
    result = L.map (relatedByPreservationAndDeletion deleting) productions
  in S.unions result

type RuleWithMatches a b = (TypedGraphRule a b, (TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b))

getUnderlyingDerivation :: RuleWithMatches a b -> TypedGraphMorphism a b -> Derivation (TypedGraphMorphism a b)
getUnderlyingDerivation (p1,(m1,_,_)) comatch =
  let
    (_,dToH1Candidate) = calculatePushoutComplementAlongM (rightMorphism p1) comatch
    dToH1 = reflectIdsFromCodomain dToH1Candidate
    gluing = invert dToH1 <&> (comatch <&> rightMorphism p1)
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
    ns = L.map (\n -> (n,n)) (nodeIds dom)
    es = L.map (\e -> (e,e)) (edgeIds dom)
    initial = buildTypedGraphMorphism dom core (GM.empty (domain dom) (domain core))
  in L.foldr (uncurry updateEdgeRelation) (L.foldr (uncurry untypedUpdateNodeRelation) initial ns) es

findRule :: Relation -> RelationItem -> RelationItem
findRule rel e = fromMaybe (error $ "there should be an action related to " ++ show e) find
  where
    find = lookup e $ S.toList rel

getOverlapping :: RuleWithMatches a b -> RuleWithMatches a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
getOverlapping (_,(_,_,comatch)) (_,(match,_,_)) = restrictMorphisms (comatch, match)

-- | Given a typed graph @tg@ and an item @i@ which can be a node or an edge, it returns True if @i@ is in
-- @t@ and False otherwise
isInGraph :: TypedGraph a b -> RelationItem -> Bool
isInGraph initial x = case x of
  Node n -> isJust $ GM.applyNodeId initial n
  Edge e -> isJust $ GM.applyEdgeId initial e
  _      -> error $ "case " ++ show x ++ "shouldn't occur"

conf :: MorphismsConfig (TypedGraphMorphism a b)
conf = MorphismsConfig monic

findH21 :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
findH21 m2 d1 =
  let
    h21 = findAllPossibleH21 conf m2 d1
  in if Prelude.null h21 then error "morphism h21 not found" else head h21

relatedByPreservationAndDeletion :: Relation -> NamedTypedGraphRule a b -> Relation
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
  in if related r nodes edges then S.singleton (Rule name, snd r) else relatedByPreservationAndDeletion rs namedRule

retypeProduction :: (Derivation (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  TypedGraphRule a b
retypeProduction (derivation, (g1,_,g3)) = newProduction
  where
    p = production derivation
    oldL = leftMorphism p
    oldR = rightMorphism p
    mappingL = mapping oldL
    mappingR = mapping oldR
    m = match derivation
    h = comatch derivation
    newLType = mapping g1 <&> mapping m
    newRType = mapping g3 <&> mapping h
    newKType = newLType <&> mappingL -- change it to use gluing and g2?
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = Production newL newR []

retype :: (TypedGraphRule a b, (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  TypedGraphRule a b
retype (p, (g1,g2,g3)) = newProduction
  where
    oldL = leftMorphism p
    oldR = rightMorphism p
    newKType = mapping g2
    newL = reflectIdsFromTypeGraph $ buildTypedGraphMorphism newKType (mapping g1) (mapping oldL)
    newR = reflectIdsFromTypeGraph $ buildTypedGraphMorphism newKType (mapping g3) (mapping oldR)
    newProduction = Production newL newR []

restrictMorphisms' :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
restrictMorphisms' (a,b) = (removeOrphans a, removeOrphans b)
  where
    orphanNodes = orphanTypedNodeIds a `intersect` orphanTypedNodeIds b
    orphanEdges = orphanTypedEdgeIds a `intersect` orphanTypedEdgeIds b
    removeOrphans m = L.foldr removeNodeFromCodomain (L.foldr removeEdgeFromCodomain m orphanEdges) orphanNodes

restrictMorphism' :: TypedGraphMorphism a b -> TypedGraphMorphism a b
restrictMorphism' a = removeOrphans
  where
    orphanNodes = orphanTypedNodeIds a
    orphanEdges = orphanTypedEdgeIds a
    removeOrphans = L.foldr removeNodeFromCodomain (L.foldr removeEdgeFromCodomain a orphanEdges) orphanNodes
