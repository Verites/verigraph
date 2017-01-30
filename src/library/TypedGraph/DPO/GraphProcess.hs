module TypedGraph.DPO.GraphProcess

( occurenceRelation
, filterRulesOccurenceRelation
, filterElementsOccurenceRelation
, myGraphProcess
, uniqueOrigin
, findOrder
)

where

import Abstract.DPO
import Abstract.DPO.Process ()
import Abstract.Morphism as M
import Data.List as L hiding (union)
import Data.Set as S
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

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

type Relation = S.Set(RelationItem, RelationItem)

uniqueOrigin :: [NamedProduction (TypedGraphMorphism a b)] -> Bool
uniqueOrigin rules = not (repeated createdList) && not (repeated deletedList)
  where
    creationAndDeletion = S.filter ruleElementItem $ unions $ L.map creationAndDeletionRelation rules
    ruleElementItem a =  case a of
      (Rule _, _) -> True
      (_, Rule _) -> True
      (_, _)      -> False
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

occurenceRelation :: [NamedProduction (TypedGraphMorphism a b)] -> Relation
occurenceRelation rules =
  let
    b = unions $ L.map creationAndDeletionRelation rules
    b' = creationAndPreservationRelation rules b
    b'' = preservationAndDeletionRelation rules b
    s = setToMonad $ unions [b,b',b'']
  in monadToSet $ transitiveClosure s

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

myGraphProcess :: RuleSequence (TypedGraphMorphism a b) -> Grammar (TypedGraphMorphism a b)
myGraphProcess sequence = grammar startGraph [] newRules
  where
    newRules = generateGraphProcess sequence
    relation = occurenceRelation newRules
    created = createdElements . filterElementsOccurenceRelation $ relation
    coreGraph = codomain . codomain . getLHS . snd . head $ newRules
    startGraph = removeElements coreGraph created

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
