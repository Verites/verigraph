module TypedGraph.DPO.GraphProcess

(occurenceRelation, filterRulesOccurenceRelation, filterElementsOccurenceRelation)

where

import Abstract.DPO
import Abstract.DPO.Process ()
import Abstract.Morphism
import Data.List as L hiding (union)
import Data.Set as S
import qualified Data.Set.Monad as SM
import Equivalence.EquivalenceClasses
import Grammar.Core
import Graph.Graph (NodeId, EdgeId)
import Util.Closures as C
import TypedGraph.DPO.GraphRule
import TypedGraph.Graph ()
import TypedGraph.Morphism as TGM

instance GenerateProcess (TypedGraphMorphism a b) where
  typing = retypeProduction
  productionTyping = retype
  restrictMorphisms = restrictMorphisms'

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

occurenceRelation :: [NamedProduction (TypedGraphMorphism a b)] -> S.Set(RelationItem, RelationItem)
occurenceRelation rules =
  let
    b = unions $ L.map creationAndDeletionRelation rules
    b' = creationAndPreservationRelation rules b
    b'' = preservationAndDeletionRelation rules b
    s = setToMonad $ unions [b,b',b'']
  in monadToSet $ transitiveClosure s

filterRulesOccurenceRelation :: S.Set(RelationItem, RelationItem) -> S.Set(RelationItem, RelationItem)
filterRulesOccurenceRelation = S.filter bothRules
  where
    bothRules (x,y) = case (x,y) of
                        (Rule _, Rule _) -> True
                        _                -> False

filterElementsOccurenceRelation :: S.Set(RelationItem, RelationItem) -> S.Set(RelationItem, RelationItem)
filterElementsOccurenceRelation = S.filter bothElements
  where
    bothElements (x,y) = case (x,y) of
                        (Rule _, _) -> False
                        (_, Rule _) -> False
                        _           -> True

createdElements :: S.Set(RelationItem, RelationItem) -> S.Set(RelationItem, RelationItem)
createdElements elementsRelation =
  let
    m = setToMonad elementsRelation
    c = relationImage m
    created = monadToSet c

   in elementsRelation

setToMonad :: (Ord a) => Set a -> SM.Set a
setToMonad = SM.fromList . toList

monadToSet :: (Ord a) => SM.Set a -> Set a
monadToSet = fromList . SM.toList

-- use with the retyped rules
creationAndDeletionRelation :: NamedProduction (TypedGraphMorphism a b) -> S.Set(RelationItem, RelationItem)
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

creationAndPreservationRelation :: [NamedProduction (TypedGraphMorphism a b)] -> S.Set(RelationItem, RelationItem) -> S.Set(RelationItem, RelationItem)
creationAndPreservationRelation rules cdRelation =
  let
    creationCase x = case fst x of
      Rule _ -> True
      _      -> False
    created = S.filter creationCase cdRelation
    result = L.map (relatedByCreationAndPreservation created) rules
  in S.unions result

relatedByCreationAndPreservation :: S.Set(RelationItem, RelationItem) -> NamedProduction (TypedGraphMorphism a b) -> S.Set(RelationItem, RelationItem)
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

preservationAndDeletionRelation :: [NamedProduction (TypedGraphMorphism a b)] -> S.Set(RelationItem, RelationItem) -> S.Set(RelationItem, RelationItem)
preservationAndDeletionRelation rules cdRelation =
  let
    deletionCase x = case snd x of
      Rule _ -> True
      _      -> False
    deleting = S.filter deletionCase cdRelation
    result = L.map (relatedByPreservationAndDeletion deleting) rules
  in S.unions result

relatedByPreservationAndDeletion :: S.Set(RelationItem, RelationItem) -> NamedProduction (TypedGraphMorphism a b) -> S.Set(RelationItem, RelationItem)
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
