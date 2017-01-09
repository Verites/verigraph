module TypedGraph.DPO.GraphProcess

(occurenceRelation)

where

import Abstract.DPO
import Abstract.DPO.Process ()
import Abstract.Morphism
--import Abstract.Relation (Relation)
import Data.List
import Grammar.Core
import Graph.Graph (NodeId, EdgeId)
import TypedGraph.DPO.GraphRule
import TypedGraph.Graph ()
import TypedGraph.Morphism as TGM

instance GenerateProcess (TypedGraphMorphism a b) where
  typing = retypeProduction
  productionTyping = retype
  restrictMorphisms = restrictMorphisms'

--type OccurrenceRelation = Relation RelationItem

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

{-buildBasicRelation :: [NamedRuleWithMatches (TypedGraphMorphism a b)] -> OccurrenceRelation
buildBasicRelation namedRules =
  let
    base = empty [] []
  in base-}

occurenceRelation :: [NamedProduction (TypedGraphMorphism a b)] -> [(RelationItem, RelationItem)]
occurenceRelation rules =
  let
    b = concatMap creationAndDeletionRelation rules
    b' = creationAndPreservationRelation rules b
    b'' = preservationAndDeletionRelation rules b
  in b ++ b' ++ b''

-- use with the retyped rules
creationAndDeletionRelation :: NamedProduction (TypedGraphMorphism a b) -> [(RelationItem, RelationItem)]
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
  in nodesAndEdges ++ withRules

creationAndPreservationRelation :: [NamedProduction (TypedGraphMorphism a b)] -> [(RelationItem, RelationItem)] -> [(RelationItem, RelationItem)]
creationAndPreservationRelation rules cdRelation =
  let
    creationCase x = case fst x of
      Rule _ -> True
      _      -> False
    created = filter creationCase cdRelation
    result = concatMap (relatedByCreationAndPreservation created) rules
  in result

relatedByCreationAndPreservation :: [(RelationItem, RelationItem)] -> NamedProduction (TypedGraphMorphism a b) -> [(RelationItem, RelationItem)]
relatedByCreationAndPreservation []     _         = []
relatedByCreationAndPreservation (r:rs) namedRule =
  let
    name = getProductionName namedRule
    nodes = preservedNodes (getProduction namedRule)
    edges = preservedEdges (getProduction namedRule)
    related (_,c) nodes edges = case c of
                                Node x -> nodes `intersect` [x] /= []
                                Edge x -> edges `intersect` [x] /= []
                                _      -> False
  in if related r nodes edges then [(fst r, Rule name)] else relatedByCreationAndPreservation rs namedRule

preservationAndDeletionRelation :: [NamedProduction (TypedGraphMorphism a b)] -> [(RelationItem, RelationItem)] -> [(RelationItem, RelationItem)]
preservationAndDeletionRelation rules cdRelation =
  let
    deletionCase x = case snd x of
      Rule _ -> True
      _      -> False
    deleting = filter deletionCase cdRelation
    result = concatMap (relatedByPreservationAndDeletion deleting) rules
  in result

relatedByPreservationAndDeletion :: [(RelationItem, RelationItem)] -> NamedProduction (TypedGraphMorphism a b) -> [(RelationItem, RelationItem)]
relatedByPreservationAndDeletion []     _         = []
relatedByPreservationAndDeletion (r:rs) namedRule =
  let
    name = getProductionName namedRule
    nodes = preservedNodes (getProduction namedRule)
    edges = preservedEdges (getProduction namedRule)
    related (c,_) nodes edges = case c of
                                Node x -> nodes `intersect` [x] /= []
                                Edge x -> edges `intersect` [x] /= []
                                _      -> False
  in if related r nodes edges then [(Rule name, snd r)] else relatedByPreservationAndDeletion rs namedRule

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
    newL = buildTypedGraphMorphism newKType (mapping g1) (mapping oldL)
    newR = buildTypedGraphMorphism newKType (mapping g3) (mapping oldR)
    newProduction = buildProduction newL newR []

restrictMorphisms' :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
restrictMorphisms' (a,b) = (removeOrphans a, removeOrphans b)
  where
    orphanNodes = orphanTypedNodes a `intersect` orphanTypedNodes b
    orphanEdges = orphanTypedEdges a `intersect` orphanTypedEdges b
    removeOrphans m = foldr removeNodeFromCodomain (foldr removeEdgeFromCodomain m orphanEdges) orphanNodes
