module TypedGraph.DPO.OccurenceRelation

( RelationItem(..)
, Relation
, AbstractRelation
, isRuleAndElement
, filterRulesOccurrenceRelation
, filterElementsOccurrenceRelation
, filterCreationRelation
, filterDeletionRelation
, isCreation
, isDeletion)

where

import           Graph.Graph                    (EdgeId, NodeId)
import Data.Set as S

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

type Relation = S.Set(RelationItem, RelationItem)
type AbstractRelation = S.Set ((RelationItem, RelationItem), (RelationItem, RelationItem))

isRuleAndElement :: (RelationItem, RelationItem) -> Bool
isRuleAndElement (a,b) = case (a,b) of
                      (Rule _, Rule _) -> False
                      (Rule _, _)      -> True
                      (_, Rule _)      -> True
                      _                -> False

filterRulesOccurrenceRelation :: Relation -> Relation
filterRulesOccurrenceRelation = S.filter bothRules
  where
    bothRules (x,y) = case (x,y) of
                        (Rule _, Rule _) -> True
                        _                -> False


filterElementsOccurrenceRelation :: Relation -> Relation
filterElementsOccurrenceRelation = S.filter bothElements
  where
    bothElements (x,y) = case (x,y) of
                        (Rule _, _) -> False
                        (_, Rule _) -> False
                        _           -> True

filterCreationRelation :: Relation -> Relation
filterCreationRelation = S.filter bothElements
  where
    bothElements (x,y) = case (x,y) of
                        (Rule _, Node _) -> True
                        (Rule _, Edge _) -> True
                        _                -> False

filterDeletionRelation :: Relation -> Relation
filterDeletionRelation = S.filter bothElements
  where
    bothElements (x,y) = case (x,y) of
                        (Node _, Rule _) -> True
                        (Edge _, Rule _) -> True
                        _                -> False

isCreation :: (RelationItem, RelationItem) -> Bool
isCreation (a,b) = case (a,b) of
                      (Rule _, Node _) -> True
                      (Rule _, Edge _) -> True
                      _                -> False

isDeletion :: (RelationItem, RelationItem) -> Bool
isDeletion (a,b) = case (a,b) of
                      (Node _, Rule _) -> True
                      (Edge _, Rule _) -> True
                      _                -> False
