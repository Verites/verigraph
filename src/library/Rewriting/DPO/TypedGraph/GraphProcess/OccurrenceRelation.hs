{-# LANGUAGE TypeSynonymInstances #-}
module Rewriting.DPO.TypedGraph.GraphProcess.OccurrenceRelation

( RelationItem(..)
, Relation
, AbstractRelation
, AbstractType (..)
, isRuleAndElement
, filterRulesOccurrenceRelation
, filterElementsOccurrenceRelation
, filterCreationRelation
, filterDeletionRelation
, isCreation
, isDeletion
, isNode
, happensAfterAction
, happensBeforeAction
, relatedItens
, neverDeleted
, present
, findOrder
, buildTransitivity
, relationToString
, restrictionToString)

where

import           Data.Graphs    (EdgeId, NodeId)
import           Data.Maybe     (isJust, isNothing)
import           Data.Partition
import           Data.Set       as S
import           Util.Closures  as C

data RelationItem = Node NodeId
                  | Edge EdgeId
                  | Rule String
                  deriving (Eq, Ord, Show)

type Relation = S.Set(RelationItem, RelationItem)
data AbstractType = AbstractProduceForbid | AbstractDeleteForbid deriving (Eq, Ord, Show)
type AbstractRelation = S.Set (AbstractType, (RelationItem, RelationItem), (RelationItem, RelationItem))

relationToString :: Relation -> String
relationToString rel = "[" ++ concatSet (toList rel) ++"]"
    where
      concatSet []     = ""
      concatSet [x]    = format x
      concatSet (x:xs) = format x ++ "," ++ concatSet xs
      format (a,b) = "(" ++ show a ++ " < " ++ show b ++")"

restrictionToString :: AbstractRelation -> String
restrictionToString res = "[" ++ concatSet (toList res) ++"]"
    where
      concatSet []     = ""
      concatSet [x]    = format x
      concatSet (x:xs) = format x ++ ",\n" ++ concatSet xs
      format (t,(a,b),(_,d)) = "(" ++ show t ++ ": " ++ show b ++ " not in between "++ "[" ++ show a ++ " < " ++ show d ++"])"

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

isNode :: RelationItem -> Bool
isNode x = case x of
           Node _ -> True
           _      -> False

-- | Tests wether an item appears before a rule in a given occurrence relation
happensBeforeAction :: Relation -> RelationItem -> String -> Bool
happensBeforeAction rel item name = member (item, Rule name) rel

relatedItens :: Relation -> (RelationItem, RelationItem) -> Bool
relatedItens rel (i1,i2) = member (i1,i2) rel || member (i2,i1) rel

-- | Tests wether an item appears after a rule in a given occurrence relation
happensAfterAction :: Relation -> RelationItem -> String -> Bool
happensAfterAction rel item name = member (Rule name,item) rel

-- | Given a relation item @i@ and the deletion relation of an doubly typed grammar,
-- it returns True if the item is deleted by some rule in this relation and False otherwise
neverDeleted :: RelationItem -> Relation -> Bool
neverDeleted e rel = isNothing (lookup e $ toList rel)

present :: RelationItem -> Relation -> Bool
present e rel = isJust (lookup e $ toList rel)

findOrder :: Relation -> Set RelationItem -> Maybe [RelationItem]
findOrder = tsort

buildTransitivity :: Relation -> Relation
buildTransitivity = monadToSet . transitiveClosure . setToMonad
