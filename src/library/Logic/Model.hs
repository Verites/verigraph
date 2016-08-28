{-|
Description : Data structures that represent models of temporal logic.

Maintainer  : Guilherme G. Azzi <ggazzi@inf.ufrgs.br>
Stability   : provisional
-}
{-# LANGUAGE TypeFamilies #-}
module Logic.Model
  (
  -- * Kripke structures
    KripkeStructure(..)
  , State(..)
  , Transition(..)

  -- ** Looking up states
  , stateIds
  , lookupState
  , getState

  -- ** Looking up transitions
  , transitionIds
  , lookupTransition
  , getTransition

  -- ** Lookup by adjacency
  , nextStates
  , precedes
  , prevStates
  , follows

  -- * Utilities for labeled elements
  , Element(..)
  , findById
  ) where

import           Data.Maybe


-- | A Kripke structure is composed of a list of states and a list of
-- transitions between such states. States are labeled with the atomic
-- propositions that hold in it.
--
-- This particular kind of labeled transition system may be used as a model
-- for temporal logics. In particular, it may be used for model checking.
--
-- This structure is polymorphic on the type of atomic propositions.
data KripkeStructure a = KripkeStructure
  { states      :: [State a] -- ^ List of labeled states of the Kripke structure
  , transitions :: [Transition a] -- ^ List of transitions of the Kripke structure
  }
  deriving (Show, Read, Eq)


-- | A state contains its unique identifier and a list of atomic
-- propositions that hold in it.
data State a
  = State Int [a]
  deriving (Show, Read, Eq)


-- | A transition contains the identifiers of the source and target states.
data Transition a = Transition
  { transitionId      :: Int
  , source            :: Int
  , target            :: Int
  , transitionPayload :: [a]
  }
  deriving (Show, Read, Eq)


-- | List of all state IDs from a given Kripke structure
stateIds :: KripkeStructure a -> [Int]
stateIds =
  map elementId . states


-- | Finds the state with given ID in the given Kripke structure
lookupState :: Int -> KripkeStructure a -> Maybe (State a)
lookupState i =
  findById i . states


-- | Gets the state with given ID in the given Kripke structure, __fails if there is none__.
getState :: Int -> KripkeStructure a -> State a
getState i =
  fromJust . lookupState i


-- | List of all transition IDs on a given Kripke structure
transitionIds :: KripkeStructure a -> [Int]
transitionIds =
  map elementId . transitions


-- | Finds the transition with given ID in the given Kripke structure.
lookupTransition :: Int -> KripkeStructure a -> Maybe (Transition a)
lookupTransition i =
  findById i . transitions


-- | Gets the transition with given ID in the given Kripke structure, __fails if there is none__.
getTransition :: Int -> KripkeStructure a -> Transition a
getTransition i =
  fromJust . lookupTransition i


-- | Obtains the IDs of the states that are reachable by a single transition
--  from the state with given ID.
nextStates :: KripkeStructure a -> Int -> [Int]
nextStates (KripkeStructure _ transitions) state =
  map target $ filter (\t -> source t == state) transitions


-- | Tests if the first given state is reachable from the second by a single transition
follows :: KripkeStructure a -> Int -> Int -> Bool
follows ts s1 s2 =
  s1 `elem` nextStates ts s2


-- | Obtains the IDs of the states from which the given state is reachable by a single transition.
prevStates :: KripkeStructure a -> Int -> [Int]
prevStates (KripkeStructure _ transitions) state =
  map source $ filter (\t -> target t == state) transitions


-- | Tests if the second given state is reachable from the first by a single transition
precedes :: KripkeStructure a -> Int -> Int -> Bool
precedes ts s1 s2 =
  s1 `elem` prevStates ts s2


-- | Type class for elements that have a numeric identifier and a list of associated values.
class Element e where
  -- | Type of associated values.
  type Payload e :: *

  -- | Obtain the numeric identifier of an element.
  elementId :: e -> Int

  -- | Obtain the associated values of an element.
  values :: e -> [Payload e]


instance Element (State a) where
  type Payload (State a) = a

  elementId (State i _) = i
  values (State _ v) = v


instance Element (Transition a) where
  type Payload (Transition a) = a

  elementId = transitionId
  values = transitionPayload


-- | Given a list of elements, find the element with the given identifier.
findById :: Element a => Int -> [a] -> Maybe a
findById i elems =
  listToMaybe $ filter (\e -> elementId e == i) elems
