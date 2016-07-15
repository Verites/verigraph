{-# LANGUAGE TypeFamilies #-}
module Logic.Model where

import Data.Maybe


-- | A transition system is composed of a list of states and a list
--  of transitions between such states. Both states and transitions
--  may be labeled with values of some particular type.
data TransitionSystem a = TransitionSystem
  { states :: [State a]
  , transitions :: [Transition a]
  }
  deriving (Show, Read, Eq)


data State a
  = State Int [a]
  deriving (Show, Read, Eq)


data Transition a = Transition
  { transitionId :: Int
  , source :: Int
  , target :: Int
  , transitionPayload :: [a]
  }
  deriving (Show, Read, Eq)


-- | List of all state IDs on a given transition system
stateIds :: TransitionSystem a -> [Int]
stateIds =
  map elementId . states


-- | Finds the state with given ID in the given transition system.
lookupState :: Int -> TransitionSystem a -> Maybe (State a)
lookupState i =
  findById i . states


-- | Gets the state with given ID in the given transition system, __fails if there is none__.
getState :: Int -> TransitionSystem a -> State a
getState i =
  fromJust . lookupState i


-- | List of all transition IDs on a given transition system
transitionIds :: TransitionSystem a -> [Int]
transitionIds =
  map elementId . transitions


-- | Finds the transition with given ID in the given transition system.
lookupTransition :: Int -> TransitionSystem a -> Maybe (Transition a)
lookupTransition i =
  findById i . transitions


-- | Gets the transition with given ID in the given transition system, __fails if there is none__.
getTransition :: Int -> TransitionSystem a -> Transition a
getTransition i =
  fromJust . lookupTransition i


-- | Obtains the IDs of the states that are reachable by a single transition
--  from the state with given ID.
nextStates :: TransitionSystem a -> Int -> [Int]
nextStates (TransitionSystem _ transitions) state =
  map target $ filter (\t -> source t == state) transitions


-- | Tests if the first given state is reachable from the second by a single transition
follows :: TransitionSystem a -> Int -> Int -> Bool
follows ts s1 s2 =
  s1 `elem` nextStates ts s2


-- | Obtains the IDs of the states from which the given state is reachable by a single transition.
prevStates :: TransitionSystem a -> Int -> [Int]
prevStates (TransitionSystem _ transitions) state =
  map source $ filter (\t -> target t == state) transitions


class Element e where
  type Payload e :: *

  elementId :: e -> Int
  values    :: e -> [Payload e]


instance Element (State a) where
  type Payload (State a) = a

  elementId (State i _) = i
  values    (State _ v) = v


instance Element (Transition a) where
  type Payload (Transition a) = a

  elementId (Transition i _ _ _) = i
  values    (Transition _ _ _ v) = v



-- | Tests if the second given stateis reachable from the first by a single transition
precedes :: TransitionSystem a -> Int -> Int -> Bool
precedes ts s1 s2 =
  s1 `elem` prevStates ts s2


-- | Given a list of elements, find the element with the given ID
findById :: Element a => Int -> [a] -> Maybe a
findById i elems =
  listToMaybe $ filter (\e -> elementId e == i) elems
