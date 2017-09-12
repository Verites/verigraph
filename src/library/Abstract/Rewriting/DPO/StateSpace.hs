{-|
Description : Utilities for exploring the state space of DPO-based High-Level Replacement Systems.

Maintainer  : Guilherme G. Azzi <ggazzi@inf.ufrgs.br>
Stability   : experimental

A High-Level Replacement (HLR) system, along with a starting state, induces a state space that
may be seen as a transition system or Kripke structure. This module provides a data structure
for representing the explored portion of such a state space, as well as a monad for doing said
exploration.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Abstract.Rewriting.DPO.StateSpace
  (
  -- * State spaces
    StateSpace
  , empty
  , states
  , transitions
  , searchForState
  , toKripkeStructure

  -- * State space builder
  , StateSpaceBuilder
  , runStateSpaceBuilder
  , evalStateSpaceBuilder
  , execStateSpaceBuilder

  -- ** Getting the configuration
  , getDpoConfig
  , getProductions

  -- ** Accessing the state space
  , putState
  , putTransition
  , findIsomorphicState

  -- ** Exploring the state space
  , expandSuccessors
  , depthSearch
  ) where


import           Control.Monad
import qualified Control.Monad.State                as Monad
import           Data.IntMap                        (IntMap)
import qualified Data.IntMap                        as IntMap
import           Data.Set                           (Set)
import qualified Data.Set                           as Set

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO             hiding (productions)
import qualified Logic.Model                        as Logic



-- | A data structure storing the explored portion of the state space induced by a
-- High-Level Replacement (HLR) system.
--
-- The states are objects in the category, up to isomorphism. Such states are
-- identified by numeric indices. The transitions are specified as pairs of
-- indices, so multiple transitions  between the same two states are seen as
-- a single one.
--
-- The states are annotated with the set of predicates that hold in them. Predicates
-- are expressed as productions, and a predicate holds in a state if the production is applicable.
data StateSpace morph = SS
  { states        :: IntMap (State morph) -- ^ Obtain the set of (explored) indexed states in a state space.
  , transitions   :: Set (Int, Int) -- ^ Obtain the set of (explored) transitions in a state space.
  , uid           :: Int -- ^ Provides an unused state index.
  , morphismsConf :: MorphismsConfig morph -- ^ Obtain the configuration of DPO semantics for the state space.
  , productions   :: [Production morph] -- ^ Obtain the productions of the HLR system of the state space.
  , predicates    :: [(String, Production morph)] -- ^ Obtain the predicates of the state space.
  }


-- | A state contains the object of the category and the list of predicates that hold in it.
type State morph = (Obj morph, [String])


-- | An empty state space for the HLR system defined by the given productions, with the given
-- configuration of the DPO semantics.
empty :: MorphismsConfig morph -> [Production morph] -> [(String, Production morph)] -> StateSpace morph
empty = SS IntMap.empty Set.empty 0


-- | Tries to find an isomorphic object in the state space, returning it along with its index.
searchForState :: forall morph. (DPO morph) => Obj morph -> StateSpace morph -> Maybe (Int, State morph)
searchForState obj space =
  let
    isIso (_, (obj', _)) =
      let
        isomorphisms = findIsomorphisms obj obj' :: [morph]
      in
        not (null isomorphisms)
  in
    case filter isIso (IntMap.toList $ states space) of
      [] ->
        Nothing

      (state : _) ->
        Just state


-- | Converts the state space to a transition system that may be used for model checking
toKripkeStructure :: StateSpace morph -> Logic.KripkeStructure String
toKripkeStructure space =
  let
    convertedStates =
      map convertState $ IntMap.toList (states space)

    convertState (index, (_, props)) =
      Logic.State index props

    convertedTransitions =
      zipWith convertTransition [0..] $ Set.toList (transitions space)

    convertTransition index (from, to) =
      Logic.Transition index from to []
  in
    Logic.KripkeStructure convertedStates convertedTransitions



-- | A monad for exploring the state space of a High-Level Replacement System.
--
-- Provides a static configuration of the DPO semantics and a static set of
newtype StateSpaceBuilder morph a = SSB
  { unSSB :: Monad.State (StateSpace morph) a }
  deriving ( Functor, Applicative, Monad
           , (Monad.MonadState (StateSpace morph))
           )


-- | Runs the builder with the given configuration and initial state space.
runStateSpaceBuilder :: StateSpaceBuilder morph a -> StateSpace morph -> (a, StateSpace morph)
runStateSpaceBuilder =
  Monad.runState . unSSB


-- | Runs the builder with the given configuration and state space, providing only
-- the computed value and ignoring the resulting state space.
evalStateSpaceBuilder :: StateSpaceBuilder morph a -> StateSpace morph -> a
evalStateSpaceBuilder  =
  Monad.evalState . unSSB


-- | Runs the builder with the given configuration and state space, ignoring
-- the computed value and providing only the resulting state space.
execStateSpaceBuilder :: StateSpaceBuilder morph a -> StateSpace morph -> StateSpace morph
execStateSpaceBuilder =
  Monad.execState . unSSB


-- | Gets the configuration of DPO semantics for this builder.
getDpoConfig :: StateSpaceBuilder morph (MorphismsConfig morph)
getDpoConfig =
  Monad.gets morphismsConf


-- | Gets the productions of the HLR system being explored in this builder.
getProductions :: StateSpaceBuilder morph [Production morph]
getProductions =
  Monad.gets productions


-- | Gets the productions of the HLR system being explored in this builder.
getPredicates :: StateSpaceBuilder morph [(String, Production morph)]
getPredicates =
  Monad.gets predicates


-- | Adds the given state if an isomorphic one doesn't exist. Returns a tuple @(index, isNew)@,
-- where @index@ is the index of the state and @isNew@ is true if no isomorphic state existed.
putState :: (DPO morph) => Obj morph -> StateSpaceBuilder morph (Int, Bool)
putState object =
  do
    maybeIndex <- findIsomorphicState object

    case maybeIndex of
      Just (index, _) ->
        return (index, False)

      Nothing -> do
        index <- Monad.gets uid
        conf <- getDpoConfig
        allPredicates <- getPredicates

        let truePredicates = map fst . filter (isTrueAt conf object) $ allPredicates
        let state = (object, truePredicates)

        Monad.modify $ \space ->
          space
          { states = IntMap.insert index state (states space)
          , uid = uid space + 1
          }
        return (index, True)
  where
    isTrueAt conf object (_, production) =
      not . null $ findApplicableMatches conf production object


-- | Adds a transition between the states with the given indices. Does __not__ check if
-- such states exist.
putTransition :: (Int, Int) -> StateSpaceBuilder m ()
putTransition transition =
  Monad.modify $ \space ->
    space
    { transitions = Set.insert transition (transitions space) }


-- | Tries to find an isomorphic object in the current state space, returning its index.
findIsomorphicState :: DPO morph => Obj morph -> StateSpaceBuilder morph (Maybe (Int, State morph))
findIsomorphicState obj =
  Monad.gets (searchForState obj)



-- | Finds all transformations of the given state with the productions of the HLR system being explored, adding them to the state space. Returns a list of the successor states as @(index, object, isNew)@, where @isNew@ indicates that the state was not present in the state space before.
expandSuccessors :: forall morph. DPO morph => (Int, Obj morph) -> StateSpaceBuilder morph [(Int, Obj morph, Bool)]
expandSuccessors (index, object) =
  do
    prods <- getProductions
    successorLists <- mapM applyProduction prods
    return (concat successorLists)
  where
    applyProduction prod =
      do
        conf <- getDpoConfig
        forM (findApplicableMatches conf prod object) $ \match -> do
          let object' = rewrite match prod

          (index', isNew) <- putState object'
          putTransition (index, index')
          return (index', object', isNew)


-- | Runs a depth-first search on the state space, starting on the given object and limiting
-- the depth to the given number.
depthSearch :: forall morph. DPO morph => Int -> Obj morph -> StateSpaceBuilder morph ()
depthSearch maxDepth startObject =
  do
    (startIndex, _) <- putState startObject
    go maxDepth (startIndex, startObject)

  where
    go :: Int -> (Int, Obj morph) -> StateSpaceBuilder morph ()
    go 0 _ =
      return ()

    go depthLeft (index, object) =
      do
        sucessors <- expandSuccessors (index, object)
        forM_ sucessors $ \(index', object', isNew) ->
          when isNew $
            go (depthLeft - 1) (index', object')
