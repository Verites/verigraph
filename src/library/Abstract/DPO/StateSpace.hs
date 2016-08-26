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
{-# LANGUAGE ScopedTypeVariables #-}
module Abstract.DPO.StateSpace
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
import qualified Control.Monad.State as Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Set (Set)
import qualified Data.Set as Set

import           Abstract.Morphism
import           Abstract.DPO
import qualified Logic.Model as Logic



-- | A data structure storing the explored portion of the state space induced by a
-- High-Level Replacement (HLR) system.
--
-- The states are objects in the category, up to isomorphism. Such states are
-- identified by numeric indices. The transitions are specified as pairs of
-- indices, so multiple transitions  between the same two states are seen as
-- a single one.
--
-- The states are annotated with the set of predicates that hold in them. Predicates
-- are expressed as rules, and a predicate holds in a state if the rule is applicable.
data StateSpace m = SS
  { states :: IntMap (State m) -- ^ Obtain the set of (explored) indexed states in a state space.
  , transitions :: Set (Int, Int) -- ^ Obtain the set of (explored) transitions in a state space.
  , uid :: Int -- ^ Provides an unused state index.
  , dpoConfig :: DPOConfig -- ^ Obtain the configuration of DPO semantics for the state space.
  , productions :: [Production m] -- ^ Obtain the productions of the HLR system of the state space.
  , predicates :: [(String, Production m)] -- ^ Obtain the predicates of the state space.
  }


-- | A state contains the object of the category and the list of predicates that hold in it.
type State m = (Obj m, [String])


-- | An empty state space for the HLR system defined by the given productions, with the given
-- configuration of the DPO semantics.
empty :: DPOConfig -> [Production m] -> [(String, Production m)] -> StateSpace m
empty = SS IntMap.empty Set.empty 0


-- | Tries to find an isomorphic object in the state space, returning it along with its index.
searchForState :: forall m. (DPO m) => Obj m -> StateSpace m -> Maybe (Int, State m)
searchForState obj space =
  let
    isIso (_, (obj', _)) =
      let
        isomorphisms = findMorphisms IsoMorphisms obj obj' :: [m]
      in
        not (null isomorphisms)
  in
    case filter isIso (IntMap.toList $ states space) of
      [] ->
        Nothing

      (state : _) ->
        Just state


-- | Converts the state space to a transition system that may be used for model checking
toKripkeStructure :: StateSpace m -> Logic.KripkeStructure String
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
newtype StateSpaceBuilder m a = SSB
  { unSSB :: Monad.State (StateSpace m) a }
  deriving ( Functor, Applicative, Monad
           , (Monad.MonadState (StateSpace m))
           )


-- | Runs the builder with the given configuration and initial state space.
runStateSpaceBuilder :: StateSpaceBuilder m a -> StateSpace m -> (a, StateSpace m)
runStateSpaceBuilder =
  Monad.runState . unSSB


-- | Runs the builder with the given configuration and state space, providing only
-- the computed value and ignoring the resulting state space.
evalStateSpaceBuilder :: StateSpaceBuilder m a -> StateSpace m -> a
evalStateSpaceBuilder  =
  Monad.evalState . unSSB


-- | Runs the builder with the given configuration and state space, ignoring
-- the computed value and providing only the resulting state space.
execStateSpaceBuilder :: StateSpaceBuilder m a -> StateSpace m -> StateSpace m
execStateSpaceBuilder =
  Monad.execState . unSSB


-- | Gets the configuration of DPO semantics for this builder.
getDpoConfig :: StateSpaceBuilder m DPOConfig
getDpoConfig =
  Monad.gets dpoConfig


-- | Gets the productions of the HLR system being explored in this builder.
getProductions :: StateSpaceBuilder m [Production m]
getProductions =
  Monad.gets productions


-- | Gets the productions of the HLR system being explored in this builder.
getPredicates :: StateSpaceBuilder m [(String, Production m)]
getPredicates =
  Monad.gets predicates


-- | Adds the given state if an isomorphic one doesn't exist. Returns a tuple @(index, isNew)@,
-- where @index@ is the index of the state and @isNew@ is true if no isomorphic state existed.
putState :: (DPO m) => Obj m -> StateSpaceBuilder m (Int, Bool)
putState object =
  do
    maybeIndex <- findIsomorphicState object

    case maybeIndex of
      Just (index, _) -> do
        return (index, False)

      Nothing -> do
        index <- Monad.gets uid
        config <- getDpoConfig
        allPredicates <- getPredicates

        let truePredicates = map fst . filter (isTrueAt config object) $ allPredicates
        let state = (object, truePredicates)

        Monad.modify $ \space ->
          space
          { states = IntMap.insert index state (states space)
          , uid = uid space + 1
          }
        return (index, True)
  where
    isTrueAt config object (_, production) =
      not . null $ findApplicableMatches config production object


-- | Adds a transition between the states with the given indices. Does __not__ check if
-- such states exist.
putTransition :: (Int, Int) -> StateSpaceBuilder m ()
putTransition transition =
  Monad.modify $ \space ->
    space
    { transitions = Set.insert transition (transitions space) }


-- | Tries to find an isomorphic object in the current state space, returning its index.
findIsomorphicState :: (DPO m) => Obj m -> StateSpaceBuilder m (Maybe (Int, State m))
findIsomorphicState obj =
  Monad.gets (searchForState obj)



-- | Finds all transformations of the given state with the productions of the HLR system being explored, adding them to the state space. Returns a list of the successor states as @(index, object, isNew)@, where @isNew@ indicates that the state was not present in the state space before.
expandSuccessors :: forall m. DPO m => (Int, Obj m) -> StateSpaceBuilder m [(Int, Obj m, Bool)]
expandSuccessors (index, object) =
  do
    prods <- getProductions
    successorLists <- mapM applyProduction prods
    return (concat successorLists)
  where
    applyProduction prod =
      do
        config <- getDpoConfig
        forM (findApplicableMatches config prod object) $ \match -> do
          let object' = rewrite match prod

          (index', isNew) <- putState object'
          putTransition (index, index')
          return (index', object', isNew)


-- | Runs a depth-first search on the state space, starting on the given object and limiting
-- the depth to the given number.
depthSearch :: forall m. DPO m => Int -> Obj m -> StateSpaceBuilder m ()
depthSearch maxDepth startObject =
  do
    (startIndex, _) <- putState startObject
    go maxDepth (startIndex, startObject)

  where
    go :: Int -> (Int, Obj m) -> StateSpaceBuilder m ()
    go 0 _ =
      return ()

    go depthLeft (index, object) =
      do
        sucessors <- expandSuccessors (index, object)
        forM_ sucessors $ \(index', object', isNew) ->
          when isNew $
            go (depthLeft - 1) (index', object')
