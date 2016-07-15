module Logic.Ctl.Semantics
  ( satisfyExpr
  , satisfyExpr'
  ) where

import Data.List

import Logic.Ctl.Base
import Logic.Model


-- | Obtain all states of the transition system that satisfy the given CTL expression.
satisfyExpr :: TransitionSystem String -> Expr -> [State String]
satisfyExpr model expr =
  statesByIds model (satisfyExpr' model expr)


satisfyExpr' :: TransitionSystem String -> Expr -> [Int]
satisfyExpr' _ (Literal False) =
  []

satisfyExpr' model (Literal True) =
  stateIds model

satisfyExpr' model (Atom v) =
  let
    goodStates = filter (stateSatisfies v) (states model)
  in
    map elementId goodStates

satisfyExpr' model (Not p) =
  let
    badStates = satisfyExpr' model p
  in
    stateIds model \\ badStates

satisfyExpr' model (And p q) =
  satisfyExpr' model p `intersect` satisfyExpr' model q

satisfyExpr' model (Or p q) =
  satisfyExpr' model p `union` satisfyExpr' model q

satisfyExpr' model (Implies p q) =
  satisfyExpr' model (q `Or` Not p)

satisfyExpr' model (Equiv p q) =
  satisfyExpr' model ((p `And` q) `Or` (Not p `And` Not q))

satisfyExpr' model (Temporal p) =
  satisfyTemporal model p


stateSatisfies :: String -> State String -> Bool
stateSatisfies p st =
  p `elem` values st


satisfyTemporal :: TransitionSystem String -> PathQuantified Expr -> [Int]
satisfyTemporal model (A (X p)) =
  satisfyExpr' model (Not$ Temporal$E$X$ Not p)

satisfyTemporal model (A (F p)) =
  satisfyAllFuture model p

satisfyTemporal model (A (G p)) =
  satisfyExpr' model (Not$ Temporal$E$F$ Not p)

satisfyTemporal model (A (U p q)) =
  satisfyExpr' model (Not$ Or (Temporal$E$U (Not q) (Not p `And` Not q))
                              (Temporal$E$G$ Not q))

satisfyTemporal model (E (X p)) =
  satisfySomeNext model p

satisfyTemporal model (E (F p)) =
  satisfyTemporal model (E$U (Literal True) p)

satisfyTemporal model (E (G p)) =
  satisfyExpr' model (Not$ Temporal$A$F$ Not p)

satisfyTemporal model (E (U p q)) =
  satisfySomeUntil model p q


satisfyAllFuture :: TransitionSystem String -> Expr -> [Int]
satisfyAllFuture model p =
  recursivelyAddPredecessors statesWherePHolds

  where
    statesWherePHolds =
      satisfyExpr' model p

    recursivelyAddPredecessors reachable =
      let
        reachable' =
          reachable `union` predecessorsA model reachable
      in
        if reachable == reachable' then
          reachable'
        else
          recursivelyAddPredecessors reachable'


satisfySomeNext :: TransitionSystem String -> Expr -> [Int]
satisfySomeNext model p =
  let
    statesWherePHolds =
      satisfyExpr' model p
    predecessorSets =
      [ prevStates model st | st <- statesWherePHolds ]
  in
    foldl' union [] predecessorSets


satisfySomeUntil :: TransitionSystem String -> Expr -> Expr -> [Int]
satisfySomeUntil model p q =
  recursivelyAddPredecessors statesWhereQHolds

  where
    statesWherePHolds =
      satisfyExpr' model p

    statesWhereQHolds =
      satisfyExpr' model q

    recursivelyAddPredecessors reachable =
      let
        predecessorsWherePHolds =
          statesWherePHolds `intersect` predecessorsE model reachable

        reachable' =
          reachable `union` predecessorsWherePHolds

      in
        if reachable == reachable' then
          reachable'
        else
          recursivelyAddPredecessors reachable'


-- | Obtain the states that are predecessors _only_ to states in the given set.
predecessorsA :: TransitionSystem a -> [Int] -> [Int]
predecessorsA model states =
  let
    allPredecessors =
      predecessorsE model states

    onlyHasCorrectSuccessors state =
      nextStates model state `subsetOf` states
  in
    filter onlyHasCorrectSuccessors allPredecessors


-- | Obtain the states that are predecessors to _some_ state in the given set.
predecessorsE :: TransitionSystem a -> [Int] -> [Int]
predecessorsE model ids =
  let
    predecessorSets =
      map (prevStates model) ids
  in
    foldl union [] predecessorSets


subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf as bs =
  all (`elem` bs) as


statesByIds :: TransitionSystem a -> [Int] -> [State a]
statesByIds model =
  map (`getState` model)
