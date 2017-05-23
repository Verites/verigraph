{-|
Description : Generic partitions generator.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
-}
module Data.TypedGraph.Partition.Generator
  ( generatePartitions
  ) where

import           Util.List (replace)

-- | Interface function to run the algorithm that generates the partitions
generatePartitions :: (a -> [a] -> Bool) -> [a] -> [[[a]]]
generatePartitions equivalenceChecker equivalenceList =
  if null equivalenceList
    then [[]]
    else backtracking equivalenceChecker equivalenceList []

-- | Runs a backtracking algorithm to create all partitions
-- receives a function of restriction to know when a element can be combined with other
backtracking :: (a -> [a] -> Bool) -> [a] -> [[a]] -> [[[a]]]
backtracking equivalenceChecker toAdd []        = backtracking equivalenceChecker (tail toAdd) [[head toAdd]]
backtracking _                  []    eqClasses = [eqClasses]
backtracking equivalenceChecker toAdd eqClasses =
  backtracking equivalenceChecker initAdd
    ([ad]:eqClasses) ++            -- adds the element in an alone class
    concat                         -- adds the element in other classes
      [backtracking equivalenceChecker initAdd newEqClasses |
        (eqClass,id) <- zip eqClasses [0..],
        equivalenceChecker ad eqClass,
        let newEqClasses = replace id (ad:eqClass) eqClasses
      ]
  where
    initAdd = tail toAdd
    ad = head toAdd
