{-|
Description : Generic partitions generator.
Maintainer  : Andrei Costa <acosta@inf.ufrgs.br>
-}
module Data.TypedGraph.Partition.Generator
  ( generatePartitions
  ) where

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

-- TODO: Verify suitability for the use of Data.Sequence to avoid using the `replace` function below

-- | Replaces the @idx@-th element by @new@ in the list @l@
--
-- This should NOT go to Util.List because it is a very inneficient operation
-- over lists. Whenever this is needed, another data structure should be
-- considered.
replace :: Int -> a -> [a] -> [a]
replace idx new list = take idx list ++ [new] ++ drop (idx+1) list