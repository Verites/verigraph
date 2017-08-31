module Util.List
( countElement
, deleteByKey
, insertByKey
, listKeys
, replace
, repeated
, split
, parallelMap
)

where

import           Control.Parallel (par)
import           Data.List (isPrefixOf)

-- | Applies the given function to each element of the list, executing in parallel.
--
-- Each element will generate a new spark, so the parallel execution strategy may not be optimal.
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap _ []      = []
parallelMap f (x:xs) = let r = f x
                        in r `par` r : parallelMap f xs

-- TODO: Verify suitability for the use of Data.Sequence
-- | Replaces the @idx@-th element by @new@ in the list @l@
replace :: Int -> a -> [a] -> [a]
replace idx new list = take idx list ++ [new] ++ drop (idx+1) list

-- | Given a list, it verifies whether there are repeated elements on it
repeated :: (Eq a) => [a] -> Bool
repeated []     = False
repeated (x:xs) = x `elem` xs || repeated xs


-- | Given a list of pairs of the form (key, value), it returns a list containg onlying the listKeys
listKeys :: [(key, a)] -> [key]
listKeys = map fst

{- | Given a list of pairs of the form (key, value), a key @k@ and a value @v@, it will
add the pair (k,v) to the list and remove all previous pairs that match the key @k@. -}
insertByKey :: Eq k => [(k, a)] -> k -> a -> [(k, a)]
insertByKey l k value = (k, value) : deleteByKey l k

{- | Given a list of pairs of the form (key, value) and a key, it removes
 all pairs that match the given key.-}
deleteByKey :: Eq k => [(k, a)] -> k -> [(k, a)]
deleteByKey l k = filter (\a -> fst a /= k) l

{- | Given an element @e@ and a list @l@, it returns the number of times that @e@ appears in @l@ -}
countElement :: Eq a => a -> [a] -> Int
countElement i = length . filter (i==)

{- | Similar to Data.List.span, but performs the test on the entire remaining list instead of just one element.@-}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining list instead of just one element.-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a delimiter and a list (or string), split into components.-}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [[]]
                                        else split delim
                                                 (drop (length delim) x)
