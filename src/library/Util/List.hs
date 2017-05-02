module Util.List
( replace
, repeated
)

where

-- TODO: Verify suitability for the use of Data.Sequence
-- | Replaces the @idx@-th element by @new@ in the list @l@
replace :: Int -> a -> [a] -> [a]
replace idx new list = take idx list ++ [new] ++ drop (idx+1) list

-- | Given a list, it verifies whether there are repeated elements on it
repeated :: (Eq a) => [a] -> Bool
repeated []     = False
repeated (x:xs) = x `elem` xs || repeated xs
