module Util.List (
   replace
   ) where

-- TODO: Verify suitability for the use of Data.Sequence
-- | Replaces the @idx@-th element by @new@ in the list @l@
replace :: Int -> a -> [a] -> [a]
replace idx new list = take idx list ++ [new] ++ drop (idx+1) list
