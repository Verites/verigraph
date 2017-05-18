module Util.List
( countElement
, deleteByKey
, insertByKey
, listKeys
, replace
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
