module Helper where

class Valid a where
    valid :: a -> Bool
