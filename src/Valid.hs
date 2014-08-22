module Valid ( Valid ) where

class Valid a where
    valid :: a -> Bool
