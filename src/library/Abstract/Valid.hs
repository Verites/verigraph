module Abstract.Valid where

class Valid a where
    valid :: a -> Bool
