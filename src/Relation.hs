{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Relation where

-- endorelações em a
class (Eq a) => Relation r a where

    domain   :: r a -> [a]       -- required
    codomain :: r a -> [a]       -- required
    defDomain :: r a -> [a]
    image     :: r a -> [a]

    empty    :: [a] -> [a] -> r a
    update   :: a -> [a] -> r a -> r a

    apply   :: r a -> a -> [a]
    compose :: r a -> r a -> r a
    inverse :: r a -> r a

    functional :: r a -> Bool
    injective  :: r a -> Bool
    surjective :: r a -> Bool
    total      :: r a -> Bool


    image r =
        foldr (\y acc -> y : acc)
              []
              [y | x <- domain r, y <- apply r x]

    defDomain m =
        image $ inverse m

    compose r1 r2 =
        foldr (\x acc ->
                  update x [ x'' | x' <- apply r1 x, x'' <- apply r2 x'] acc)
              (empty (domain r1) (codomain r2))
              (domain r1)

    inverse r =
        foldr (\(x, y) acc -> update y [x] acc)
              (empty (codomain r) (domain r))
              [(x, y) | x <- domain r, y <- apply r x]

    injective r =
        all (\x -> length x == 1) $ map (apply invr) (domain invr)
        where
            invr = inverse r

    surjective r =
        total $ inverse r

    total r =
        domain r == defDomain r &&
        codomain r == image r


