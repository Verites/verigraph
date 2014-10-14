{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Relation  where {- (
    domain    :: Relation a -> [a],
    defDomain :: Relation a -> [a],
    image     :: Relation a -> [a],
    inverse   :: Relation a -> Relation [a],
    apply     :: Relation a -> a -> [a],
    compose   :: Relation a -> Relation a -> Relation a
)
-}

import Data.List
import qualified Data.Map as Map 

-- datatype for endorelações em a
data Relation a = 
   Relation { 
       domain   :: [a],     -- domain 
       codomain :: [a],     -- codomain
       mapping  :: Map.Map a [a]  -- mapping
   } deriving (Ord,Show,Read)                

instance (Eq a) => Eq (Relation a) where
    r1 == r2 = domain r1 == domain r2     &&
               codomain r1 == codomain r2 &&
               mapping r1 == mapping r2
               
defDomain :: Relation a -> [a]
defDomain (Relation dom cod m) = 
   (Map.keys m)

image :: (Eq a) => Relation a -> [a]
image (Relation dom cod m) = 
   nub (concat $ Map.elems m)

empty :: (Eq a, Ord a) => [a] -> [a] -> Relation a
--empty = Relation [] [] Map.empty
empty c d = Relation (sort $ nub c) (sort $ nub d) Map.empty

updateRelation :: a -> a -> Relation a -> Relation a 
updateRelation x y (Relation dom cod m) = 
  Relation ([x] `union` dom) ([y] `union` cod) (Map.insertWith (++) x [y] m)  


update :: a -> a -> Relation a -> Relation a
update x y (Relation dom cod m) = 
  Relation ([x] `union` dom) ([y] `union` cod) (Map.insert x [y] m)  
  

inverse :: Relation a -> Relation a
inverse (Relation dom cod m) =
    Relation cod dom m'
  where
    m' = Map.foldWithKey
        (\x ys m -> foldr (\y mp -> Map.insert y x mp) ys)
        Map.empty
        m        

apply :: Relation a -> a -> [a]
apply x (Relation dom cod m) =
    case Map.lookup x m of
        Just l    -> l
        otherwise -> []
                          

compose :: Relation a -> Relation a -> Relation a
compose r1@(Relation dom cod m) r2@(Relation dom' cod' m') =
    Relation dom cod' m''
  where
    m'' =
        foldr
            (\a m -> let im = do
                              b <- apply a r1
                              c <- apply b r2
                              return c
                     in Map.insert a im m)
            (Map.empty dom cod')
            $ defDomain m
                              
{-
class (Eq t) => Morphism t where
  dom
  cod 
  compose
  equal
  id 
  
  monomorphism 
  epimorphism
  isomorphism

  
   

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

-}
