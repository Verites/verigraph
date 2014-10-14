{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Relation where

import Data.List
import qualified Data.Map as Map 

-- datatype for endorelações em a
data Relation a = 
   Relation 
   { 
     domain   :: [a],     -- domain 
     codomain :: [a],     -- codomain
     mapping  :: Map.Map a [a]  -- mapping
   } 
   deriving (Eq,Ord,Show,Read)                

defDomain :: Rel a -> [a]
defDomain (Rel dom cod m) = 
   (Map.keys m)  


image :: Rel a -> [a]
image (Rel dom cod m) = 
   nub (concat $ Map.elems m)


empty :: Rel a
empty = Rel [] [] Map.empty


updateRel :: a -> a -> Rel a -> Rel a 
updateRel x y (Rel dom cod m) = 
  Rel ([x] `union` dom) ([y] `union` cod) (Map.insertWith (++) x [y] m)  


update :: a -> a -> Rel a -> Rel a
update x y (Rel dom cod m) = 
  Rel ([x] `union` dom) ([y] `union` cod) (Map.insert x [y] m)  
  

inverse :: Rel a -> Rel a
inverse (Rel dom cod m) = Rel cod dom m'
  where m' = Map.foldWithKey (\x ys m -> foldr (\y mp -> M.insert y x mp) ys) Map.empty m        


apply :: Rel a -> a -> [a]
apply x (Rel dom cod m) = concat $ Map.lookup x m
                          

compose :: Rel a -> Rel a -> Rel a
compose (Rel dom cod m) (Rel dom' cod' m') = Rel dom cod' m''
  where
    x <- dom 
                              

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


