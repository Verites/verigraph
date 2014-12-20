{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Relation (
      apply
    , compose
    , defDomain
    , domain
    , empty
    , functional
    , Relation.id
    , image
    , injective
    , inverse
    , Relation
    , surjective
    , total
    , update
) where


import Data.List
import qualified Data.Map as Map 

-- datatype for endorelations em a
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
empty dom cod = Relation (sort $ nub dom) (sort $ nub cod) Map.empty

id :: (Eq a, Ord a) => [a] -> Relation a
id dom = Relation d d idMap
    where
    d = sort $ nub dom
    idMap = foldr (\x acc -> Map.insert x [x] acc) Map.empty dom

update :: (Eq a, Ord a) => a -> a -> Relation a -> Relation a 
update x y (Relation dom cod m) = 
  Relation ([x] `union` dom) ([y] `union` cod) (Map.insertWith (++) x [y] m)  

inverse :: (Ord a) => Relation a -> Relation a
inverse (Relation dom cod m) =
    Relation cod dom m'
  where
    m' = Map.foldWithKey
        (\x ys m -> foldr (\y mp -> Map.insertWith (++) y [x] mp) m ys)
        Map.empty
        m        

apply :: (Ord a) => Relation a -> a -> [a]
apply (Relation dom cod m) x =
    case Map.lookup x m of
        Just l    -> l
        otherwise -> []
                          

compose :: (Ord a) => Relation a -> Relation a -> Relation a
compose r1@(Relation dom cod m) r2@(Relation dom' cod' m') =
    Relation dom cod' m''
  where
    m'' =
        foldr
            (\a m -> let im = do
                              b <- apply r1 a
                              c <- apply r2 b
                              return c
                     in Map.insert a im m)
            Map.empty
            $ defDomain r1

functional :: (Ord a) => Relation a -> Bool
functional r =
    all (\x -> length x == 1) $ map (apply r) (domain r)

injective :: (Ord a) => Relation a -> Bool
injective r =
    all (\x -> length x == 1) $ map (apply invr) (domain invr)
    where
        invr = inverse r

surjective r =
    total $ inverse r

total r =
    domain r == defDomain r
                              
