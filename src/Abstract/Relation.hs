{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Abstract.Relation (
    -- * Types
      Relation
    -- * Construction
    , empty
    -- * Transformation
    , compose
    , Abstract.Relation.id
    , inverse
    , update
    -- * Query
    , apply
    , defDomain
    , domain
    , image
    -- ** Predicates
    , functional
    , injective
    , surjective
    , total
) where


import Data.List as L
import qualified Data.Map as Map 

-- datatype for endorelations on a
data Relation a = 
   Relation { 
       domain   :: [a],
       codomain :: [a],
       mapping  :: Map.Map a [a]
   } deriving (Ord,Show,Read)                

instance (Eq a) => Eq (Relation a) where
    r1 == r2 = domain r1 == domain r2     &&
               codomain r1 == codomain r2 &&
               mapping r1 == mapping r2
               
-- | Return a list of all domain elements mapped by the relation.
defDomain :: Relation a -> [a]
defDomain = Map.keys . Map.filter (not . L.null) . mapping

-- | Return a list of all elements in the image of the relation.
image :: (Eq a) => Relation a -> [a]
image = nub . concat . Map.elems . mapping

-- | An empty relation, with domain and codomain specified.
empty :: (Eq a, Ord a) => [a] -> [a] -> Relation a
empty dom cod = Relation (sort $ nub dom) (sort $ nub cod) Map.empty

-- | The identity relation on @dom@.
id :: (Eq a, Ord a) => [a] -> Relation a
id dom = Relation d d idMap
    where
    d = sort $ nub dom
    idMap = foldr (\x acc -> Map.insert x [x] acc) Map.empty dom

-- | Add a mapping between @x@ and @y@ to the relation. If @x@ already exists,
-- @y@ is joined to the corresponding elements.
update :: (Eq a, Ord a) => a -> a -> Relation a -> Relation a 
update x y (Relation dom cod m) = 
    Relation ([x] `union` dom) ([y] `union` cod) (Map.insertWith insertUniquely x [y] m)  
  where
    insertUniquely y y'
        | null $ y `L.intersect` y' = y ++ y'
        | otherwise = y'

-- | The inverse relation.
inverse :: (Ord a) => Relation a -> Relation a
inverse (Relation dom cod m) =
    Relation cod dom m'
  where
    m' = Map.foldWithKey
        (\x ys m -> foldr (\y mp -> Map.insertWith (++) y [x] mp) m ys)
        Map.empty
        m        
-- | Return a list of all elements that @x@ gets mapped into.
apply :: (Ord a) => Relation a -> a -> [a]
apply (Relation dom cod m) x =
    case Map.lookup x m of
        Just l    -> l
        otherwise -> []
                          
-- | Compose @r1@ and @r2@.
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

-- | Test if @r@ is functional.
functional :: (Ord a) => Relation a -> Bool
functional r =
    all containsOne $ map (apply r) (defDomain r)
  where
    containsOne [x] = True
    containsOne _ = False

-- | Test if @r@ is injective.
injective :: (Ord a) => Relation a -> Bool
injective = functional . inverse

-- | Test if @r@ is surjective.
surjective :: (Ord a) => Relation a -> Bool
surjective = total . inverse

-- | Test if @r@ is total.
total :: (Ord a) => Relation a -> Bool
total r =
    sort (domain r) == sort (defDomain r)
                              
