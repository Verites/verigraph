{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Relation
    (
    -- * Types
      Relation
    -- * Construction
    , empty
    , fromLists
    , fromMapAndCodomain
    -- * Transformation
    , compose
    , Data.Relation.id
    , inverseRelation
    , updateRelation
    , removeFromDomain
    , removeFromCodomain
    , insertOnCodomain
    , filterDomain
    -- * Query
    , apply
    , domain
    , codomain
    , image
    , mapping
    , orphans
    -- ** Predicates
    , isFunctional
    , isInjective
    , isSurjective
    , isTotal
    -- * Conversion
    -- ** Lists
    , toList
    ) where


import           Data.List as L
import qualified Data.Map  as Map

-- | Datatype for endorelations on a
data Relation a =
   Relation {
       domain   :: [a],
       codomain :: [a],
       mapping  :: Map.Map a [a]
   } deriving (Ord,Show,Read)

instance (Eq a, Ord a) => Eq (Relation a) where
    r1 == r2 = sort(domain r1) == sort(domain r2) &&
               sort(codomain r1) == sort(codomain r2) &&
               mapping r1 == mapping r2

-- | Construct a relation from the domain elements, the codomain elements, and
-- pairs of related elements, respectively. If a pair `(x,y)` is related, `x` is
-- added to the domain and `y` to the codomain.
fromLists :: Ord a => [a] -> [a] -> [(a, a)] -> Relation a
fromLists dom cod pairs =
  let
    mapping = foldl' (\m (k,v) -> Map.insertWith (++) k [v] m) Map.empty pairs
    domain = sort . nub $ dom ++ map fst pairs
    codomain = sort . nub $ cod ++ map snd pairs
  in Relation domain codomain mapping

toList :: Relation a -> [(a, a)]
toList r = [ (x, y) | (x, ys) <- Map.toList (mapping r), y <- ys ]

-- | Construct a relation from a map and a list of codomain elements.
fromMapAndCodomain :: Ord a => Map.Map a a -> [a] -> Relation a
fromMapAndCodomain mapping codomain =
  Relation (Map.keys mapping) (sort $ nub codomain) (Map.map (:[]) mapping)

-- | Return a list of all domain elements mapped by the relation.
listDomain :: Relation a -> [a]
listDomain = Map.keys . Map.filter (not . L.null) . mapping

-- | Return a list of all elements in the image of the relation.
image :: (Eq a) => Relation a -> [a]
image = nub . concat . Map.elems . mapping

-- | An empty relation, with domain and codomain specified.
empty :: Ord a => [a] -> [a] -> Relation a
empty dom cod = Relation  orderedDomain orderedCodomain emptyMap
  where
    orderedDomain = sort $ nub dom
    orderedCodomain = sort $ nub cod
    emptyMap = foldr (\x -> Map.insert x []) Map.empty orderedDomain

-- | The identity relation on @dom@.
id :: Ord a => [a] -> Relation a
id dom = Relation d d idMap
  where
    d = sort $ nub dom
    idMap = foldr (\x acc -> Map.insert x [x] acc) Map.empty dom

-- | Return the elements in the domain which are not in the image of the relation (orphans)
orphans :: (Eq a) => Relation a -> [a]
orphans r = (L.\\) (codomain r) (image r)

-- | Add a mapping between @x@ and @y@ to the relation. If @x@ already exists,
-- @y@ is joined to the corresponding elements.
updateRelation :: Ord a => a -> a -> Relation a -> Relation a
updateRelation x y (Relation dom cod m) = Relation ([x] `union` dom) ([y] `union` cod) (Map.insertWith insertUniquely x [y] m)
  where
    insertUniquely y y'
        | null $ y `L.intersect` y' = y ++ y'
        | otherwise = y'

-- | The inverse relation.
inverseRelation :: (Ord a) => Relation a -> Relation a
inverseRelation (Relation dom cod m) = Relation cod dom m'
  where
    m' = Map.foldrWithKey
        (\x ys m -> foldr (\y mp -> Map.insertWith (++) y [x] mp) m ys)
        Map.empty
        m
-- | Return a list of all elements that @x@ gets mapped into.
apply :: (Ord a) => Relation a -> a -> [a]
apply (Relation _ _ m) x =
    case Map.lookup x m of
        Just l -> l
        _      -> []

-- | Compose @r1@ and @r2@.
compose :: (Ord a) => Relation a -> Relation a -> Relation a
compose r1@(Relation dom _ _) r2@(Relation _ cod' _) = Relation dom cod' m''
  where
    m'' = foldr (intermidiaryCompose r1 r2) Map.empty $ listDomain r1

intermidiaryCompose :: Ord a => Relation a -> Relation a -> a -> Map.Map a [a] -> Map.Map a [a]
intermidiaryCompose r1 r2 a m =
  let im = do
            b <- apply r1 a
            apply r2 b
  in Map.insert a im m

-- | Remove an element from the domain of the relation, as well as any mappings involving it.
removeFromDomain :: Ord a => a -> Relation a -> Relation a
removeFromDomain x r = r { domain = L.delete x (domain r)
                         , mapping = Map.delete x (mapping r)
                         }

-- | Remove an element from the codomain of the relation
removeFromCodomain :: Ord a => a -> Relation a -> Relation a
removeFromCodomain x r = r { codomain = L.delete x (codomain r)
                           , mapping = Map.map (L.delete x) (mapping r)
                           }

-- | Insert an element on the codomain of the relation
insertOnCodomain :: Ord a => a -> Relation a -> Relation a
insertOnCodomain x r = r { codomain = [x] `union` codomain r }

-- | Remove the elements of the domain that don't pass the given test.
filterDomain :: (a -> Bool) -> Relation a -> Relation a
filterDomain test r = r
  { domain = L.filter test (domain r)
  , mapping = Map.filterWithKey (const . test) (mapping r)
  }

-- | Test if @r@ is functional.
isFunctional :: Relation a -> Bool
isFunctional r = all containsOne $ Map.elems (mapping r)
  where
    containsOne [_] = True
    containsOne _   = False

-- | Test if @r@ is injective.
isInjective :: (Ord a) => Relation a -> Bool
isInjective = isFunctional . inverseRelation

-- | Test if @r@ is surjective.
isSurjective :: (Ord a) => Relation a -> Bool
isSurjective = isTotal . inverseRelation

-- | Test if @r@ is total.
isTotal :: (Ord a) => Relation a -> Bool
isTotal r = sort (domain r) == sort (listDomain r)
