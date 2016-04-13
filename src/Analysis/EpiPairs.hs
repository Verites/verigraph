module Analysis.EpiPairs (EpiPairs(..)) where

import           Abstract.Morphism        (Morphism, Obj, codomain, compose)
import           Analysis.GPToVeri        (mountTGMBoth)
import           Analysis.GraphPart       (genEqClass)
import           Analysis.VeriToGP        (mixTGM)
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.GraphMorphism      (TypedGraph)

class Morphism m => EpiPairs m where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects.
  createPairs :: Obj m -> Obj m -> [(m, m)]

  -- | Create all jointly epimorphic pairs of morphisms from the codomains of
  -- the given morphisms.
  createPairsCodomain :: m -> m -> [(m, m)]
  createPairsCodomain m1 m2 = createPairs (codomain m1) (codomain m2)

  -- | Given two morphisms from the same domain, create all jointly epimorphic
  -- pairs of morphisms from their codomains, such that the square formed by
  -- all these morphisms commutes.
  --
  -- Given /f : X -> A/ and /g : X -> B/, obtain all jointly epimorphic pairs
  -- /(f', g')/ such that the following diagram commutes.
  --
  -- @
  --       g
  --    X----->B
  --    |      |
  --  f |      | f'
  --    v      v
  --    A----->Y
  --       g'
  -- @
  commutingPairs :: m -> m -> [(m, m)]


instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createPairs m1 m2 = map (mountTGMBoth m1 m2) (genEqClass (mixTGM m1 m2))

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes, considering they have same domain
  commutingPairs m1 m2 = filt
    where
      allPairs = createPairs (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
