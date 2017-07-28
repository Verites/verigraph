{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Category.Graph
( Graph
, GraphMorphism
, GraphCat
, runCat
) where


import Data.Functor.Identity

import           Abstract.Category.NewClasses
import           Data.Graphs                        as G
import           Data.Graphs.Morphism
import qualified Data.Relation                      as R


newtype GraphCat n e a = GC { unGC :: Identity a }
    deriving (Functor, Applicative, Monad)

runCat :: GraphCat n e a -> a
runCat = runIdentity . unGC

instance Category (GraphCat n e) (GraphMorphism n e) where
  type Obj (GraphCat n e) = Graph n e

  domain = domainGraph
  codomain = codomainGraph

  m2 <&> m1 = compose m2 m1
  identity g = GraphMorphism g g (R.id $ nodeIds g) (R.id $ edgeIds g)

  data MorphismClass (GraphCat n e) 
    = AllMorphisms
    | Monomorphisms
    | Epimorphisms
    | Isomorphisms
    
  anyMorphism = AllMorphisms
  monic = Monomorphisms
  epic = Epimorphisms
  iso = Isomorphisms

  isSubclassOf c1 c2 = return $ isSubclassOf' c1 c2
    where
      isSubclassOf' _ AllMorphisms = True
      isSubclassOf' Monomorphisms Monomorphisms = True
      isSubclassOf' Epimorphisms Epimorphisms = True
      isSubclassOf' Isomorphisms Isomorphisms = True
      isSubclassOf' Isomorphisms Monomorphisms = True
      isSubclassOf' Isomorphisms Epimorphisms = True
      isSubclassOf' _ _ = False
    
  belongsToClass f c = return $ f `belongsTo` c
    where
      _ `belongsTo` AllMorphisms = True
      f `belongsTo` Monomorphisms = R.isInjective (nodeRelation f) && R.isInjective (edgeRelation f)
      f `belongsTo` Epimorphisms = R.isSurjective (nodeRelation f) && R.isSurjective (edgeRelation f)
      f `belongsTo` Isomorphisms = f `belongsTo` Monomorphisms && f `belongsTo` Epimorphisms
