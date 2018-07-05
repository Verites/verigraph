{-# LANGUAGE TypeFamilies #-}
module Category.LabeledGraph.Category
  ( LabeledGraph
  , LabeledMorphism
  , MorphismType(..)
  , toMorphismClass
  , toMorphismType ) where

import           Abstract.Category
import qualified Data.EnumMap               as EnumMap
import           Data.LabeledGraph          (LabeledGraph)
import qualified Data.LabeledGraph          as Graph
import           Data.LabeledGraph.Morphism
import           Data.Variable

data MorphismType =
    AnyMorphism | Monomorphism | Epimorphism | Isomorphism
    deriving Eq

toMorphismClass :: MorphismType -> MorphismClass LabeledMorphism
toMorphismClass = Cls

instance Category LabeledMorphism where
  type Obj LabeledMorphism = LabeledGraph

  domain = domainGraph
  codomain = codomainGraph
  (<&>) = compose

  identity g = LabeledMorphism
    { domainGraph = g
    , codomainGraph = g
    , nodeMap = EnumMap.fromList [(n, n) | n <- Graph.nodeIds g]
    , edgeMap = EnumMap.fromList [(e, e) | e <- Graph.edgeIds g]
    , variableMap = EnumMap.fromList [(v, v) | v <- freeVariableIdsOf g ]
    }

  newtype MorphismClass LabeledMorphism =
    Cls { toMorphismType :: MorphismType }
    deriving Eq

  anyMorphism = Cls AnyMorphism
  monic = Cls Monomorphism
  epic = Cls Epimorphism
  iso = Cls Isomorphism

  _ `belongsToClass` Cls AnyMorphism = True

  m `belongsToClass` Cls Monomorphism = isInjective m

  m `belongsToClass` Cls Epimorphism = isSurjective m

  m `belongsToClass` Cls Isomorphism =
    m `belongsToClass` Cls Monomorphism &&
    m `belongsToClass` Cls Epimorphism

  _ `isSubclassOf` Cls AnyMorphism = True

  Cls Monomorphism `isSubclassOf` Cls Monomorphism = True
  Cls Epimorphism `isSubclassOf` Cls Epimorphism = True
  Cls Isomorphism `isSubclassOf` Cls Isomorphism = True
  Cls Isomorphism `isSubclassOf` Cls Monomorphism = True
  Cls Isomorphism `isSubclassOf` Cls Epimorphism = True
  _ `isSubclassOf` _ = False



