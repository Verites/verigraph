{-# LANGUAGE TypeFamilies #-}
module Category.LabeledGraph.FinitaryCategory () where

import           Abstract.Category.FinitaryCategory
import qualified Data.EnumMap                       as EnumMap
import           Data.LabeledGraph                  (LabeledGraph)
import qualified Data.LabeledGraph                  as Graph
import           Data.LabeledGraph.Morphism
import           Data.Variable

instance FinitaryCategory LabeledMorphism where
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

  isMonomorphism = isInjective
  isEpimorphism = isSurjective
  isIsomorphism = isBijective
