module XML.GGXReader where

import           TypedGraph.GraphRule
import           TypedGraph.Morphism
import           TypedGraph.Graph
import           XML.ParsedTypes

instantiateRule :: ParsedTypeGraph -> RuleWithNacs -> GraphRule a b

instantiateSpan :: TypedGraph a b -> TypedGraph a b -> [Mapping] -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
