module XML.GGXReader where

import           Graph.GraphMorphism
import           Graph.GraphRule
import           Graph.TypedGraphMorphism
import           XML.ParsedTypes

instantiateRule :: ParsedTypeGraph -> RuleWithNacs -> GraphRule a b

instantiateSpan :: TypedGraph a b -> TypedGraph a b -> [Mapping] -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
