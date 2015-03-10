module GraphGrammar (
      graphGrammar
    , GraphGrammar
    , initialGraph
    , rules
    , typeGraph
) where

import Graph (Graph)
import GraphMorphism
import GraphRule (GraphRule)
import qualified Morphism as M

data GraphGrammar a b = GraphGrammar {
                            getInitialGraph :: GraphMorphism a b
                          , getRules        :: [(String, GraphRule a b)]
                        } deriving (Show, Read)

graphGrammar = GraphGrammar
initialGraph = getInitialGraph
rules        = getRules
typeGraph :: (Eq a, Eq b) => GraphGrammar a b -> Graph a b
typeGraph    = M.codomain . getInitialGraph

