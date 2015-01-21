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

data GraphGrammar a b = GraphGrammar {
                            getInitialGraph :: GraphMorphism a b
                          , getTypeGraph    :: Graph a b
                          , getRules        :: [(Int, GraphRule a b)]
                        }

graphGrammar = GraphGrammar
initialGraph = getInitialGraph
rules        = getRules
typeGraph    = getTypeGraph

