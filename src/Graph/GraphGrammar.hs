module Graph.GraphGrammar (
      graphGrammar
    , GraphGrammar
    , initialGraph
    , rules
    , typeGraph
) where

import Graph.Graph (Graph)
import Graph.GraphMorphism
import Graph.GraphRule (GraphRule)
import qualified Abstract.Morphism as M

data GraphGrammar a b = GraphGrammar {
                            getInitialGraph :: GraphMorphism a b
                          , getRules        :: [(String, GraphRule a b)]
                        } deriving (Show, Read)

graphGrammar = GraphGrammar
initialGraph = getInitialGraph
rules        = getRules
typeGraph :: GraphGrammar a b -> Graph a b
typeGraph    = M.codomain . getInitialGraph

