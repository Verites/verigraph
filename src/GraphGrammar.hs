{-# LANGUAGE TypeFamilies #-}

module GraphGrammar (GraphGrammar) where

import GraphGrammarClass
import GraphRuleClass
import TypedMorphismClass
import MorphismClass
import Graph (Graph)
import GraphRule (GraphRule)
import Morphism (Morphism)

data GraphGrammar a b = GraphGrammar {
                            getTypeGraph    :: Morphism a b
                          , getInitialGraph :: Graph a b
                          , getRules        :: [(Int, GraphRule a b)]
                        }

instance GraphGrammarClass (GraphGrammar a b) where
    type R (GraphGrammar a b) = GraphRule a b
    type K (GraphGrammar a b) = Int

    typeGraph    = getTypeGraph
    initialGraph = getInitialGraph
    rules        = getRules

    graphGrammar = GraphGrammar

