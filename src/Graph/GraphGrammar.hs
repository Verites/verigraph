module Graph.GraphGrammar (
      graphGrammar
    , GraphGrammar
    , initialGraph
    , rules
    , sndOrderRules
    , typeGraph
) where

import qualified Abstract.Morphism   as M
import           Graph.Graph         (Graph)
import           Graph.GraphMorphism
import           Graph.GraphRule     (GraphRule)
import           Graph.SndOrderRule  (SndOrderRule)

-- TODO: extract as DPO grammar?
data GraphGrammar a b = GraphGrammar {
                            initialGraph  :: GraphMorphism a b
                          , rules         :: [(String, GraphRule a b)]
                          , sndOrderRules :: [SndOrderRule a b]
                        } deriving (Show)--, Read)

graphGrammar :: GraphMorphism a b -> [(String, GraphRule a b)] -> [SndOrderRule a b] -> GraphGrammar a b
graphGrammar = GraphGrammar

typeGraph :: GraphGrammar a b -> Graph a b
typeGraph    = M.codomain . initialGraph
