module TypedGraph.GraphGrammar (
      graphGrammar
    , GraphGrammar
    , initialGraph
    , constraints
    , rules
    , sndOrderRules
    , typeGraph
) where

import           Abstract.AdhesiveHLR
import qualified Abstract.Morphism    as M
import           Graph.Graph          (Graph)
import           Graph.GraphMorphism
import           SndOrder.Rule        (SndOrderRule)
import           TypedGraph.GraphRule (GraphRule)
import           TypedGraph.Morphism

-- TODO: use a list of initial Graphs instead of a single graph
-- TODO: extract as DPO grammar?
data GraphGrammar a b = GraphGrammar {
                            initialGraph  :: GraphMorphism a b
                          , constraints   :: [AtomicConstraint (TypedGraphMorphism a b)]
                          , rules         :: [(String, GraphRule a b)]
                          , sndOrderRules :: [(String, SndOrderRule a b)]
                        } deriving (Show)

graphGrammar :: GraphMorphism a b -> [AtomicConstraint (TypedGraphMorphism a b)] -> [(String, GraphRule a b)] -> [(String, SndOrderRule a b)]
  -> GraphGrammar a b
graphGrammar = GraphGrammar

typeGraph :: GraphGrammar a b -> Graph a b
typeGraph    = M.codomain . initialGraph
