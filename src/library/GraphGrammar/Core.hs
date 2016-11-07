module GraphGrammar.Core (
      graphGrammar
    , GraphGrammar
    , initialGraph
    , constraints
    , rules
    , sndOrderRules
    , typeGraph
) where

import           Abstract.AdhesiveHLR
import qualified Abstract.Morphism        as M
import           Graph.Graph              (Graph)
import           SndOrder.Rule            (SndOrderRule)
import           TypedGraph.Graph         (TypedGraph)
import           TypedGraph.DPO.GraphRule (GraphRule)
import           TypedGraph.Morphism

-- TODO: use a list of initial Graphs instead of a single graph
-- TODO: extract as DPO grammar?
data GraphGrammar a b =
  GraphGrammar {
      initialGraph  :: TypedGraph a b
    , constraints   :: [Constraint (TypedGraphMorphism a b)]
    , rules         :: [(String, GraphRule a b)]
    , sndOrderRules :: [(String, SndOrderRule a b)]
    } deriving (Show)

graphGrammar ::
     TypedGraph a b -> [Constraint (TypedGraphMorphism a b)]
  -> [(String, GraphRule a b)] -> [(String, SndOrderRule a b)]
  -> GraphGrammar a b
graphGrammar = GraphGrammar

typeGraph :: GraphGrammar a b -> Graph a b
typeGraph = M.codomain . initialGraph
