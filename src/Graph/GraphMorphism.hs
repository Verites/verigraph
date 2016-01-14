{-# LANGUAGE TypeFamilies #-}

module Graph.GraphMorphism (
    -- * Types
      GraphMorphism
    , TypedGraph
    -- * Construction
    , Graph.GraphMorphism.empty
    , graphMorphism
    , gmbuild
    -- * Transformation
    , inverse
    , updateCodomain
    , updateDomain
    , updateNodes
    , updateEdges
    -- * Query
    , applyNode
    , applyEdge
    , nodeRelation
    , edgeRelation
    , Graph.GraphMorphism.null
    ) where

import qualified Abstract.Relation as R
import Graph.Graph as G
import Graph.Graph (Graph)
import Abstract.Morphism
import Abstract.Valid
import Data.Maybe (isNothing)

data GraphMorphism a b = GraphMorphism {
                          getDomain    :: Graph a b
                        , getCodomain  :: Graph a b
                        , nodeRelation :: R.Relation G.NodeId
                        , edgeRelation :: R.Relation G.EdgeId
                    } deriving (Read)

instance Eq (GraphMorphism a b) where
    m1 == m2 = domain m1 == domain m2 &&
               codomain m1 == codomain m2 &&
               nodeRelation m1 == nodeRelation m2 &&
               edgeRelation m1 == edgeRelation m2

type TypedGraph a b = GraphMorphism a b

instance Show (GraphMorphism a b) where
    show m =
--        "Domain: " ++ (show $ getDomain m) ++
--        "\nCodomain: " ++ (show $ getCodomain m) ++
        "\nNode mappings: \n" ++
        concatMap (\n -> (show n) ++ " --> " ++ (show (applyNode m n)) ++ "\n")
                  (G.nodes $ getDomain m) ++
        "\nEdge mappings: \n" ++
        concatMap (\e -> (show e) ++ " --> " ++ (show (applyEdge m e)) ++ "\n")
                  (G.edges $ getDomain m)

-- | Return the node to which @ln@ gets mapped.
applyNode :: GraphMorphism a b -> G.NodeId -> Maybe G.NodeId
applyNode m ln =
    case R.apply (nodeRelation m) ln of
        (x:xs) -> Just x
        _ -> Nothing

-- | Return the edge to which @le@ gets mapped.
applyEdge :: GraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdge m le =
    case R.apply (edgeRelation m) le of
        (x:xs) -> Just x
        _ -> Nothing

-- | An empty morphism between two graphs.
empty :: Graph a b -> Graph a b -> GraphMorphism a b
empty gA gB = GraphMorphism gA gB (R.empty (nodes gA) (nodes gB)) (R.empty (edges gA) (edges gB))

-- | Construct a graph morphism
gmbuild :: Graph a b -> Graph a b -> [(Int,Int)] -> [(Int,Int)] -> GraphMorphism a b
gmbuild gA gB n e = foldr (\(a,b) -> updateEdges a b) g (map (\(x,y) -> (EdgeId x,EdgeId y)) e)
    where
        g = foldr (\(a,b) -> updateNodes a b) (Graph.GraphMorphism.empty gA gB) (map (\(x,y) -> (NodeId x,NodeId y)) n)

-- | Construct a graph morphism based on domain, codomain and both node and
-- edge relations.
graphMorphism = GraphMorphism

-- | The inverse graph morphism.
inverse (GraphMorphism dom cod nm em) =
    GraphMorphism cod dom (R.inverse nm) (R.inverse em)

-- | Test if the morphism is null.
null :: TypedGraph a b -> Bool
null = G.null . getDomain

-- | Set a new codomain.
updateCodomain :: Graph a b -> GraphMorphism a b -> GraphMorphism a b
updateCodomain g gm = gm { getCodomain = g }

-- | Set a new domain.
updateDomain :: Graph a b -> GraphMorphism a b -> GraphMorphism a b
updateDomain g gm = gm { getDomain = g }

-- | Add a mapping between both nodes into the morphism. If @ln@ is already
-- mapped, or neither nodes are in their respective graphs, return the original
-- morphism.
updateNodes :: NodeId -> NodeId -> GraphMorphism a b -> GraphMorphism a b
updateNodes ln gn morphism@(GraphMorphism l g nm em)
    | G.isNodeOf l ln && G.isNodeOf g gn && notMapped morphism ln =
        GraphMorphism l g (R.update ln gn nm) em
    | otherwise = morphism
  where
    notMapped m = isNothing . applyNode m

-- | Add a mapping between both edges into the morphism. If @le@ is already
-- mapped, or neither edges are in their respective graphs, return the original
-- morphism.
updateEdges :: EdgeId -> EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdges le ge morphism@(GraphMorphism l g nm em)
    | G.isEdgeOf l le && G.isEdgeOf g ge && notMapped morphism le =
        GraphMorphism l g nm (R.update le ge em)
    | otherwise = morphism
  where
    notMapped m = isNothing . applyEdge m


instance Morphism (GraphMorphism a b) where
    type Obj (GraphMorphism a b) = Graph a b

    domain = getDomain
    codomain = getCodomain
    compose m1 m2 =
        GraphMorphism (domain m1)
                      (codomain m2)
                      (R.compose (nodeRelation m1) (nodeRelation m2))
                      (R.compose (edgeRelation m1) (edgeRelation m2))
    id g = GraphMorphism g g (R.id $ nodes g) (R.id $ edges g)
    monomorphism m =
        R.injective (nodeRelation m) &&
        R.injective (edgeRelation m)
    epimorphism m =
        R.surjective (nodeRelation m) &&
        R.surjective (edgeRelation m)
    isomorphism m =
        monomorphism m && epimorphism m



instance Valid (GraphMorphism a b) where
    valid m@(GraphMorphism dom cod nm em) =
        R.total nm &&
        R.functional nm &&
        R.total em &&
        R.functional em &&
        valid dom &&
        valid cod &&
        all (\e -> (G.sourceOf cod =<< applyEdge m e) ==
                   (applyNode m =<< G.sourceOf dom e)
                   &&
                   (G.targetOf cod =<< applyEdge m e) ==
                   (applyNode m =<< G.targetOf dom e))
            (G.edges dom)
