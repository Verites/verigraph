{-# LANGUAGE TypeFamilies #-}

module Graph.GraphMorphism (
    -- * Types
      GraphMorphism
    -- * Construction
    , Graph.GraphMorphism.empty
    , graphMorphism
    , gmbuild
    -- * Transformation
    , inverse
    , updateCodomain
    , updateDomain
    , updateNodes
    , updateNodeRelationGM
    , updateEdgeRelationGM
    , updateEdges
    , removeEdgeDom
    , removeEdgeCod
    , removeNodeDom
    , removeNodeCod
    , createEdgeDom
    , createEdgeCod
    , createNodeDom
    , createNodeCod
    -- * Query
    , applyNode
    , applyNodeUnsafe
    , applyEdge
    , applyEdgeUnsafe
    , nodeRelation
    , edgeRelation
    , orphanNodes
    , orphanEdges

    , partialInjectiveGM
    ) where

import           Abstract.Morphism
import qualified Abstract.Relation as R
import           Abstract.Valid
import           Data.List
import           Data.Maybe        (fromJust, fromMaybe, isNothing, mapMaybe)
import           Graph.Graph       as G

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

instance Show (GraphMorphism a b) where
    show m =
--        "Domain: " ++ (show $ getDomain m) ++
--        "\nCodomain: " ++ (show $ getCodomain m) ++
        "\nNode mappings: \n" ++
        concatMap (\n -> show n ++ " --> " ++ show (applyNode m n) ++ "\n")
                  (G.nodes $ getDomain m) ++
        "\nEdge mappings: \n" ++
        concatMap (\e -> show e ++ " --> " ++ show (applyEdge m e) ++ " (from: " ++
          show (fromJust (G.sourceOf (domain m) e)) ++ " -> " ++
          show (fromJust (G.targetOf (domain m) e)) ++ ")\n")
                  (G.edges $ getDomain m)

-- | Return the orphan nodes in a graph morphism
orphanNodes :: GraphMorphism a b -> [G.NodeId]
orphanNodes gm = R.orphans (nodeRelation gm)

-- | Return the orphan edges in a graph morphism
orphanEdges :: GraphMorphism a b -> [G.EdgeId]
orphanEdges gm = R.orphans (edgeRelation gm)

-- | Return the node to which @ln@ gets mapped.
applyNode :: GraphMorphism a b -> G.NodeId -> Maybe G.NodeId
applyNode m ln =
    case R.apply (nodeRelation m) ln of
        (x:_) -> Just x
        _ -> Nothing

-- | Return the edge to which @le@ gets mapped.
applyEdge :: GraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdge m le =
    case R.apply (edgeRelation m) le of
        (x:_) -> Just x
        _ -> Nothing

-- | Return the node to which @le@ gets mapped or error in the case of undefined
applyNodeUnsafe :: GraphMorphism a b -> NodeId -> NodeId
applyNodeUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

-- | Return the edge to which @le@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: GraphMorphism a b -> EdgeId -> EdgeId
applyEdgeUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

-- | An empty morphism between two graphs.
empty :: Graph a b -> Graph a b -> GraphMorphism a b
empty gA gB = GraphMorphism gA gB (R.empty (nodes gA) (nodes gB)) (R.empty (edges gA) (edges gB))

-- | Construct a graph morphism
gmbuild :: Graph a b -> Graph a b -> [(Int,Int)] -> [(Int,Int)] -> GraphMorphism a b
gmbuild gA gB n = foldr (uncurry updateEdges . (\(x,y) -> (EdgeId x,EdgeId y))) g
    where
        g = foldr (uncurry updateNodes . (\(x,y) -> (NodeId x,NodeId y))) (Graph.GraphMorphism.empty gA gB) n

-- | Construct a graph morphism based on domain, codomain and both node and
-- edge relations.
graphMorphism :: Graph a b -> Graph a b -> R.Relation NodeId -> R.Relation EdgeId -> GraphMorphism a b
graphMorphism = GraphMorphism

-- | The inverse graph morphism.
inverse :: GraphMorphism a b -> GraphMorphism a b
inverse (GraphMorphism dom cod nm em) =
    GraphMorphism cod dom (R.inverse nm) (R.inverse em)

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
    | G.isNodeOf l ln && G.isNodeOf g gn && notMapped morphism ln = GraphMorphism l g (R.update ln gn nm) em
    | otherwise = morphism
  where
    notMapped m = isNothing . applyNode m

-- | Add a mapping between both edges into the morphism. If @le@ is already
-- mapped, or neither edges are in their respective graphs, return the original
-- morphism.
updateEdges :: EdgeId -> EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdges le ge morphism@(GraphMorphism l g nm em)
    | G.isEdgeOf l le && G.isEdgeOf g ge && notMapped morphism le = GraphMorphism l g nm (R.update le ge em)
    | otherwise = morphism
  where
    notMapped m = isNothing . applyEdge m

-- | Remove an edge from the domain of the morphism
removeEdgeDom :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeDom e gm =
  gm { getDomain = removeEdge e (domain gm)
     , edgeRelation = R.removeDom e (edgeRelation gm)
     }

-- | Remove an edge from the codomain of the morphism
removeEdgeCod :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeCod e gm =
  gm { getCodomain = G.removeEdge e (codomain gm)
     , edgeRelation = R.removeCod e (edgeRelation gm)
     }

-- | Remove a node from the domain of the morphism
--
-- TODO: what happens if there were edges incident to the node?
removeNodeDom :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeDom n gm =
  gm { getDomain = removeNode n (domain gm)
     , nodeRelation = R.removeDom n (nodeRelation gm) }

-- | Remove a node from the codomain of the morphism
--
-- TODO: what happens if there were edges incident to the node?
removeNodeCod :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeCod n gm =
  gm { getCodomain = removeNode n (codomain gm)
     , nodeRelation = R.removeCod n (nodeRelation gm)
     }

-- | Insertion of nodes in graph morphisms
-- if the node do not exists, it is created
updateNodeRelationGM :: G.NodeId -> G.NodeId -> GraphMorphism a b -> GraphMorphism a b
updateNodeRelationGM n1 n2 gm =
  gm { getDomain = G.insertNode n1 (domain gm)
     , getCodomain = G.insertNode n2 (codomain gm)
     , nodeRelation = R.update n1 n2 (nodeRelation gm)
     }

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createEdgeDom :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
createEdgeDom e1 s1 t1 e2 gm =
  gm { getDomain = G.insertEdge e1 s1 t1 (domain gm)
     , edgeRelation = R.update e1 e2 (edgeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createEdgeCod :: G.EdgeId -> G.NodeId -> G.NodeId -> GraphMorphism a b -> GraphMorphism a b
createEdgeCod e2 s2 t2 gm =
  gm { getCodomain = G.insertEdge e2 s2 t2 (codomain gm)
     , edgeRelation = R.insertCod e2 (edgeRelation gm)
     }

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createNodeDom :: G.NodeId -> G.NodeId -> GraphMorphism a b -> GraphMorphism a b
createNodeDom n1 n2 gm =
  gm { getDomain = G.insertNode n1 (domain gm)
     , nodeRelation = R.update n1 n2 (nodeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createNodeCod :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
createNodeCod n2 gm =
  gm { getCodomain = G.insertNode n2 (codomain gm)
     , nodeRelation = R.insertCod n2 (nodeRelation gm)
     }

-- | modifies a graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelationGM :: G.EdgeId -> G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdgeRelationGM e1 e2 gm =
  gm { edgeRelation = R.update e1 e2 (edgeRelation gm) }

-- | Test if a @nac@ is partial injective (injective out of @q@)
partialInjectiveGM :: GraphMorphism a b -> GraphMorphism a b -> Bool
partialInjectiveGM nac q = disjointCodomain && injective
  where
    nodes = mapMaybe (applyNode nac) (G.nodes (domain nac))
    nodesI = G.nodes (codomain nac) \\ nodes
    codN = mapMaybe (applyNode q)
    edges = mapMaybe (applyEdge nac) (G.edges (domain nac))
    edgesI = G.edges (codomain nac) \\ edges
    codE = mapMaybe (applyEdge q)
    disjointNodes = Prelude.null (codN nodes `intersect` codN nodesI)
    disjointEdges = Prelude.null (codE edges `intersect` codE edgesI)
    disjointCodomain = disjointNodes && disjointEdges
    injective = R.partInjectiveR nodes (nodeRelation q) && R.partInjectiveR edges (edgeRelation q)

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
