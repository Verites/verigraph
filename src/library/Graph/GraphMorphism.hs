{-# LANGUAGE TypeFamilies #-}

module Graph.GraphMorphism (
    -- * Types
      GraphMorphism
    -- * Construction
    , Graph.GraphMorphism.empty
    , buildGraphMorphism
    -- * Transformation
    , invertGraphMorphism
    , updateCodomain
    , updateDomain
    , updateNodes
    , updateNodeRelation
    , updateEdgeRelation
    , updateEdges
    , removeEdgeFromDomain
    , removeEdgeFromCodomain
    , removeNodeFromDomain
    , removeNodeFromDomainForced
    , removeNodeFromCodomain
    , createEdgeOnDomain
    , createEdgeOnCodomain
    , createNodeOnDomain
    , createNodeOnCodomain
    -- * Query
    , applyNode
    , applyNodeUnsafe
    , applyEdge
    , applyEdgeUnsafe
    , nodeRelation
    , edgeRelation
    , orphanNodeIds
    , orphanEdgeIds
    , orphanEdges

    , isPartialInjective
    ) where

import           Abstract.Morphism
import qualified Abstract.Relation as R
import           Abstract.Valid
import           Data.List
import           Data.Maybe        (fromMaybe, isNothing, mapMaybe)
import           Graph.Graph       as G

data GraphMorphism a b = GraphMorphism {
                          getDomain    :: Graph a b
                        , getCodomain  :: Graph a b
                        , nodeRelation :: R.Relation G.NodeId
                        , edgeRelation :: R.Relation G.EdgeId
                    }

instance Eq (GraphMorphism a b) where
    m1 == m2 = domain m1 == domain m2 &&
               codomain m1 == codomain m2 &&
               nodeRelation m1 == nodeRelation m2 &&
               edgeRelation m1 == edgeRelation m2

instance Show (GraphMorphism a b) where
    show m =
      "\nNode mappings: \n" ++ concatMap showNode (G.nodeIds $ getDomain m)
      ++ "\nEdge mappings: \n" ++ concatMap showEdge (G.edges $ getDomain m)
     where
       showNode n =
         show n ++ " --> " ++ show (applyNode m n) ++ "\n"

       showEdge (Edge e srcId tgtId _) =
         show e ++ " --> " ++ show (applyEdge m e)
         ++ " (from: " ++ show srcId ++ " to:" ++ show tgtId ++ ")\n"

-- | Return the orphan nodes ids in a graph morphism
orphanNodeIds :: GraphMorphism a b -> [G.NodeId]
orphanNodeIds gm = R.orphans (nodeRelation gm)

-- | Return the orphan edges in a graph morphism
orphanEdges :: GraphMorphism a b -> [G.Edge b]
orphanEdges gm = map idToEdge (R.orphans (edgeRelation gm))
  where
    idToEdge id =
      fromMaybe
        (error "orphanEdges: EdgeId is not in graph")
        (lookupEdge id (domain gm))

-- | Return the orphan edgesIds in a graph morphism
orphanEdgeIds :: GraphMorphism a b -> [G.EdgeId]
orphanEdgeIds gm = R.orphans (edgeRelation gm)

-- | Return the node to which @ln@ gets mapped.
applyNode :: GraphMorphism a b -> G.NodeId -> Maybe G.NodeId
applyNode m ln =
    case R.apply (nodeRelation m) ln of
        (x:_) -> Just x
        _     -> Nothing

-- | Return the edge to which @le@ gets mapped.
applyEdge :: GraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdge m le =
    case R.apply (edgeRelation m) le of
        (x:_) -> Just x
        _     -> Nothing

-- | Return the node to which @le@ gets mapped or error in the case of undefined
applyNodeUnsafe :: GraphMorphism a b -> NodeId -> NodeId
applyNodeUnsafe m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

-- | Return the edge to which @le@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: GraphMorphism a b -> EdgeId -> EdgeId
applyEdgeUnsafe m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

-- | An empty morphism between two graphs.
empty :: Graph a b -> Graph a b -> GraphMorphism a b
empty gA gB = GraphMorphism gA gB (R.empty (nodeIds gA) (nodeIds gB)) (R.empty (edgeIds gA) (edgeIds gB))

-- | Construct a graph morphism
buildGraphMorphism :: Graph a b -> Graph a b -> [(Int,Int)] -> [(Int,Int)] -> GraphMorphism a b
buildGraphMorphism gA gB n = foldr (uncurry updateEdges . (\(x,y) -> (EdgeId x,EdgeId y))) g
    where
        g = foldr (uncurry updateNodes . (\(x,y) -> (NodeId x,NodeId y))) (Graph.GraphMorphism.empty gA gB) n

-- | The inverse graph morphism.
invertGraphMorphism :: GraphMorphism a b -> GraphMorphism a b
invertGraphMorphism (GraphMorphism dom cod nm em) =
    GraphMorphism cod dom (R.inverseRelation nm) (R.inverseRelation em)

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
    | G.isNodeOf l ln && G.isNodeOf g gn && notMapped morphism ln = GraphMorphism l g (R.updateRelation ln gn nm) em
    | otherwise = morphism
  where
    notMapped m = isNothing . applyNode m

-- | Add a mapping between both edges into the morphism. If @le@ is already
-- mapped, or neither edges are in their respective graphs, return the original
-- morphism.
updateEdges :: EdgeId -> EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdges le ge morphism@(GraphMorphism l g nm em)
    | G.isEdgeOf l le && G.isEdgeOf g ge && notMapped morphism le = GraphMorphism l g nm (R.updateRelation le ge em)
    | otherwise = morphism
  where
    notMapped m = isNothing . applyEdge m

-- | Remove an edge from the domain of the morphism
removeEdgeFromDomain :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeFromDomain e gm =
  gm { getDomain = removeEdge e (domain gm)
     , edgeRelation = R.removeFromDomain e (edgeRelation gm)
     }

-- | Remove an edge from the codomain of the morphism
removeEdgeFromCodomain :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeFromCodomain e gm =
  gm { getCodomain = G.removeEdge e (codomain gm)
     , edgeRelation = R.removeFromCodomain e (edgeRelation gm)
     }

-- | Remove a node from the domain of the morphism.
-- Don't change the morphism if there were edges incident to the node.
removeNodeFromDomain :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromDomain n gm = if currentDomain == updatedDomain then gm else updatedGM
  where
    currentDomain = domain gm
    updatedDomain = removeNode n currentDomain
    updatedGM = gm { getDomain = updatedDomain
                   , nodeRelation = R.removeFromDomain n $ nodeRelation gm }

-- | Remove a node from the domain of the morphism
-- It does not verify if the node has incident edges, thus it may generate invalid graph morphisms.
removeNodeFromDomainForced :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromDomainForced n gm =
  gm { getDomain = removeNodeForced n (domain gm)
     , nodeRelation = R.removeFromDomain n (nodeRelation gm) }

-- | Remove a node from the codomain of the morphism
-- Don't change the morphism if there were edges incident to the node.
removeNodeFromCodomain :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromCodomain n gm =  if currentCodomain == updatedCodomain then gm else updatedGM
  where
    currentCodomain = codomain gm
    updatedCodomain = removeNode n currentCodomain
    updatedGM = gm { getCodomain = updatedCodomain
                   , nodeRelation = R.removeFromCodomain n $ nodeRelation gm }

-- | Inserts nodes in a graph morphism, if the nodes do not exist, they are created
updateNodeRelation :: G.NodeId -> G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
updateNodeRelation n1 n2 gm =
  gm { getDomain = G.insertNode n1 (domain gm)
     , getCodomain = G.insertNode n2 (codomain gm)
     , nodeRelation = R.updateRelation n1 n2 (nodeRelation gm)
     }

-- | Modifies a graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelation :: G.EdgeId -> G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdgeRelation e1 e2 gm =
  gm { edgeRelation = R.updateRelation e1 e2 (edgeRelation gm) }

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createEdgeOnDomain :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> GraphMorphism a (Maybe b) -> GraphMorphism a (Maybe b)
createEdgeOnDomain e1 s1 t1 e2 gm =
  gm { getDomain = G.insertEdge e1 s1 t1 (domain gm)
     , edgeRelation = R.updateRelation e1 e2 (edgeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createEdgeOnCodomain :: G.EdgeId -> G.NodeId -> G.NodeId -> GraphMorphism a (Maybe b) -> GraphMorphism a (Maybe b)
createEdgeOnCodomain e2 s2 t2 gm =
  gm { getCodomain = G.insertEdge e2 s2 t2 (codomain gm)
     , edgeRelation = R.insertOnCodomain e2 (edgeRelation gm)
     }

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createNodeOnDomain :: G.NodeId -> G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
createNodeOnDomain n1 n2 gm =
  gm { getDomain = G.insertNode n1 (domain gm)
     , nodeRelation = R.updateRelation n1 n2 (nodeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createNodeOnCodomain :: G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
createNodeOnCodomain n2 gm =
  gm { getCodomain = G.insertNode n2 (codomain gm)
     , nodeRelation = R.insertOnCodomain n2 (nodeRelation gm)
     }

-- | Test if a @nac@ is partial injective (injective out of @q@)
isPartialInjective :: GraphMorphism a b -> GraphMorphism a b -> Bool
isPartialInjective nac q = disjointCodomain && injective
  where
    nodes = mapMaybe (applyNode nac) (G.nodeIds (domain nac))
    nodesI = G.nodeIds (codomain nac) \\ nodes
    codN = mapMaybe (applyNode q)
    edges = mapMaybe (applyEdge nac) (G.edgeIds (domain nac))
    edgesI = G.edgeIds (codomain nac) \\ edges
    codE = mapMaybe (applyEdge q)
    disjointNodes = Prelude.null (codN nodes `intersect` codN nodesI)
    disjointEdges = Prelude.null (codE edges `intersect` codE edgesI)
    disjointCodomain = disjointNodes && disjointEdges
    injective = R.isPartialInjective nodes (nodeRelation q) && R.isPartialInjective edges (edgeRelation q)

instance Morphism (GraphMorphism a b) where
    type Obj (GraphMorphism a b) = Graph a b

    domain = getDomain
    codomain = getCodomain
    compose m1 m2 =
        GraphMorphism (domain m1)
                      (codomain m2)
                      (R.compose (nodeRelation m1) (nodeRelation m2))
                      (R.compose (edgeRelation m1) (edgeRelation m2))
    id g = GraphMorphism g g (R.id $ nodeIds g) (R.id $ edgeIds g)
    isMonomorphism m =
        R.isInjective (nodeRelation m) &&
        R.isInjective (edgeRelation m)
    isEpimorphism m =
        R.isSurjective (nodeRelation m) &&
        R.isSurjective (edgeRelation m)
    isIsomorphism m =
        isMonomorphism m && isEpimorphism m



instance Valid (GraphMorphism a b) where
    validate morphism@(GraphMorphism dom cod nodeMap edgeMap) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , ensure (R.isFunctional nodeMap) "The relation of nodes is not functional"
        , ensure (R.isTotal nodeMap) "The function of nodes is not total on its domain"
        , ensure (R.isFunctional edgeMap) "The relation of edges is not functional"
        , ensure (R.isTotal edgeMap) "The function of edges is not total on its domain"
        , ensure incidencePreserved "The morphism doesn't preserve incidence/adjacency"
        ]
      where
        incidencePreserved =
          all
            (\(Edge e domSrc domTgt _) ->
                (G.sourceOf cod =<< applyEdge morphism e) == applyNode morphism domSrc
                  && (G.targetOf cod =<< applyEdge morphism e) == applyNode morphism domTgt)
            (G.edges dom)
