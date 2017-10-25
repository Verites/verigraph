{-# LANGUAGE TypeFamilies #-}

module Data.Graphs.Morphism (
    -- * Types
      GraphMorphism(..)
    , compose
    -- * Construction
    , Data.Graphs.Morphism.empty
    , buildGraphMorphism
    , fromGraphsAndRelations
    , fromGraphsAndLists
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
    , applyNodeId
    , applyNodeIdUnsafe
    , applyEdge
    , applyEdgeUnsafe
    , applyEdgeId
    , applyEdgeIdUnsafe
    , orphanNodeIds
    , orphanEdgeIds
    , orphanEdges
    ) where

import           Control.Arrow
import           Data.Function (on)
import qualified Data.List     as List

import           Base.Valid
import           Data.Graphs   as G
import           Data.Maybe    (fromMaybe, isNothing)
import qualified Data.Relation as R

data GraphMorphism a b = GraphMorphism {
    domainGraph   :: Graph a b
  , codomainGraph :: Graph a b
  , nodeRelation  :: R.Relation G.NodeId
  , edgeRelation  :: R.Relation G.EdgeId
}

compose :: GraphMorphism a b -> GraphMorphism a b -> GraphMorphism a b
compose m2 m1 = GraphMorphism (domainGraph m1)
              (codomainGraph m2)
              (R.compose (nodeRelation m1) (nodeRelation m2))
              (R.compose (edgeRelation m1) (edgeRelation m2))

instance Eq (GraphMorphism a b) where
    m1 == m2 = domainGraph m1 == domainGraph m2 &&
               codomainGraph m1 == codomainGraph m2 &&
               nodeRelation m1 == nodeRelation m2 &&
               edgeRelation m1 == edgeRelation m2

instance Show (GraphMorphism a b) where
    show m = concat $
        "\nNode mappings: \n" : (map showNode . List.sort) (G.nodeIds $ domainGraph m)
        ++ "\nEdge mappings: \n" : (map showEdge . List.sortBy (compare `on` edgeId)) (G.edges $ domainGraph m)
      where
        showNode n =
          show n ++ " --> " ++ show (applyNodeId m n) ++ "\n"

        showEdge (Edge e srcId tgtId _) =
          show e ++ " --> " ++ show (applyEdgeId m e)
          ++ " (from: " ++ show srcId ++ " to:" ++ show tgtId ++ ")\n"

-- | Return the nodes ids of the codomain which are not in the image of the given morphism.
orphanNodeIds :: GraphMorphism a b -> [G.NodeId]
orphanNodeIds gm = R.orphans (nodeRelation gm)

-- | Return the edges of the codomain which are not in the image of the given morphism.
orphanEdges :: GraphMorphism a b -> [G.Edge b]
orphanEdges gm = map idToEdge (R.orphans (edgeRelation gm))
  where
    idToEdge id =
      fromMaybe
        (error "orphanEdges: EdgeId is not in graph")
        (lookupEdge id (codomainGraph gm))

-- | Return the edge ids of the codomain which are not in the image of the given morphism.
orphanEdgeIds :: GraphMorphism a b -> [G.EdgeId]
orphanEdgeIds gm = R.orphans (edgeRelation gm)

-- | Return the node to which @ln@ gets mapped.
applyNode :: GraphMorphism a b -> G.Node a -> Maybe (G.Node a)
applyNode m ln =
    case applyNodeId m (nodeId ln) of
        Just x  -> lookupNode x (codomainGraph m)
        Nothing -> Nothing

-- | Return the nodeId to which @ln@ gets mapped.
applyNodeId :: GraphMorphism a b -> G.NodeId -> Maybe G.NodeId
applyNodeId m ln =
    case R.apply (nodeRelation m) ln of
        (x:_) -> Just x
        _     -> Nothing

-- | Return the edge to which @le@ gets mapped.
applyEdge :: GraphMorphism a b -> G.Edge b -> Maybe (G.Edge b)
applyEdge m le =
    case applyEdgeId m (edgeId le) of
        Just x  -> lookupEdge x (codomainGraph m)
        Nothing -> Nothing

-- | Return the edgeId to which @le@ gets mapped.
applyEdgeId :: GraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdgeId m le =
    case R.apply (edgeRelation m) le of
        (x:_) -> Just x
        _     -> Nothing

-- | Return the node to which @le@ gets mapped or error in the case of undefined
applyNodeUnsafe :: GraphMorphism a b -> G.Node a -> G.Node a
applyNodeUnsafe morph n = fromMaybe (error "Error, apply nodeId in a non total morphism") $ applyNode morph n

-- | Return the nodeId to which @le@ gets mapped or error in the case of undefined
applyNodeIdUnsafe :: GraphMorphism a b -> NodeId -> NodeId
applyNodeIdUnsafe morph n = fromMaybe (error "Error, apply nodeId in a non total morphism") $ applyNodeId morph n

-- | Return the edge to which @le@ gets mapped or error in the case of undefined
applyEdgeUnsafe :: GraphMorphism a b -> G.Edge b -> G.Edge b
applyEdgeUnsafe morph e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge morph e

-- | Return the edgeId to which @le@ gets mapped or error in the case of undefined
applyEdgeIdUnsafe :: GraphMorphism a b -> EdgeId -> EdgeId
applyEdgeIdUnsafe morph e = fromMaybe (error "Error, apply edgeId in a non total morphism") $ applyEdgeId morph e

-- | An empty morphism between two graphs.
empty :: Graph a b -> Graph a b -> GraphMorphism a b
empty gA gB = GraphMorphism gA gB (R.empty (nodeIds gA) (nodeIds gB)) (R.empty (edgeIds gA) (edgeIds gB))

-- | Construct a graph morphism
buildGraphMorphism :: Graph a b -> Graph a b -> [(Int,Int)] -> [(Int,Int)] -> GraphMorphism a b
buildGraphMorphism gA gB n = foldr (uncurry updateEdges . (EdgeId *** EdgeId)) g
    where
        g = foldr (uncurry updateNodes . (NodeId *** NodeId)) (Data.Graphs.Morphism.empty gA gB) n

-- | Constructs a @GraphMorphism@ from two Graphs, a node relation and a edge relation.
fromGraphsAndRelations :: Graph a b -> Graph a b -> R.Relation NodeId -> R.Relation EdgeId -> GraphMorphism a b
fromGraphsAndRelations = GraphMorphism

-- | Constructs a @GraphMorphism@ from two Graphs, and lists describing the node and edge mappings.
fromGraphsAndLists :: Graph a b -> Graph a b -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> GraphMorphism a b
fromGraphsAndLists dom cod nodes edges = GraphMorphism dom cod nodeRelation edgeRelation
  where
    nodeRelation = R.fromLists (nodeIds dom) (nodeIds cod) nodes
    edgeRelation = R.fromLists (edgeIds dom) (edgeIds cod) edges

-- | The inverse graph morphism.
invertGraphMorphism :: GraphMorphism a b -> GraphMorphism a b
invertGraphMorphism (GraphMorphism dom cod nm em) =
    GraphMorphism cod dom (R.inverseRelation nm) (R.inverseRelation em)

-- | Set a new codomain.
updateCodomain :: Graph a b -> GraphMorphism a b -> GraphMorphism a b
updateCodomain g gm = gm { codomainGraph = g }

-- | Set a new domain.
updateDomain :: Graph a b -> GraphMorphism a b -> GraphMorphism a b
updateDomain g gm = gm { domainGraph = g }

-- | Add a mapping between both nodes into the morphism. If @ln@ is already
-- mapped, or neither nodes are in their respective graphs, return the original
-- morphism.
updateNodes :: NodeId -> NodeId -> GraphMorphism a b -> GraphMorphism a b
updateNodes ln gn morphism@(GraphMorphism l g nm em)
    | G.isNodeOf l ln && G.isNodeOf g gn && notMapped morphism ln = GraphMorphism l g (R.updateRelation ln gn nm) em
    | otherwise = morphism
  where
    notMapped m = isNothing . applyNodeId m

-- | Add a mapping between both edges into the morphism. If @le@ is already
-- mapped, or neither edges are in their respective graphs, return the original
-- morphism.
updateEdges :: EdgeId -> EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdges le ge morphism@(GraphMorphism l g nm em)
    | G.isEdgeOf l le && G.isEdgeOf g ge && notMapped morphism le = GraphMorphism l g nm (R.updateRelation le ge em)
    | otherwise = morphism
  where
    notMapped m = isNothing . applyEdgeId m

-- | Remove an edge from the domain of the morphism
removeEdgeFromDomain :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeFromDomain e gm =
  gm { domainGraph = removeEdge e (domainGraph gm)
     , edgeRelation = R.removeFromDomain e (edgeRelation gm)
     }

-- | Remove an edge from the codomain of the morphism
removeEdgeFromCodomain :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeFromCodomain e gm =
  gm { codomainGraph = G.removeEdge e (codomainGraph gm)
     , edgeRelation = R.removeFromCodomain e (edgeRelation gm)
     }

-- | Remove a node from the domain of the morphism.
-- Don't change the morphism if there were edges incident to the node.
removeNodeFromDomain :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromDomain n gm = if currentDomain == updatedDomain then gm else updatedGM
  where
    currentDomain = domainGraph gm
    updatedDomain = removeNode n currentDomain
    updatedGM = gm { domainGraph = updatedDomain
                   , nodeRelation = R.removeFromDomain n $ nodeRelation gm }

-- | Remove a node from the domain of the morphism
-- It does not verify if the node has incident edges, thus it may generate invalid graph morphisms.
removeNodeFromDomainForced :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromDomainForced n gm =
  gm { domainGraph = removeNodeForced n (domainGraph gm)
     , nodeRelation = R.removeFromDomain n (nodeRelation gm) }

-- | Remove a node from the codomain of the morphism
-- Don't change the morphism if there were edges incident to the node.
removeNodeFromCodomain :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeFromCodomain n gm =  if currentCodomain == updatedCodomain then gm else updatedGM
  where
    currentCodomain = codomainGraph gm
    updatedCodomain = removeNode n currentCodomain
    updatedGM = gm { codomainGraph = updatedCodomain
                   , nodeRelation = R.removeFromCodomain n $ nodeRelation gm }

-- | Inserts nodes in a graph morphism, if the nodes do not exist, they are created
updateNodeRelation :: G.NodeId -> G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
updateNodeRelation n1 n2 gm =
  gm { domainGraph = G.insertNode n1 (domainGraph gm)
     , codomainGraph = G.insertNode n2 (codomainGraph gm)
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
  gm { domainGraph = G.insertEdge e1 s1 t1 (domainGraph gm)
     , edgeRelation = R.updateRelation e1 e2 (edgeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createEdgeOnCodomain :: G.EdgeId -> G.NodeId -> G.NodeId -> GraphMorphism a (Maybe b) -> GraphMorphism a (Maybe b)
createEdgeOnCodomain e2 s2 t2 gm =
  gm { codomainGraph = G.insertEdge e2 s2 t2 (codomainGraph gm)
     , edgeRelation = R.insertOnCodomain e2 (edgeRelation gm)
     }

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createNodeOnDomain :: G.NodeId -> G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
createNodeOnDomain n1 n2 gm =
  gm { domainGraph = G.insertNode n1 (domainGraph gm)
     , nodeRelation = R.updateRelation n1 n2 (nodeRelation gm)
     }

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism.
--   It assumes that s2,t2 exist, and that e2 does not exist
createNodeOnCodomain :: G.NodeId -> GraphMorphism (Maybe a) b -> GraphMorphism (Maybe a) b
createNodeOnCodomain n2 gm =
  gm { codomainGraph = G.insertNode n2 (codomainGraph gm)
     , nodeRelation = R.insertOnCodomain n2 (nodeRelation gm)
     }


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
            (\e@(Edge _ domSrc domTgt _) ->
                (Just . sourceId =<< applyEdge morphism e) == applyNodeId morphism domSrc
             && (Just . targetId =<< applyEdge morphism e) == applyNodeId morphism domTgt)
            (G.edges dom)
