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
    , updateNodeRelationGM
    , updateEdgeRelationGM
    , updateEdges
    , removeEdgeDom
    , removeEdgeCod
    , removeNodeDom
    , removeNodeCod
    , createEdgeDom
    , createEdgeCod
    -- * Query
    , applyNode
    , applyEdge
    , nodeRelation
    , edgeRelation
    , Graph.GraphMorphism.null
    , orphanNodes
    , orphanEdges
    
    , partialInjectiveGM
    , newNodesTyped
    , newEdgesTyped
    ) where

import qualified Abstract.Relation as R
import Graph.Graph as G
import Graph.Graph (Graph)
import Abstract.Morphism
import Abstract.Valid
import Data.Maybe (isNothing,fromJust,mapMaybe)

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
        concatMap (\e -> (show e) ++ " --> " ++ (show (applyEdge m e)) ++ " (from: " ++
          (show (fromJust (G.sourceOf (domain m) e))) ++ " -> " ++
          (show (fromJust (G.targetOf (domain m) e))) ++ ")\n")
                  (G.edges $ getDomain m)

-- | Infinite list of new node instances of a typed graph
newNodesTyped :: TypedGraph a b -> [G.NodeId]
newNodesTyped tg = G.newNodes $ domain tg
 
-- | Infinite list of new edge instances of a typed graph
newEdgesTyped :: TypedGraph a b -> [G.EdgeId]
newEdgesTyped tg = G.newEdges $ domain tg

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

-- | Remove an edge from the domain of the morphism
removeEdgeDom :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeDom e gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in  GraphMorphism (G.removeEdge e g1) g2 nm (R.removeDom e em)


-- | Remove an edge from the codomain of the morphism
removeEdgeCod :: G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
removeEdgeCod e gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in  GraphMorphism g1 (G.removeEdge e g2) nm (R.removeCod e em)


-- | Remove a node from the domain of the morphism
removeNodeDom :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeDom n gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in GraphMorphism (removeNode n g1) g2 (R.removeDom n nm) em 


-- | Remove a node from the codomain of the morphism
removeNodeCod :: G.NodeId -> GraphMorphism a b -> GraphMorphism a b
removeNodeCod n gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in GraphMorphism g1 (removeNode n g2) (R.removeCod n nm) em

---- Insertion of nodes in graph morphisms
-- verificar se nao é o mesmo q já existe
updateNodeRelationGM :: G.NodeId -> G.NodeId -> GraphMorphism a b -> GraphMorphism a b
updateNodeRelationGM n1 n2 gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in GraphMorphism (G.insertNode n1 g1) (G.insertNode n2 g2) (R.update n1 n2 nm) em

-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createEdgeDom :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
createEdgeDom e1 s1 t1 e2 gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in GraphMorphism (G.insertEdge e1 s1 t1 g1) g2 nm (R.update e1 e2 em)

-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism. 
--   It assumes that s2,t2 exist, and that e2 does not exist
createEdgeCod :: G.EdgeId -> G.NodeId -> G.NodeId -> GraphMorphism a b -> GraphMorphism a b
createEdgeCod e2 s2 t2 gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
      dom = R.domain em
      cod = R.codomain em
      m   = R.mapping em
  in GraphMorphism g1 (G.insertEdge e2 s2 t2 g2) nm (R.insertCod e2 em)


-- | modifies a graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelationGM :: G.EdgeId -> G.EdgeId -> GraphMorphism a b -> GraphMorphism a b
updateEdgeRelationGM e1 e2 gm = 
  let g1 = domain gm
      g2 = codomain gm
      nm = nodeRelation gm
      em = edgeRelation gm
  in GraphMorphism g1 g2 nm (R.update e1 e2 em)

-- | Test if a @nac@ is partial injective (injective out of @q@)
partialInjectiveGM :: GraphMorphism a b -> GraphMorphism a b -> Bool
partialInjectiveGM nac q = R.partInjectiveR nodes nodeR && R.partInjectiveR edges edgeR
  where
    nodeR = nodeRelation q
    nodes = mapMaybe (applyNode nac) (G.nodes (domain nac))
    edgeR = edgeRelation q
    edges = mapMaybe (applyEdge nac) (G.edges (domain nac))

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
