{-# LANGUAGE TypeFamilies #-}

module Graph.TypedGraphMorphism (
      TypedGraphMorphism
    , idMap
    , partialInjectiveTGM
    , invertTGM
    , nodesDomain
    , edgesDomain
    , nodesCodomain
    , edgesCodomain
    , graphDomain
    , graphCodomain
    , mapping
    , applyNodeTGM
    , applyEdgeTGM
    , typedMorphism
    , removeNodeDomTyped
    , removeEdgeDomTyped
    , removeNodeCodTyped
    , removeEdgeCodTyped
    , createEdgeDomTGM
    , createEdgeCodTGM
    , updateEdgeRelationTGM
    , updateNodeRelationTGM
    , orphanNodesTyped
    , orphanEdgesTyped
) where

import           Abstract.Morphism   as M
import           Abstract.AdhesiveHLR
import           Abstract.Valid
import           Data.Maybe
import           Data.List           ((\\))
import           Graph.Graph         (Graph, edges, nodes)
import           Graph.Graph         as G
import           Graph.GraphMorphism as GM

data TypedGraphMorphism a b = TypedGraphMorphism {
                              getDomain   :: GM.GraphMorphism a b
                            , getCodomain :: GM.GraphMorphism a b
                            , getMapping  :: GM.GraphMorphism a b
                         } deriving (Show, Read)

typedMorphism = TypedGraphMorphism
mapping = getMapping

-- | Return the graph domain
graphDomain :: TypedGraphMorphism a b -> Graph a b
graphDomain = M.domain . M.domain

-- | Return the graph codomain
graphCodomain :: TypedGraphMorphism a b -> Graph a b
graphCodomain = M.domain . M.codomain

-- | Return the node to which @ln@ gets mapped.
applyNodeTGM :: TypedGraphMorphism a b -> G.NodeId -> Maybe G.NodeId
applyNodeTGM tgm = GM.applyNode (mapping tgm)

-- | Return the edge to which @le@ gets mapped.
applyEdgeTGM :: TypedGraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdgeTGM tgm = GM.applyEdge (mapping tgm)

-- | Return the orphan nodes in a typed graph morphism
orphanNodesTyped :: TypedGraphMorphism a b -> [G.NodeId]
orphanNodesTyped tgm = GM.orphanNodes (mapping tgm)

-- | Return the orphan edges in a typed graph morphism
orphanEdgesTyped :: TypedGraphMorphism a b -> [G.EdgeId]
orphanEdgesTyped tgm = GM.orphanEdges (mapping tgm)

-- | Invert a typed graph morphism
invertTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b
invertTGM tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism cod dom (GM.inverse m)

-- | Return the nodes in the domain of this TGM
nodesDomain :: TypedGraphMorphism a b -> [NodeId]
nodesDomain = nodes . M.domain . getDomain

-- | Return the edges in the domain of this TGM
edgesDomain :: TypedGraphMorphism a b -> [EdgeId]
edgesDomain = edges . M.domain . getDomain

-- | Return the nodes in the codomain of this TGM
nodesCodomain :: TypedGraphMorphism a b -> [NodeId]
nodesCodomain = nodes . M.domain . getCodomain

-- | Return the edges in the codomain of this TGM
edgesCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgesCodomain = edges . M.domain . getCodomain

-- | This function adds an edge e1 (with source s1, target t1 and type tp) to the domain of the typed graph morphism, and associate it to e2
--   It assumes s1, t1, e2, tp already exist, and that e1 does not exist.
createEdgeDomTGM :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeDomTGM e1 s1 t1 tp e2 tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism (GM.createEdgeDom e1 s1 t1 tp dom)
                           cod
                           (GM.createEdgeDom e1 s1 t1 e2 m)


-- | This function adds an edge e2 (with source s2, target t2 and type tp) to the codomain of the typed graph morphism
--   It assumes s2, t2, tp already exist, and that e2 does not exist.
createEdgeCodTGM :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
createEdgeCodTGM e2 s2 t2 tp tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism dom
                           (GM.createEdgeDom e2 s2 t2 tp cod)
                           (GM.createEdgeCod e2 s2 t2 m)

-- | updates a typed graph morphism, mapping node n1 to node n2. It assumes both nodes already exist.
updateNodeRelationTGM :: G.NodeId -> G.NodeId -> G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateNodeRelationTGM n1 n2 tp tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism (GM.updateNodeRelationGM n1 tp dom)
                        (GM.updateNodeRelationGM n2 tp cod)
                        (GM.updateNodeRelationGM n1 n2 m)

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelationTGM :: G.EdgeId -> G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
updateEdgeRelationTGM e1 e2 tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism dom cod (GM.updateEdgeRelationGM e1 e2 m)

-- | Remove a node from the domain of a typed graph morphism
removeNodeDomTyped :: G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeDomTyped n tgm  =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism (GM.removeNodeDom n dom) cod (GM.removeNodeDom n m)


-- | Remove an edge from the domain of a typed graph morphism
removeEdgeDomTyped :: G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeDomTyped e tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism (GM.removeEdgeDom e dom) cod (GM.removeEdgeDom e m)


-- | Remove a node from the codomain of a typed graph morphism
removeNodeCodTyped :: G.NodeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeNodeCodTyped n tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism dom (GM.removeNodeDom n cod) (GM.removeNodeCod n m)


-- | Remove an edge from the domain of a typed graph morphism
removeEdgeCodTyped :: G.EdgeId -> TypedGraphMorphism a b -> TypedGraphMorphism a b
removeEdgeCodTyped e tgm =
  let dom = M.domain tgm
      cod = M.codomain tgm
      m   = mapping tgm
  in TypedGraphMorphism dom (GM.removeEdgeDom e cod) (GM.removeEdgeCod e m)

-- | Test if a @nac@ is partial injective (injective out of @m@)
partialInjectiveTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
partialInjectiveTGM nac q = GM.partialInjectiveGM (getMapping nac) (getMapping q)

-- | Creates a TGM mapping the same elements of theirs codomains, from @tgm1@ to @tgm2@
idMap :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
idMap tgm1@(TypedGraphMorphism _ cod1 _) tgm2@(TypedGraphMorphism _ cod2 _) =
  typedMorphism cod1 cod2 edgesUpdate
    where
      init = GM.empty (M.domain cod1) (M.domain cod2)
      nodesUpdate = foldr (\n -> GM.updateNodes n n) init (G.nodes (M.domain cod1))
      edgesUpdate = foldr (\e -> GM.updateEdges e e) nodesUpdate (G.edges (M.domain cod1))

instance Eq (TypedGraphMorphism a b) where
    (TypedGraphMorphism dom1 cod1 m1) == (TypedGraphMorphism dom2 cod2 m2) =
        dom1 == dom2 &&
        cod1 == cod2 &&
        m1 == m2

instance Morphism (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = GraphMorphism a b

    domain = Graph.TypedGraphMorphism.getDomain
    codomain = Graph.TypedGraphMorphism.getCodomain
    compose t1 t2 =
        TypedGraphMorphism (domain t1)
                      (codomain t2)
                      $ compose (getMapping t1)
                                (getMapping t2)
    id t = TypedGraphMorphism t t (M.id $ domain t)
    monomorphism = monomorphism . mapping
    epimorphism = epimorphism . mapping
    isomorphism = isomorphism . mapping


instance Valid (TypedGraphMorphism a b) where
    valid (TypedGraphMorphism dom cod m) =
        valid dom &&
        valid cod &&
        dom == compose m cod

instance AdhesiveHLR (TypedGraphMorphism a b) where

  {-
     PO algorithm:
     1. invert r
     2. compose k and r^-1
     3. create node table  (R -> G')
     5. create edge table  (R -> G')
     4. associate nodes
     6. associate edges
  -}

  po k r =
    let
        kr = M.compose (invertTGM r) k                                 -- invert r and compose with k, obtain kr : R -> D
        createdNodes = orphanNodesTyped r                                -- nodes in R to be created
        createdEdges = orphanEdgesTyped r                                -- edges in R to be created
        nodeTable    = zip createdNodes (GM.newNodesTyped $ M.codomain kr) -- table mapping NodeIds in R to NodeIds in G'
        edgeTable    = zip createdEdges (GM.newEdgesTyped $ M.codomain kr) -- table mapping EdgeIds in R to EdgeIds in G'

        -- generate new node instances in G', associating them to the "created" nodes in R
        kr'          = foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (M.domain kr) a
                                            in updateNodeRelationTGM a b tp tgm)
                             kr
                             nodeTable

        -- query the instance graphs R
        typemor = M.domain         kr'                     -- typemor is the typed graph (R -> T)
        g       = M.domain         typemor                 -- g  is the instance graph R
        mp      = mapping        kr'                     -- mp is the mapping of kr'  : (R -> D'), where D' = D + new nodes
        s1 e = fromJust $ G.sourceOf g e                     -- obtain source of e in R
        t1 e = fromJust $ G.targetOf g e                     -- obtain target of e in R
        s2 e = fromJust $ GM.applyNode mp (s1 e)             -- obtain source of m'(e) in G'
        t2 e = fromJust $ GM.applyNode mp (t1 e)             -- obtain target of m'(e) in G'
        tp e = fromJust $ GM.applyEdge typemor e             -- obtain type of e in R

        -- generate new edge table with new information
        edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e, t2 e, tp e)) edgeTable

        -- create new morphism adding all edges
        kr''      = foldr (\(a,sa,ta,b,sb,tb,tp) tgm -> updateEdgeRelationTGM a b (createEdgeCodTGM b sb tb tp tgm) )
                          kr'
                          edgeTable'
    in (kr'', idMap k kr'')

  {-
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  poc m l =
    let ml       = M.compose l m                                                         -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdge $ mapping m) (orphanEdgesTyped l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNode $ mapping m) (orphanNodesTyped l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeCodTyped                                          -- delete all edges, then all nodes from ml
                       (foldr removeEdgeCodTyped ml delEdges)
                           delNodes
    in (k, idMap k m)

  injectivePullback f g = (delNodesFromF', delNodesFromG')
    where
      f' = invertTGM f
      g' = invertTGM g
      nodes = nodesDomain f'
      edges = edgesDomain f'
      knodes = filter (\n -> isJust (applyNodeTGM f' n) && isJust (applyNodeTGM g' n)) nodes
      kedges = filter (\e -> isJust (applyEdgeTGM f' e) && isJust (applyEdgeTGM g' e)) edges
      delNodes = nodes \\ knodes
      delEdges = edges \\ kedges
      delEdgesFromF' = foldr removeEdgeDomTyped f' delEdges
      delNodesFromF' = foldr removeNodeDomTyped delEdgesFromF' delNodes
      delEdgesFromG' = foldr removeEdgeDomTyped g' delEdges
      delNodesFromG' = foldr removeNodeDomTyped delEdgesFromG' delNodes
