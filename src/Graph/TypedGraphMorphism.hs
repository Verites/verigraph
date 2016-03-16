{-# LANGUAGE TypeFamilies #-}

module Graph.TypedGraphMorphism (
      TypedGraphMorphism
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

import Graph.Graph (Graph,nodes,edges)
import Graph.Graph as G
import Graph.GraphMorphism as GM
import Abstract.Morphism as M
import Abstract.Valid

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
applyNodeTGM tgm ln = GM.applyNode (mapping tgm) ln

-- | Return the edge to which @le@ gets mapped.
applyEdgeTGM :: TypedGraphMorphism a b -> G.EdgeId -> Maybe G.EdgeId
applyEdgeTGM tgm le = GM.applyEdge (mapping tgm) le

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

-- | Test if a @nac@ is partial injective (injective out of @q@)
partialInjectiveTGM :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
partialInjectiveTGM nac q = GM.partialInjectiveGM (getMapping nac) (getMapping q)

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
        
