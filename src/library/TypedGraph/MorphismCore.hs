{-# LANGUAGE TypeFamilies #-}
module TypedGraph.MorphismCore where

import           Abstract.Morphism   as M
import           Abstract.Valid
import           Graph.Graph
import           Graph.GraphMorphism as GM
import           TypedGraph.Graph

data TypedGraphMorphism a b = TypedGraphMorphism {
                              getDomain   :: TypedGraph a b
                            , getCodomain :: TypedGraph a b
                            , mapping     :: GraphMorphism a b
                         } deriving (Eq, Show, Read)

typedMorphism :: TypedGraph a b -> TypedGraph a b -> GraphMorphism a b -> TypedGraphMorphism a b
typedMorphism = TypedGraphMorphism

instance Morphism (TypedGraphMorphism a b) where
    type Obj (TypedGraphMorphism a b) = TypedGraph a b

    domain = getDomain
    codomain = getCodomain
    compose t1 t2 = TypedGraphMorphism (domain t1) (codomain t2) $ compose (mapping t1) (mapping t2)
    id t = TypedGraphMorphism t t (M.id $ domain t)
    isMonomorphism = isMonomorphism . mapping
    isEpimorphism = isEpimorphism . mapping
    isIsomorphism = isIsomorphism . mapping

instance Valid (TypedGraphMorphism a b) where
    valid (TypedGraphMorphism dom cod m) =
        valid dom &&
        valid cod &&
        dom == compose m cod

-- | Return the nodes in the domain of this TGM
nodesDomain :: TypedGraphMorphism a b -> [NodeId]
nodesDomain = nodes . domain . getDomain

-- | Return the edges in the domain of this TGM
edgesDomain :: TypedGraphMorphism a b -> [EdgeId]
edgesDomain = edges . domain . getDomain

-- | Return the nodes in the codomain of this TGM
nodesCodomain :: TypedGraphMorphism a b -> [NodeId]
nodesCodomain = nodes . domain . getCodomain

-- | Return the edges in the codomain of this TGM
edgesCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgesCodomain = edges . domain . getCodomain

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and a node @__n__@ in @G1@, it returns the node in @G2@ to which @__n__@ gets mapped
applyNode :: TypedGraphMorphism a b -> NodeId -> Maybe NodeId
applyNode tgm = GM.applyNode (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and an edge @__e__@ in @G1@, it returns the edge in @G2@ to which @__e__@ gets mapped
applyEdge :: TypedGraphMorphism a b -> EdgeId -> Maybe EdgeId
applyEdge tgm = GM.applyEdge (mapping tgm)
