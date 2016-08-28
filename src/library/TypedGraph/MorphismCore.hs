{-# LANGUAGE TypeFamilies #-}
module TypedGraph.MorphismCore where

import           Abstract.Morphism   as M
import           Abstract.Valid
import           Graph.Graph
import           Graph.GraphMorphism
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
    compose t1 t2 =
        TypedGraphMorphism (domain t1)
                      (codomain t2)
                      $ compose (mapping t1)
                                (mapping t2)
    id t = TypedGraphMorphism t t (M.id $ domain t)
    monomorphism = monomorphism . mapping
    epimorphism = epimorphism . mapping
    isomorphism = isomorphism . mapping

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

-- | Return the node to which @ln@ gets mapped
applyNodeTGM :: TypedGraphMorphism a b -> NodeId -> Maybe NodeId
applyNodeTGM tgm = applyNode (mapping tgm)

-- | Return the edge to which @le@ gets mapped
applyEdgeTGM :: TypedGraphMorphism a b -> EdgeId -> Maybe EdgeId
applyEdgeTGM tgm = applyEdge (mapping tgm)
