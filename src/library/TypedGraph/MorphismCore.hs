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

-- | Given two @TypedGraph@s @G1@ and @G2@ and a simple @GraphMorphism@ between them, it returns a @TypedGraphMorphism@ from @G1@ to @G2@
buildTypedGraphMorphism :: TypedGraph a b -> TypedGraph a b -> GraphMorphism a b -> TypedGraphMorphism a b
buildTypedGraphMorphism = TypedGraphMorphism

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
    validate (TypedGraphMorphism dom cod m) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , ensure (dom == compose m cod) "Morphism doesn't preserve typing"
        ]

-- | Return the nodes in the domain of a given @TypedGraphMorphism@
nodesFromDomain :: TypedGraphMorphism a b -> [NodeId]
nodesFromDomain = nodes . domain . getDomain

-- | Return the edges in the domain of a given @TypedGraphMorphism@
edgesFromDomain :: TypedGraphMorphism a b -> [EdgeId]
edgesFromDomain = edges . domain . getDomain

-- | Return the nodes in the codomain of a given @TypedGraphMorphism@
nodesFromCodomain :: TypedGraphMorphism a b -> [NodeId]
nodesFromCodomain = nodes . domain . getCodomain

-- | Return the edges in the codomain of a given @TypedGraphMorphism@
edgesFromCodomain :: TypedGraphMorphism a b -> [EdgeId]
edgesFromCodomain = edges . domain . getCodomain

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and a node @__n__@ in @G1@, it returns the node in @G2@ to which @__n__@ gets mapped
applyNode :: TypedGraphMorphism a b -> NodeId -> Maybe NodeId
applyNode tgm = GM.applyNode (mapping tgm)

-- | Given a TypedGraphMorphism @/__t__: G1 -> G2/@ and an edge @__e__@ in @G1@, it returns the edge in @G2@ to which @__e__@ gets mapped
applyEdge :: TypedGraphMorphism a b -> EdgeId -> Maybe EdgeId
applyEdge tgm = GM.applyEdge (mapping tgm)
