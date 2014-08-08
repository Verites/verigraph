{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

import Data.List
import Graph (Graph)
import GraphClass

class GraphClass g => MorphismClass m g where

    -- Extract essencial contents of a morphism
    domain             :: m g -> g                                -- required
    codomain           :: m g -> g                                -- required
    mapping            :: m g -> ([(Nd g, Nd g)], [(Ed g, Ed g)]) -- required

    -- Create and manipulate morphisms
    empty              :: g -> g -> m g                           -- required
    updateNodeMapping  :: (Nd g) -> (Nd g) -> m g -> m g          -- required
    updateEdgeMapping  :: (Ed g) -> (Ed g) -> m g -> m g          -- required

    -- Query functions
    hasNodeMapping     :: (Eq (Nd g)) => Nd g -> Nd g -> m g -> Bool
    hasEdgeMapping     :: (Eq (Ed g)) => Ed g -> Ed g -> m g -> Bool

    -- Query functions: default implementation
    hasNodeMapping lNode rNode m =
        (lNode, rNode) `elem` nodeMappings
      where
        (nodeMappings, _) = mapping m

    hasEdgeMapping lEdge rEdge m =
        (lEdge, rEdge) `elem` edgeMappings
      where
        (_, edgeMappings) = mapping m
