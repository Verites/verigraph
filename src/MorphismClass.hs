{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

import Data.List
import Graph (Graph)
import GraphClass

class (GraphClass (G m)) => MorphismClass m where
    type G m :: *

    -- Extract essencial contents of a morphism
    domain    :: m -> (G m)                            -- required
    codomain  :: m -> (G m)                            -- required
    mapping   :: m -> ([(Nd (G m), Nd (G m))], [(Ed (G m), Ed (G m))]) -- required

    -- Create and manipulate morphisms
    empty              :: (G m) -> (G m) -> m                     -- required
    updateNodeMapping  :: (Nd (G m)) -> (Nd (G m)) -> m -> m      -- required
    updateEdgeMapping  :: (Ed (G m)) -> (Ed (G m)) -> m -> m      -- required

    -- Query functions
    hasNodeMapping     :: (Eq (Nd (G m))) => Nd (G m) -> Nd (G m) -> m -> Bool
    hasEdgeMapping     :: (Eq (Ed (G m))) => Ed (G m) -> Ed (G m) -> m -> Bool

    -- Query functions: default implementation
    hasNodeMapping lNode rNode m =
        (lNode, rNode) `elem` nodeMappings
      where
        (nodeMappings, _) = mapping m

    hasEdgeMapping lEdge rEdge m =
        (lEdge, rEdge) `elem` edgeMappings
      where
        (_, edgeMappings) = mapping m
