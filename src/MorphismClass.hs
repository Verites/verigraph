{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

import Data.List
import Graph (Graph)
import GraphClass

class (GraphClass (G m), Valid m) => MorphismClass m where
    type G m :: *

    -- Extract essential contents of a morphism
    domain    :: m -> (G m)                                -- required
    codomain  :: m -> (G m)                                -- required
    image     :: m -> (G m)
    defDomain :: m -> (G m)                                -- Definition Domain

    -- Apply morphism
    applyToNode    :: (Nd (G m)) -> [(Nd (G m))]           -- required
    applyInvToNode :: (Nd (G m)) -> [(Nd (G m))]
    applyToEdge    :: (Ed (G m)) -> [(Ed (G m))]           -- required
    applyInvToEdge :: (Ed (G m)) -> [(Ed (G m))]

    -- Create and manipulate morphisms
    empty       :: (G m) -> (G m) -> m                     -- required
    insertNodes :: (Nd (G m)) -> (Nd (G m)) -> m -> m      -- required
    insertEdges :: (Ed (G m)) -> (Ed (G m)) -> m -> m      -- required
    compose     :: m -> m -> m

    -- Properties
    total     :: m -> Bool
    injective :: m -> Bool

    
