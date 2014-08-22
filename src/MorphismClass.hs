{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

import Helper (Valid)
import GraphClass

class GraphClass (G m) => MorphismClass m where
    type G m :: *

    -- Extract essential contents of a morphism
    domain    :: m -> (G m)                                -- required
    codomain  :: m -> (G m)                                -- required
    image     :: m -> (G m)
    defDomain :: m -> (G m)                                -- Definition Domain

    inverse   :: m -> m                                    -- required

    -- Apply morphism
    applyToNode     :: (Nd (G m)) -> m -> [(Nd (G m))]     -- required
    applyToEdge     :: (Ed (G m)) -> m -> [(Ed (G m))]     -- required

    -- Create and manipulate morphisms
    empty       :: (G m) -> (G m) -> m                     -- required
    updateNodes :: (Nd (G m)) -> (Nd (G m)) -> m -> m      -- required
    updateEdges :: (Ed (G m)) -> (Ed (G m)) -> m -> m      -- required
    compose     :: m -> m -> m

    -- Properties
    total      :: m -> Bool
    injective  :: m -> Bool
    surjective :: m -> Bool
  ---------- Default implementations -------------

    image m =
        let cod  = codomain m
            invm = inverse m
            nodesNotMapped = filter (\n -> null $ applyToNode n invm) $ nodes cod
            edgesNotMapped = filter (\e -> null $ applyToEdge e invm) $ edges cod
        in flip (foldr removeNode) nodesNotMapped $
           foldr removeEdge cod edgesNotMapped

    defDomain m =
        image $ inverse m

    compose m1 m2 =
        let dom = domain m1
            cod = codomain m2
        in 
            flip
                (foldr (\e m -> doubleApplyToEdge e m1 m2 m))
                (edges dom) $
                foldr (\n m -> doubleApplyToNode n m1 m2 m)
                      (MorphismClass.empty dom cod)
                      (nodes dom)
        where
            doubleApplyToNode ln m1 m2 m =
                foldr (\rn acc -> updateNodes ln rn acc)
                      m
                      (applyToNode ln m1 >>= (flip applyToNode m2))
            doubleApplyToEdge le m1 m2 m =
                foldr (\re acc -> updateEdges le re acc)
                      m
                      (applyToEdge le m1 >>= flip applyToEdge m2)

    total m =
        ((length . nodes . domain) m == (length . nodes . defDomain) m) &&
        ((length . edges . domain) m == (length . edges . defDomain) m)

    -- checks if each element from the image is mapped to a single 
    -- element from the Domain
    injective m =
        all (\n -> n == 1) mappings  
        where                        
            invm     = inverse m
            dom      = defDomain invm
            mappings = (map (length . (flip applyToNode invm)) (nodes dom)) ++
                       (map (length . (flip applyToEdge invm)) (edges dom))

    surjective m =
        total $ inverse m
