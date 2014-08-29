{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

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
    applyNode :: m -> (Nd (G m)) -> [(Nd (G m))]     -- required
    applyEdge :: m -> (Ed (G m)) -> [(Ed (G m))]     -- required

    -- Create and manipulate morphisms
    empty       :: (G m) -> (G m) -> m                     -- required
    updateNodes :: (Nd (G m)) -> (Nd (G m)) -> m -> m      -- required
    updateEdges :: (Ed (G m)) -> (Ed (G m)) -> m -> m      -- required
    compose     :: m -> m -> m

    -- Properties
    functional :: m -> Bool
    injective  :: m -> Bool
    surjective :: m -> Bool
    total      :: m -> Bool
  ---------- Default implementations -------------

    image m =
        let cod  = codomain m
            invm = inverse m
            nodesNotMapped = filter (\n -> null $ applyNode invm n) $ nodes cod
            edgesNotMapped = filter (\e -> null $ applyEdge invm e) $ edges cod
        in flip (foldr removeNode) nodesNotMapped $
           foldr removeEdge cod edgesNotMapped

    defDomain m =
        image $ inverse m

    compose m1 m2 =
        let dom = domain m1
            cod = codomain m2
        in 
            flip
                (foldr (\e m -> doubleApplyToEdge m1 m2 m e))
                (edges dom) $
                foldr (\n m -> doubleApplyToNode m1 m2 m n)
                      (MorphismClass.empty dom cod)
                      (nodes dom)
        where
            doubleApplyToNode m1 m2 m ln =
                foldr (updateNodes ln)
                      m
                      (applyNode m1 ln >>= applyNode m2)
            doubleApplyToEdge m1 m2 m le =
                foldr (updateEdges le)
                      m
                      (applyEdge m1 le >>= applyEdge m2)

    functional m =
        injective $ inverse m

    -- checks if each element from the image is mapped to a single 
    -- element from the Domain
    injective m =
        all (\n -> n == 1) mappings  
        where                        
            invm     = inverse m
            dom      = defDomain invm
            mappings = (map (length . (applyNode invm)) (nodes dom)) ++
                       (map (length . (applyEdge invm)) (edges dom))

    surjective m =
        total $ inverse m

    total m =
        ((length . nodes . domain) m == (length . nodes . defDomain) m) &&
        ((length . edges . domain) m == (length . edges . defDomain) m)

