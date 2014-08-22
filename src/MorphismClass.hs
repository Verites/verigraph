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
            nodesNotMapped = filter (\n -> null $ applyToNode n invm) cod
            edgesNotMapped = filter (\n -> null $ applyToNode n invm) cod
        in flip (foldr removeNode) nodesNotMapped $
           foldr removeEdge cod edgesNotMapped

    defDomain m =
        image $ inverse m
        
    compose m1 m2 =
        let dom = domain m1
            cod = codomain m2 
            domNodes = nodes dom
            domEdges = edges dom
        in  flip (foldr (\e m -> insEdges m1 m2 e m))
                 domEdges $
                 foldr (insNodes m1 m2) (MorphismClass.empty dom cod) domNodes
        where
            insNodes m1 m2 ln m =
                let ns = applyToNode ln m1 >>= (\mn -> applyToNode mn m2)
                in case ns of
                    [rn]      -> updateNodes ln rn m
                    otherwise -> m
            insEdges m1 m2 le m =
                let es = applyToEdge le m1 >>= (\me -> applyToEdge me m2)
                in case es of
                    [re]      -> updateEdges le re m
                    otherwise -> m

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
