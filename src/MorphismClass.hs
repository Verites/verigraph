{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module MorphismClass where

import Data.Maybe (fromJust)
import Helper (Valid)
import GraphClass

class GraphClass (G m) => MorphismClass m where
    type G m :: *

    -- Extract essential contents of a morphism
    domain    :: m -> (G m)                                -- required
    codomain  :: m -> (G m)                                -- required
    image     :: m -> (G m)
    defDomain :: m -> (G m)                                -- Definition Domain

    -- Apply morphism
    applyToNode    :: (Nd (G m)) -> m -> [(Nd (G m))]      -- required
    applyInvToNode :: (Nd (G m)) -> m -> [(Nd (G m))]
    applyToEdge    :: (Ed (G m)) -> m -> [(Ed (G m))]      -- required
    applyInvToEdge :: (Ed (G m)) -> m -> [(Ed (G m))]

    -- Create and manipulate morphisms
    empty     :: (G m) -> (G m) -> m                       -- required
    mapNodes  :: (Nd (G m)) -> (Nd (G m)) -> m -> m        -- required
    mapEdges  :: (Ed (G m)) -> (Ed (G m)) -> m -> m        -- required
    compose   :: m -> m -> m

    -- Properties
    total     :: m -> Bool
    injective :: m -> Bool

  ---------- Default implementations -------------

    image m =
        let dom = domain m
            cod = codomain m
            domNodes = nodes dom
            domEdges = edges dom
            imgNodes = concatMap (flip applyToNode m) domNodes
            imgEdges = concatMap (flip applyToEdge m) domEdges
        in flip (foldr (\e g -> insertEdge e
                                      (fromJust $ sourceOf e cod)
                                      (fromJust $ targetOf e cod)
                                      g))
                 imgEdges $
                 foldr insertNode (GraphClass.empty) imgNodes


    defDomain m =
        let dom = domain m
            domNodes = nodes dom
            domEdges = edges dom
            mappedNodes = filter (\n -> (not . null) $ applyToNode n m)
                                 domNodes
            mappedEdges = filter (\e -> (not . null) $ applyToEdge e m)
                                 domEdges
        in flip (foldr (\e g -> insertEdge e
                                      (fromJust $ sourceOf e dom)
                                      (fromJust $ targetOf e dom)
                                      g))
                 mappedEdges $
                 foldr insertNode (GraphClass.empty) mappedNodes

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
                    [rn]      -> mapNodes ln rn m
                    otherwise -> m
            insEdges m1 m2 le m =
                let es = applyToEdge le m1 >>= (\me -> applyToEdge me m2)
                in case es of
                    [re]      -> mapEdges le re m
                    otherwise -> m




    total m =
        ((length . nodes . domain) m == (length . nodes . defDomain) m) &&
        ((length . edges . domain) m == (length . edges . defDomain) m)

    -- checks if each element from the image is mapped to a single 
    -- element from the Domain
    injective m =
        all (\n -> n == 1) mappings  
        where                        
            img = image m
            mappings = (map (length . (flip applyInvToNode m)) (nodes img)) ++
                       (map (length . (flip applyInvToEdge m)) (edges img))


