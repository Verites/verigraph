{-# LANGUAGE TypeFamilies #-}

module Morphism
    ( Morphism
    , TypedGraph
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import qualified Data.MultiMap as MM
import Helper (Valid)
import MorphismClass
import Graph (Graph)
import qualified GraphClass as G

data Morphism a b = Morphism {
                          getDomain     :: Graph a b
                        , getCodomain   :: Graph a b
                        , getMapping    :: (IntMap Int, IntMap Int)
                        , getInvMapping :: (MM.MultiMap Int Int, MM.MultiMap Int Int)
                    }

type TypedGraph a b = Morphism a b

instance Show (Morphism a b) where
    show m =
--        "Domain: " ++ (show $ getDomain m) ++
--        "\nCodomain: " ++ (show $ getCodomain m) ++
        "\nNode mappings: \n" ++
        concatMap (\n -> (show n) ++ " --> " ++ (show (applyToNode n m)) ++ "\n")
                  (G.nodes $ domain m) ++
        "\nEdge mappings: \n" ++
        concatMap (\e -> (show e) ++ " --> " ++ (show (applyToEdge e m)) ++ "\n")
                  (G.edges $ domain m)

instance MorphismClass (Morphism a b) where
    type G (Morphism a b) = Graph a b

    domain m   = getDomain m
    codomain m = getCodomain m

    applyToNode ln m =
        let found = Map.lookup ln $ (fst . getMapping) m
        in case found of
            Just gn   -> [gn]
            otherwise -> []
    applyInvToNode gn m =
        MM.lookup gn $ (fst . getInvMapping) m

    applyToEdge le m =
        let found = Map.lookup le $ (snd . getMapping) m
        in case found of
            Just ge   -> [ge]
            otherwise -> []
    applyInvToEdge ge m =
        MM.lookup ge $ (snd . getInvMapping) m
        
    empty gA gB = Morphism gA gB (Map.empty, Map.empty) (MM.empty, MM.empty)
    mapNodes ln gn morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isNodeOf ln l && G.isNodeOf gn g =
            Morphism l g (Map.insert ln gn nm, em)
                         (MM.insert  gn ln nInv, eInv) 
        | otherwise = morphism

    mapEdges le ge morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isEdgeOf le l && G.isEdgeOf ge g =
            Morphism l g (nm, Map.insert le ge em)
                         (nInv, MM.insert ge le eInv)
        | otherwise = morphism

