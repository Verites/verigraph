{-# LANGUAGE TypeFamilies #-}

module Morphism
    ( Morphism
    , TypedGraph
    ) where

import Data.IntMap (IntMap)
--import qualified Data.IntMap as Map
import qualified Data.MultiMap as Map
import Helper
import MorphismClass
import Graph (Graph)
import qualified GraphClass as G

data Morphism a b = Morphism {
                          getDomain     :: Graph a b
                        , getCodomain   :: Graph a b
                        , getMapping    :: (Map.MultiMap Int Int, Map.MultiMap Int Int)
                        , getInvMapping :: (Map.MultiMap Int Int, Map.MultiMap Int Int)
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

    inverse (Morphism dom cod mapping invmapping) =
        Morphism cod dom invmapping mapping

    applyToNode ln m =
        Map.lookup ln $ (fst . getMapping) m

    applyToEdge le m =
        Map.lookup le $ (snd . getMapping) m
        
    empty gA gB = Morphism gA gB (Map.empty, Map.empty) (Map.empty, Map.empty)
    updateNodes ln gn morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isNodeOf ln l && G.isNodeOf gn g =
            Morphism l g (Map.insert ln gn nm, em)
                         (Map.insert  gn ln nInv, eInv) 
        | otherwise = morphism

    updateEdges le ge morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isEdgeOf le l && G.isEdgeOf ge g =
            Morphism l g (nm, Map.insert le ge em)
                         (nInv, Map.insert ge le eInv)
        | otherwise = morphism

instance Valid (Morphism a b) where
    valid m =
        let dom = domain m
            cod = codomain m
        in
        total m &&
        functional m &&
        valid dom &&
        valid cod &&
        all (\e -> ((flip G.sourceOf cod) $ head $ applyToEdge e m) ==
                    (fmap (head . (flip applyToNode m)) $ G.sourceOf e dom)
                   &&
                   ((flip G.targetOf cod) $ head $ applyToEdge e m) ==
                    (fmap (head . (flip applyToEdge m)) $ G.targetOf e dom))
            (G.edges dom)
