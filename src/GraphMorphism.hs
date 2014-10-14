{-# LANGUAGE TypeFamilies #-}

module Morphism
    ( Morphism
    , TypedGraph
    ) where

import Data.IntMap (IntMap)
--import qualified Data.IntMap as Map
import qualified Data.MultiMap as Map
import Valid
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
        concatMap (\n -> (show n) ++ " --> " ++ (show (applyNode m n)) ++ "\n")
                  (G.nodes $ domain m) ++
        "\nEdge mappings: \n" ++
        concatMap (\e -> (show e) ++ " --> " ++ (show (applyEdge m e)) ++ "\n")
                  (G.edges $ domain m)

instance MorphismClass (Morphism a b) where
    type G (Morphism a b) = Graph a b

    domain m   = getDomain m
    codomain m = getCodomain m

    inverse (Morphism dom cod mapping invmapping) =
        Morphism cod dom invmapping mapping

    applyNode m ln =
        Map.lookup ln $ (fst . getMapping) m

    applyEdge m le =
        Map.lookup le $ (snd . getMapping) m
        
    empty gA gB = Morphism gA gB (Map.empty, Map.empty) (Map.empty, Map.empty)
    updateNodes ln gn morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isNodeOf l ln && G.isNodeOf g gn =
            Morphism l g (Map.insert ln gn nm, em)
                         (Map.insert  gn ln nInv, eInv) 
        | otherwise = morphism

    updateEdges le ge morphism@(Morphism l g (nm, em) (nInv, eInv))
        | G.isEdgeOf l le && G.isEdgeOf g ge =
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
        all (\e -> (G.sourceOf cod e >>= applyEdge m) ==
                   (applyNode m e >>= G.sourceOf dom)
                   &&
                   (G.targetOf cod e >>= applyEdge m) ==
                   (applyEdge m e >>= G.targetOf dom))
            (G.edges dom)

