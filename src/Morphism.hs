{-# LANGUAGE TypeFamilies #-}

module Morphism (Morphism) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import MorphismClass
import Graph (Graph)
import qualified GraphClass as G

data Morphism a b = Morphism {
                          getDomain   :: Graph a b
                        , getCodomain :: Graph a b
                        , getMapping  :: (IntMap Int, IntMap Int)
                    }

instance MorphismClass (Morphism a b) where
    type G (Morphism a b) = Graph a b

    domain m   = getDomain m
    codomain m = getCodomain m
    mapping m = (Map.toList nm, Map.toList em)
        where (nm, em) = getMapping m

    empty gA gB = Morphism gA gB (Map.empty, Map.empty)
    updateNodeMapping ln rn morphism@(Morphism lg rg (nm, em))
        | G.isNodeOf ln lg && G.isNodeOf rn rg =
            Morphism lg rg (Map.insert ln rn nm, em)
        | otherwise = morphism

    updateEdgeMapping le re morphism@(Morphism lg rg (nm, em))
        | G.isEdgeOf le lg && G.isEdgeOf re rg =
            Morphism lg rg (nm, Map.insert le re em)
        | otherwise = morphism
