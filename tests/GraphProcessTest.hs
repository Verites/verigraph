-- | Test Suite for GraphProcess Module

import           Abstract.AdhesiveHLR
import           Abstract.Cardinality
import           Abstract.Cocomplete
import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import           Data.List
import           Data.Maybe
import           Graph.Graph
import           Graph.GraphMorphism
import           TypedGraph.DPO.GraphProcess
import           TypedGraph.DPO.GraphRule
import           TypedGraph.Morphism
import           Utils


{-grafo tipo-}
grafotipo = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]

a = build [10,20,30,40] []
b = build [50,60,70,80] []

ta = buildGraphMorphism a grafotipo [(10,1),(20,1),(30,1),(40,1)] []
tb = buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []
tc = buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []
td = buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []

mf = buildGraphMorphism a b [(10,50),(20,60),(30,70),(40,80)] []
mg = buildGraphMorphism a b [(10,50),(20,60),(30,70),(40,80)] []

tmf = buildTypedGraphMorphism ta tb mf
tmg = buildTypedGraphMorphism ta tb mg

teste = calculateCoequalizer tmf tmg


main :: IO ()
main = return ()
