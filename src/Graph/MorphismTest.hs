import           Abstract.Morphism
import           Graph.FindMorphism()
import           Graph.Graph
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
-- import           Test.HUnit

-- 1 = bolinha
-- 2 = quadrado
-- 3 = bolinha bolinha
-- 4 = quadrado quadrado
-- 5 = bolinha quadrado
-- 6 = quadrado bolinha

typegraph :: Graph a b
typegraph = build [1,2] [(3,1,1),(4,2,2),(5,1,2),(6,2,1)]

g1 :: GM.GraphMorphism a b
g1 = GM.gmbuild g1' typegraph [(1,1),(2,1)] [(3,3),(4,3)]
  where
    g1' = build [1,2] [(3,1,2),(4,1,2)]

g2 :: GM.GraphMorphism a b
g2 = GM.gmbuild g2' typegraph [(1,1),(2,1)] [(3,3)]
  where
    g2' = build [1,2] [(3,1,2)]

g3 :: GM.GraphMorphism a b
g3 = GM.gmbuild g3' typegraph [(1,1),(2,2),(3,1),(4,2)] [(5,5), (6,5)]
  where
    g3' = build [1,2,3,4] [(5,1,2),(6,3,4)]

g4 :: GM.GraphMorphism a b
g4 = GM.gmbuild g4' typegraph [(1,1),(2,1),(3,2)] [(4,5),(5,5)]
  where
    g4' = build [1,2,3] [(4,1,3),(5,2,3)]

g5 :: GM.GraphMorphism a b
g5 = GM.gmbuild g5' typegraph [(1,1),(2,2),(2,3)] [(4,5),(5,5)]
  where
    g5' = build [1,2,3] [(4,1,2),(5,1,3)]

g9 :: GM.GraphMorphism a b
g9 = GM.gmbuild g9' typegraph [(1,1),(2,2),(3,1),(4,2)] [(5,5),(6,5),(7,5),(8,5)]
  where
    g9' = build [1,2,3,4] [(5,1,2),(6,3,4),(7,3,2),(8,1,4)]

g12 :: GM.GraphMorphism a b
g12 = GM.gmbuild g12' typegraph [(1,1)] [(2,3)]
  where
    g12' = build [1] [(2,1,1)]

g13 :: GM.GraphMorphism a b
g13 = GM.gmbuild g13' typegraph [(1,1),(2,2)] [(3,5)]
  where
    g13' = build [1,2] [(3,1,2)]



genericLenTest :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> (Int,Int,Int,Int)
genericLenTest src tgt = (length $ mono src tgt,
                          length $ surj src tgt,
                          length $ isom src tgt,
                          length $ allm src tgt)
  where
    mono :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> [TGM.TypedGraphMorphism a b ]
    mono src tgt = matches MONO src tgt

    surj :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> [TGM.TypedGraphMorphism a b ]
    surj src tgt = matches EPI src tgt
    
    isom :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> [TGM.TypedGraphMorphism a b ]
    isom src tgt = matches ISO src tgt
    
    allm :: GM.GraphMorphism a b -> GM.GraphMorphism a b -> [TGM.TypedGraphMorphism a b ]
    allm src tgt = matches ALL src tgt


test1  = genericLenTest g1 g2
test1' = genericLenTest g2 g1

test2  = genericLenTest g3 g4
test2' = genericLenTest g4 g3

test3  = genericLenTest g5 g3
test3' = genericLenTest g3 g5

test4  = genericLenTest g5 g5

test5  = genericLenTest g9 g4
test5' = genericLenTest g4 g9

test6  = genericLenTest g2 g12
test6' = genericLenTest g12 g2

test7  = genericLenTest g13 g4
test7' = genericLenTest g4 g13
