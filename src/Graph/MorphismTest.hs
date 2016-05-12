
--import           Graph.FindMorphism
import           Graph.Graph
import qualified Graph.GraphMorphism as GM
--import qualified Graph.TypedGraphMorphism as TGM
import           Test.HUnit

{-grafo tipo-}


-- 1 = bolinha
-- 2 = quadrado
-- 3 = bolinha bolinha
-- 4 = quadrado quadrado
-- 5 = bolinha quadrado
-- 6 = quadrado bolinha

grafotipo :: Graph a b
grafotipo = build [1,2] [(3,1,1),(4,2,2),(5,1,2),(6,2,1)]

--Grafo normal

g1 :: GM.GraphMorphism a b
g1 = GM.gmbuild g1' grafotipo [(1,1),(2,1)] [(3,3),(4,3)]
  where
    g1' = build [1,2] [(3,1,2),(4,1,2)]

g2 :: GM.GraphMorphism a b
g2 = GM.gmbuild g2' grafotipo [(1,1),(2,1)] [(3,3)]
  where
    g2' = build [1,2] [(3,1,2)]

g3 :: GM.GraphMorphism a b
g3 = GM.gmbuild g3' grafotipo [(1,1),(2,2),(3,1),(4,2)] [(5,5), (6,5)]
  where
    g3' = build [1,2,3,4] [(5,1,2),(6,3,4)]

g4 :: GM.GraphMorphism a b
g4 = GM.gmbuild g4' grafotipo [(1,1),(2,1),(3,2)] [(4,5),(5,5)]
  where
    g4' = build [1,2,3] [(4,1,3),(5,2,3)]

g5 :: GM.GraphMorphism a b
g5 = GM.gmbuild g5' grafotipo [(1,1),(2,2),(2,3)] [(4,5),(5,5)]
  where
    g5' = build [1,2,3] [(4,1,2),(5,1,3)]

g6 :: GM.GraphMorphism a b
g6 = g3

g7 :: GM.GraphMorphism a b
g7 = g5

g8 :: GM.GraphMorphism a b
g8 = g8



g9' = build [1,2,3,4] [(5,1,2),(6,3,4),(7,3,2),(8,1,4)]
--g10' = g4'

--g11' = g2'
g12' :: Graph a b
g12' = build [1] [(2,1,1)]

g13' :: Graph a b
g13' = build [1,2] [(3,1,2)]
--g14' = g4'

--tipagem


--grafotipado = GM.gmbuild lr1 grafotipo [(14,4),(13,3),(11,1)] [(11,1)]

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
  where
    foo :: Integer -> (Integer, Integer)
    foo x = (1,x)
    
tests :: Test
tests = TestList [TestLabel "test1" test1]
