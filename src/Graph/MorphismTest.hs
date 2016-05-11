import           Graph.FindMorphism
import           Graph.Graph
import qualified Graph.GraphMorphism as Gm
import qualified Graph.TypedGraphMorphism as TGM


{-grafo tipo-}


-- 1 = bolinha
-- 2 = quadrado

--3 = loop na bolinha
--4 = loop no quadrado

--5 = bolinha quadrado
--6 = quadrado bolinha
grafotipo = build [1,2] [(3,1,1),(4,2,2),(5,1,2),(6,2,1)]

--Grafo normal
g1' = build [1,2] [(3,1,2),(4,1,2)]
g2' = build [1,2] [(3,1,2)]

g3' = build [1,2,3,4] [(5,1,2),(6,3,4)]
g4' = build [1,2,3] [(4,1,3),(5,2,3)]

g5' = build [1,2,3] [(4,1,2),(5,1,3)]
g6' = g3'

g7' = g5'
g8' = g8'

g9' = build [1,2,3,4] [(5,1,2),(6,3,4),(7,3,2),(8,1,4)]
g10' = g4'

g11' = g2'
g12' = build [1] [(2,1,1)]

g13' = build [1,2] [(3,1,2)]
g14' = g4'

--tipagem


--grafotipado = GM.gmbuild lr1 grafotipo [(14,4),(13,3),(11,1)] [(11,1)]
