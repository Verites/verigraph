--{-# LANGUAGE TypeFamilies #-}

--import Analysis.ConcurrentRules
import           Abstract.Valid
import qualified Data.List.NonEmpty             as NE
import qualified XML.GGXReader                  as XML
--import qualified Analysis.CriticalSequence as CS
--import qualified Analysis.CriticalPairs as CP
--import           Partitions.GraphPartitionToVerigraph
--import           Partitions.GraphPartition
--import           Partitions.VerigraphToGraphPartition
import           Graph.Graph                    as G
--import qualified TypedGraph.GraphRule as GR
import qualified Graph.GraphMorphism            as GM
import           TypedGraph.Morphism            as TGM
--import qualified TypedGraph.GraphGrammar as GG
--import qualified XML.GGXWriter as GW
import           TypedGraph.GraphRule
--import System.Process
--import System.Environment
--import System.Exit
import           Abstract.Morphism
import           TypedGraph.Morphism.Cocomplete as C



import           Abstract.AdhesiveHLR           as RW
import           Abstract.DPO                   as RW
--import Data.Matrix
import qualified Data.List                      as L
import           Abstract.Cardinality
import           Data.Maybe

--import qualified XML.GGXReader as XML

main =
  return ()

iN = insertNode
iE = insertEdge
uN = GM.updateNodes
uE = GM.updateEdges

{-grafo tipo-}
grafotipo = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]

a = build [10,20,30,40] []
b = build [50,60,70,80] []

ta = GM.buildGraphMorphism a grafotipo [(10,1),(20,1),(30,1),(40,1)] []
tb = GM.buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []
tc = GM.buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []
td = GM.buildGraphMorphism b grafotipo [(50,1),(60,1),(70,1),(80,1)] []

mf = GM.buildGraphMorphism a b [(10,50),(20,60),(30,70),(40,80)] []
mg = GM.buildGraphMorphism a b [(10,50),(20,60),(30,70),(40,80)] []

tmf = buildTypedGraphMorphism ta tb mf
tmg = buildTypedGraphMorphism ta tb mg

teste = calculateCoequalizer tmf tmg

grafotipo2 = build [1] [(1,1,1)]

a2 = build [10,20] [(100,20,10)]
b2 = build [50,60,70] [(200,60,50),(300,60,50)]

ta2 = GM.buildGraphMorphism a2 grafotipo2 [(10,1),(20,1)] [(100,1)]
tb2 = GM.buildGraphMorphism b2 grafotipo2 [(50,1),(60,1),(70,1)] [(200,1),(300,1)]

mf2 = GM.buildGraphMorphism a2 b2 [(10,50),(20,60)] [(100,200)]
mg2 = GM.buildGraphMorphism a2 b2 [(10,50),(20,60)] [(100,300)]

tmf2 = buildTypedGraphMorphism ta2 tb2 mf2
tmg2 = buildTypedGraphMorphism ta2 tb2 mg2

teste2 = calculateCoequalizer tmf2 tmg2

grafotipo3 = build [1] [(1,1,1)]

a3 = build [10,20,30] []
b3 = build [50,60,70,80] [(200,60,50),(300,70,60),(400,80,70)]
c3 = build [50,60,70,80] [(200,60,50),(300,70,60),(400,80,70)]

ta3 = GM.buildGraphMorphism a3 grafotipo3 [(10,1),(20,1),(30,1)] []
tb3 = GM.buildGraphMorphism b3 grafotipo3 [(50,1),(60,1),(70,1),(80,1)] [(200,1),(300,1),(400,1)]
tc3 = GM.buildGraphMorphism c3 grafotipo3 [(50,1),(60,1),(70,1),(80,1)] [(200,1),(300,1),(400,1)]

mf3 = GM.buildGraphMorphism a3 b3 [(10,50),(20,60),(30,70)] []
mg3 = GM.buildGraphMorphism a3 b3 [(10,80),(20,50),(30,60)] []
mh3 = GM.buildGraphMorphism a3 c3 [(10,60),(20,70),(30,80)] []

tmf3 = buildTypedGraphMorphism ta3 tb3 mf3
tmg3 = buildTypedGraphMorphism ta3 tb3 mg3
tmh3 = buildTypedGraphMorphism ta3 tc3 mh3

teste3' = C.calculatePushout tmf3 tmh3

teste3 = calculateCoequalizer tmf3 tmg3

grafotipo4 = build [1] [(1,1,1)]

a4 = build [10,20] [(100,20,10),(200,20,10),(300,20,10),(400,20,10)]
b4 = build [50,60] [(500,60,50),(600,60,50)]

--newLabels = relablingFunctions [ta4,tb4] (0,0) []

ta4 = GM.buildGraphMorphism a4 grafotipo4 [(10,1),(20,1)] [(100,1),(200,1),(300,1),(400,1)]
tb4 = GM.buildGraphMorphism b4 grafotipo4 [(50,1),(60,1)] [(500,1),(600,1)]

mf4 = GM.buildGraphMorphism a4 b4 [(10,50),(20,60)] [(100,500),(200,500),(300,600),(400,500)]
mg4 = GM.buildGraphMorphism a4 b4 [(10,50),(20,60)] [(100,600),(200,500),(300,600),(400,500)]

tmf4 = buildTypedGraphMorphism ta4 tb4 mf4
tmg4 = buildTypedGraphMorphism ta4 tb4 mg4

teste4 = calculateCoequalizer tmf4 tmg4

grafotipo5 = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]

a5 = build [10,20,30,40] []
b5 = build [50,60,70,80] []

ta5 = GM.buildGraphMorphism a5 grafotipo5 [(10,1),(20,1),(30,1),(40,1)] []
tb5 = GM.buildGraphMorphism b5 grafotipo5 [(50,1),(60,1),(70,1),(80,1)] []

mf5 = GM.buildGraphMorphism a5 b5 [(10,60),(20,50),(30,70),(40,80)] []
mg5 = GM.buildGraphMorphism a5 b5 [(10,50),(20,60),(30,70),(40,80)] []
mh5 = GM.buildGraphMorphism a5 b5 [(10,50),(20,80),(30,60),(40,70)] []

tmf5 = buildTypedGraphMorphism ta5 tb5 mf5
tmg5 = buildTypedGraphMorphism ta5 tb5 mg5
tmh5 = buildTypedGraphMorphism ta5 tb5 mh5

teste5 = calculateNCoequalizer $ NE.fromList [tmf5,tmg5,tmh5]
teste6 = calculateNCoequalizer $ NE.fromList [tmf,tmg]
teste7 = calculateNCoequalizer $ NE.fromList [tmf2,tmg2]

grafotipo6 = build [1] [(1,1,1)]

a6 = build [10,20] [(100,20,10)]
b6 = build [50,60,70,80] [(500,60,50),(600,70,60),(700,80,70)]

ta6 = GM.buildGraphMorphism a6 grafotipo6 [(10,1),(20,1)] [(100,1)]
tb6 = GM.buildGraphMorphism b6 grafotipo6 [(50,1),(60,1),(70,1),(80,1)] [(500,1),(600,1),(700,1)]

mf6 = GM.buildGraphMorphism a6 b6 [(10,50),(20,60)] [(100,500)]
mg6 = GM.buildGraphMorphism a6 b6 [(10,60),(20,70)] [(100,600)]
mh6 = GM.buildGraphMorphism a6 b6 [(10,50),(20,60)] [(100,500)]

tmf6 = buildTypedGraphMorphism ta6 tb6 mf6
tmg6 = buildTypedGraphMorphism ta6 tb6 mg6
tmh6 = buildTypedGraphMorphism ta6 tb6 mh6

teste60 = calculateNCoequalizer $ NE.fromList [tmf6]

{--
{-sendMSG-}
lr1 = build [11,13,14] [(11,13,11)]
kr1 = build [21,23,24] []
rr1 = build [31,33,34] [(35,33,34)]



--tipagem
tlr1 = GM.buildGraphMorphism lr1 grafotipo [(14,4),(13,3),(11,1)] [(11,1)]
tkr1 = GM.buildGraphMorphism kr1 grafotipo [(24,4),(23,3),(21,1)] []
trr1 = GM.buildGraphMorphism rr1 grafotipo [(34,4),(33,3),(31,1)] [(35,5)]

--span
kr1_lr1 = GM.buildGraphMorphism kr1 lr1 [(24,14),(23,13),(21,11)] []
l1 = buildTypedGraphMorphism tkr1 tlr1 kr1_lr1

kr1_rr1 = GM.buildGraphMorphism kr1 rr1 [(24,34),(23,33),(21,31)] []
r1 = buildTypedGraphMorphism tkr1 trr1 kr1_rr1

--nac
nacGraph = build [501,502,503,504] [(501,503,501),(503,502,503)]
nacType = GM.buildGraphMorphism nacGraph grafotipo [(501,1),(502,2),(503,3),(504,4)] [(501,1),(503,3)]
nacMap = GM.buildGraphMorphism lr1 nacGraph [(11,501),(13,503),(14,504)] [(11,501)]
nacSendMsg = buildTypedGraphMorphism tlr1 nacType nacMap

--sendMsg = graphRule l1 r1 [nacSendMsg]

{-getDATA-}
lr2 = build [42,43,44] [(44,42,44),(45,43,44)]
kr2 = build [52,53,54] [(55,53,54)]
rr2 = build [62,63,64] [(65,63,64),(63,62,63)]

--tipagem
tlr2 = GM.buildGraphMorphism lr2 grafotipo [(44,4),(43,3),(42,2)] [(44,4),(45,5)]
tkr2 = GM.buildGraphMorphism kr2 grafotipo [(54,4),(53,3),(52,2)] [(55,5)]
trr2 = GM.buildGraphMorphism rr2 grafotipo [(64,4),(63,3),(62,2)] [(65,5),(63,3)]

--span
kr2_lr2 = GM.buildGraphMorphism kr2 lr2 [(52,42),(53,43),(54,44)] [(55,45)]
l2 = buildTypedGraphMorphism tkr2 tlr2 kr2_lr2

kr2_rr2 = GM.buildGraphMorphism kr2 rr2 [(54,64),(53,63),(52,62)] [(55,65)]
r2 = buildTypedGraphMorphism tkr2 trr2 kr2_rr2

--getDATA = graphRule l2 r2 []

{-receiveMSG-}
lr3 = build [71,72,73,74] [(75,73,74),(73,72,73)]
kr3 = build [84,83,82,81] [(83,82,83)]
rr3 = build [94,93,92,91] [(91,93,91),(93,92,93)]

--tipagem
tlr3 = GM.buildGraphMorphism lr3 grafotipo [(74,4),(73,3),(72,2),(71,1)] [(75,5),(73,3)]
tkr3 = GM.buildGraphMorphism kr3 grafotipo [(84,4),(83,3),(82,2),(81,1)] [(83,3)]
trr3 = GM.buildGraphMorphism rr3 grafotipo [(94,4),(93,3),(92,2),(91,1)] [(91,1),(93,3)]

--span
kr3_lr3 = GM.buildGraphMorphism kr3 lr3 [(84,74),(83,73),(82,72),(81,71)] [(83,73)]
l3 = buildTypedGraphMorphism tkr3 tlr3 kr3_lr3

kr3_rr3 = GM.buildGraphMorphism kr3 rr3 [(84,94),(83,93),(82,92),(81,91)] [(83,93)]
r3 = buildTypedGraphMorphism tkr3 trr3 kr3_rr3

--receiveMSG = graphRule l3 r3 []

{-deleteMSG-}
lr4 = build [101,102,103] [(101,103,101),(103,102,103)]
kr4 = build [112,111] []
rr4 = build [121,122] [(122,122,121)]

--tipagem
tlr4 = GM.buildGraphMorphism lr4 grafotipo [(101,1),(102,2),(103,3)] [(101,1),(103,3)]
tkr4 = GM.buildGraphMorphism kr4 grafotipo [(112,2),(111,1)] []
trr4 = GM.buildGraphMorphism rr4 grafotipo [(121,1),(122,2)] [(122,2)]

--span
kr4_lr4 = GM.buildGraphMorphism kr4 lr4 [(111,101),(112,102)] []
l4 = buildTypedGraphMorphism tkr4 tlr4 kr4_lr4

kr4_rr4 = GM.buildGraphMorphism kr4 rr4 [(111,121),(112,122)] []
r4 = buildTypedGraphMorphism tkr4 trr4 kr4_rr4

deleteMSG = graphRule l4 r4 []

{-teste-}
lr5 = build [200,201] []
kr5 = build [300] []
rr5 = build [400] []

--tipagem
tlr5 = GM.buildGraphMorphism lr5 grafotipo [(200,1),(201,1)] []
tkr5 = GM.buildGraphMorphism kr5 grafotipo [(300,1)] []
trr5 = GM.buildGraphMorphism rr5 grafotipo [(400,1)] []

--span
kr5_lr5 = GM.buildGraphMorphism kr5 lr5 [(300,200)] []
l5 = buildTypedGraphMorphism tkr5 tlr5 kr5_lr5

kr5_rr5 = GM.buildGraphMorphism kr5 rr5 [(300,400)] []
r5 = buildTypedGraphMorphism tkr5 trr5 kr5_rr5

teste = graphRule l5 r5 []

{-testeNac-}
lr6 = build [212] []
kr6 = build [222] []
rr6 = build [232] []

--tipagem
tlr6 = GM.buildGraphMorphism lr6 grafotipo [(212,2)] []
tkr6 = GM.buildGraphMorphism kr6 grafotipo [(222,2)] []
trr6 = GM.buildGraphMorphism rr6 grafotipo [(232,2)] []

--span
kr6_lr6 = GM.buildGraphMorphism kr6 lr6 [(222,212)] []
l6 = buildTypedGraphMorphism tkr6 tlr6 kr6_lr6

kr6_rr6 = GM.buildGraphMorphism kr6 rr6 [(222,232)] []
r6 = buildTypedGraphMorphism tkr6 trr6 kr6_rr6

--nac
ng6 = build [242,241] []
nt6 = GM.buildGraphMorphism ng6 grafotipo [(242,2),(241,1)] []
lr6_n6 = GM.buildGraphMorphism lr6 ng6 [(212,242)] []
nac6 = buildTypedGraphMorphism tlr6 nt6 lr6_n6

wnac = graphRule l6 r6 [nac6]

{-testeNac2-}
lr7 = build [312] []
kr7 = build [322] []
rr7 = build [332] []

--tipagem
tlr7 = GM.buildGraphMorphism lr7 grafotipo [(312,2)] []
tkr7 = GM.buildGraphMorphism kr7 grafotipo [(322,2)] []
trr7 = GM.buildGraphMorphism rr7 grafotipo [(332,2)] []

--span
kr7_lr7 = GM.buildGraphMorphism kr7 lr7 [(322,312)] []
l7 = buildTypedGraphMorphism tkr7 tlr7 kr7_lr7

kr7_rr7 = GM.buildGraphMorphism kr7 rr7 [(322,332)] []
r7 = buildTypedGraphMorphism tkr7 trr7 kr7_rr7

wnac2 = graphRule l7 r7 []

{-testeCreate-}
lr8 = build [] []
kr8 = build [] []
rr8 = build [350] []

--tipagem
tlr8 = GM.buildGraphMorphism lr8 grafotipo [] []
tkr8 = GM.buildGraphMorphism kr8 grafotipo [] []
trr8 = GM.buildGraphMorphism rr8 grafotipo [(350,1)] []

--span
kr8_lr8 = GM.buildGraphMorphism kr8 lr8 [] []
l8 = buildTypedGraphMorphism tkr8 tlr8 kr8_lr8

kr8_rr8 = GM.buildGraphMorphism kr8 rr8 [] []
r8 = buildTypedGraphMorphism tkr8 trr8 kr8_rr8

testeCreate = graphRule l8 r8 []



{-Fim das Regras-}

--test1 = build [1] []
--test2 = build [1,2] []

--gm1 = GM.buildGraphMorphism test1 grafotipo [(1,1)] []
--gm2 = GM.buildGraphMorphism test2 grafotipo [(1,1),(2,1)] []

--t1 = GM.buildGraphMorphism test1 test2 [(1,1)] []
--t2 = GM.buildGraphMorphism test2 test1 [(1,1),(2,1)] []

-----

--rules = [sendMsg,getDATA,receiveMSG,deleteMSG]
--rules2 = rules ++ [teste,wnac,wnac2,testeCreate]

--rulesTest = concat (replicate 32 rules)
--27.9s useDelete old - nac matches part inj
--28.5s useDelete old - nac matches total inj
--33.6s useDelete categorial diagram - nac matches part inj
--34.2s useDelete categorial diagram - nac matches total inj

--rulesTest2 = concat (replicate 16 rules2)
-- 9.8s useDelete old - nac matches part inj
-- 9.9s useDelete old - nac matches total inj
--11.7s useDelete categorial diagram - nac matches part inj
--11.9s useDelete categorial diagram - nac matches total inj

--initGraph = GM.empty grafotipo grafotipo
--ggg = GG.graphGrammar initGraph [("sendMsg",sendMsg), ("getDATA", getDATA), ("receiveMsg", receiveMSG), ("deleteMsg", deleteMSG), ("teste", teste), ("wnac", wnac), ("wnac2", wnac2), ("testeCreate", testeCreate)]

-- fileName = "teste-conflito.ggx"

--fileName = "elevator.ggx"

-- fileName = "ev.ggx"

{-calculate = do
  tg <- XML.readTypeGraph fileName
  rs <- XML.readRules fileName
  let rulesNames = map (\((x,_,_,_),_) -> x) rs
  print rulesNames
  let rles = map (XML.instantiateRule (head tg)) rs
  print $ "Numero de regras: " ++ show (length rles)
  print "Delete-Use"
  let ud = m rles
  print ud
  print "Produce-Forbid"
  let pf = mpf rles
  print pf
  print "Produce Edge Delete Node"
  let pe = mpe rles
  print pe
  print "Matriz final"
  print $ ud + pf + pe
  --return (criticalPairs (rles!!0) (rles!!2))
  return (rles)
  --return ()
-}
{------
filtMono x = filter (\(_,q) -> M.monomorphism q) x

filtPairs inverseLeft x = filter (\(h1,_) -> satsGluingCond inverseLeft h1) x

calculatePushoutComplement inverseLeft x = map (\(h1,q21) -> let (k,r') = RW.calculatePushoutComplement h1 (left inverseLeft) in
 (h1,q21,k,r')) x

calculatePushout inverseLeft x = map (\(h1,q21,k,r') ->
 let (m1,l') = RW.calculatePushout k (right inverseLeft) in
 (h1,q21,k,r',m1,l')) x

filtM1 l x = filter (\(_,_,_,_,m1,_) -> satisfiesNACs l m1) x

h21 x n = concat
 (map (\(h1,q21,k,r',m1,l') ->
 let f = matches (M.domain n) (M.codomain k) FREE in
 if Prelude.null f
 then []
 else [(h1,q21,k,r',m1,l',head f)])
 x)

validH21 n x = filter (\(h1,q21,k,r',m1,l',l2d1) -> M.compose l2d1 r' == M.compose n q21) x

m1m2 x = map (\(h1,q21,k,r',m1,l',l2d1) -> (m1, M.compose l2d1 l')) x
filtM2 r = filter (\(m1,m2) -> satsGluingCond r m2)

------}

{-cpRT = criticalPairs receiveMSG teste
mA = m1 (cpRT!!1)
mB = m2 (cpRT!!1)
kToG = M.compose (left receiveMSG) mA
gToK = buildTypedGraphMorphism (M.codomain kToG) (M.domain kToG) (GM.inverse (TGM.mapping kToG))
gToR = M.compose gToK (right receiveMSG)
rToG = buildTypedGraphMorphism (M.codomain gToR) (M.domain gToR) (GM.inverse (TGM.mapping gToR))
nvMB = M.compose mB gToR

---------
cps = criticalPairs getDATA sendMsg
cp0 = cps!!0
l = left sendMsg
r = TGM.inverseTGM (right sendMsg)
deleted = M.compose l (m2 cp0)
created = M.compose r deleted
---------}

{-ri = sendMsg
le = sendMsg

n = head (nacs ri)
inverseRule = inverseGR le

pairs = createPairs (right le) n
filtPairs = filter (\(m'1,_) -> satsGluingCond inverseRule m'1) pairs
m' = map fst filtPairs
q = map snd filtPairs

kr' = map (\m'1 -> RW.calculatePushoutComplement m'1 (left inverseRule)) m'
k = map fst kr'
r' = map snd kr'

ml' = map (\x -> RW.calculatePushout x (right inverseRule)) k
mm1 = map fst ml'
l' = map snd ml'

m1k = zip mm1 k

filtM1 = filter (\(m1,_) -> satisfiesNACs le m1) m1k

h12 = map (\(_,k) -> matches (M.codomain (left ri)) (M.codomain k) FREE) filtM1
filtH12 = map (\(x,y,z) -> validH12 x y z) (zip3 h12 (map snd filtPairs) r')
adjH12 = ajeita filtH12 filtM1 l'

mm2 = map (\(h,m1,ls) -> (m1,M.compose h ls)) adjH12
filtM2 = filter (\(m1,m2) -> satsGluingCond ri m2) mm2

len = length filtM2

validH12 h12 q r' = filter (\h -> M.compose n q == M.compose h r') h12

ajeita [] _ _ = []
ajeita (h:hs) (m1:m1s) (l:ls) = (if Prelude.null h then [] else [(head h,m1,l)]) ++ (ajeita hs m1s ls)-}
---

{-g = fst (head filtPairs)
k = fst (RW.calculatePushoutComplement g (left inverseRule))
r = right inverseRule

kr = M.compose (TGM.invert r) k                                 -- invert r and compose with k, obtain kr : R -> D
createdNodess = TGM.orphanTypedNodes r                                -- nodes in R to be created
createdEdgess = TGM.orphanTypedEdges r                                -- edges in R to be created
nodeTable    = zip createdNodess (GM.newTypedNodes $ M.codomain kr) -- table mapping NodeIds in R to NodeIds in G'
edgeTable    = zip createdEdgess (GM.newTypedEdges $ M.codomain kr) -- table mapping EdgeIds in R to EdgeIds in G'

-- generate new node instances in G', associating them to the "created" nodes in R
kr'          = foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (M.domain kr) a
 in TGM.updateNodeRelation a b tp tgm)
 kr
 nodeTable

-- query the instance graphs R
typemor = M.domain         kr'                     -- typemor is the typed graph (R -> T)
g2      = M.domain         typemor                 -- g  is the instance graph R
mp      = TGM.mapping        kr'                     -- mp is the mapping of kr'  : (R -> D'), where D' = D + new nodes
s1 e = fromJust $ sourceOf g2 e                    -- obtain source of e in R
t1 e = fromJust $ targetOf g2 e                    -- obtain target of e in R
s2 e = fromJust $ GM.applyNode mp (s1 e)             -- obtain source of m'(e) in G'
t2 e = fromJust $ GM.applyNode mp (t1 e)             -- obtain target of m'(e) in G'
tp e = fromJust $ GM.applyEdge typemor e             -- obtain type of e in R

-- generate new edge table with new information
edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e, t2 e, tp e)) edgeTable

--(101,103,101,1009,1006,1002,1)
e101 = EdgeId 101
n103 = NodeId 103
n101 = NodeId 101
e1009 = EdgeId 1009
n1006 = NodeId 1006
n1002 = NodeId 1002
e1 = EdgeId 1

--(103,102,103,1010,1000,1006,3)
e103 = EdgeId 103
n102 = NodeId 102
e1010 = EdgeId 1010
n1000 = NodeId 1000
e3 = EdgeId 3

-- create new morphism adding all edges
kr''      = foldr (\(a,sa,ta,b,sb,tb,tp) tgm -> TGM.updateEdgeRelation a b (TGM.createEdgeOnCodomain b sb tb tp tgm) )
 kr'
 edgeTable'-}

-----

--graphEqClass = map (\x -> GP.genEqClass (mixTGM (right getDATA) x)) (nacs sendMsg)
--md = map (\x -> (map (mountTypedGraphMorphisms (right getDATA) x graphEqClass))) (nacs sendMsg)
--ms = map (map (mountTGM (right getDATA) "Right")) (map(\x -> GP.genEqClass (mixTGM x (right getDATA))) (nacs sendMsg))

--injectiveMatches = True

--m   r = matrix (length r) (length r) (\y -> length $ allDeleteUse       injectiveMatches (r!!((fst y)-1)) (r!!((snd y)-1)))
--mpf r = matrix (length r) (length r) (\y -> length $ allProduceForbid   injectiveMatches (r!!((fst y)-1)) (r!!((snd y)-1)))
--mpe r = matrix (length r) (length r) (\y -> length $ allProdEdgeDelNode injectiveMatches (r!!((fst y)-1)) (r!!((snd y)-1)))

--classes de equivalência dos lados esquerdos das regras
--utilizado apenas no módulo toJPG
--gg :: [GP.EqClassGraphMap]
--gg = GP.genEqClass $ mixLeftRule sendMsg sendMsg
--gg = GP.genEqClass $ mixTGM (head (nacs sendMsg)) (right sendMsg)

--classes de equivalencia em formato Text
--ggs = gind (map GP.eqGraph gg)

--main :: IO ()
--main = calculate--f2
{-
   do
      --f ((length ggs)-1)
      args <- getArgs
      parse args
--      let fileName = head args
--      print (if Prelude.null args then "Error" else ma)
      return ()

parse [] = error "Passe um arquivo, por favor"
parse fs = XML.main2 $ head fs-}

--r = rulesTest2

--apaga os .dot
--f2 =
--  do
--      writeFile ("matrix.txt") (show ((m r) + (mpf r) + (mpe r)))
--      return ()

--cria os .dot e os .jpg
{-f 0 =
   do
      writeFile ("0.dot") (write ggs 0)
      runCommand ("dot -Tjpg 0.dot > 0.jpg")
      return ()

f n =
   do
      writeFile ((show n)++".dot") (write ggs n)
      runCommand ("dot -Tjpg "++(show n)++".dot > "++(show n)++".jpg")
      (f (n-1))-}
--}
