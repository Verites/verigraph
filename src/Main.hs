{-# LANGUAGE TypeFamilies #-}

import CriticalPairs.VeriToGP
--import CriticalPairs.GPToJPG
import CriticalPairs.CriticalPairs
--import CriticalPairs.CriticalPairsTeste
import qualified CriticalPairs.GraphPart as GP
import Graph.Graph
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.Rewriting as RW
import Graph.GraphRule
import System.Process
import System.Environment
import System.Exit

import Data.Matrix

import qualified Abstract.Morphism as M
import Data.Maybe
import CriticalPairs.Matches
import CriticalPairs.GPToVeri

import qualified XML.GTXLReader as XML

iN = insertNode
iE = insertEdge
uN = GM.updateNodes
uE = GM.updateEdges

{-grafo tipo-}
grafotipo = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]

{-sendMSG-}
lr1 = build [11,13,14] [(11,13,11)]
kr1 = build [21,23,24] []
rr1 = build [31,33,34] [(35,33,34)]

--tipagem
tlr1 = GM.gmbuild lr1 grafotipo [(14,4),(13,3),(11,1)] [(11,1)]
tkr1 = GM.gmbuild kr1 grafotipo [(24,4),(23,3),(21,1)] []
trr1 = GM.gmbuild rr1 grafotipo [(34,4),(33,3),(31,1)] [(35,5)]

--span
kr1_lr1 = GM.gmbuild kr1 lr1 [(24,14),(23,13),(21,11)] []
l1 = TGM.typedMorphism tkr1 tlr1 kr1_lr1

kr1_rr1 = GM.gmbuild kr1 rr1 [(24,34),(23,33),(21,31)] []
r1 = TGM.typedMorphism tkr1 trr1 kr1_rr1

--nac
nacGraph = build [501,502,503,504] [(501,503,501),(503,502,503)]
nacType = GM.gmbuild nacGraph grafotipo [(501,1),(502,2),(503,3),(504,4)] [(501,1),(503,3)]
nacMap = GM.gmbuild lr1 nacGraph [(11,501),(13,503),(14,504)] [(11,501)] 
nacSendMsg = TGM.typedMorphism tlr1 nacType nacMap

sendMsg = graphRule l1 r1 [nacSendMsg]

{-getDATA-}
lr2 = build [42,43,44] [(44,42,44),(45,43,44)]
kr2 = build [52,53,54] [(55,53,54)]
rr2 = build [62,63,64] [(65,63,64),(63,62,63)]

--tipagem
tlr2 = GM.gmbuild lr2 grafotipo [(44,4),(43,3),(42,2)] [(44,4),(45,5)]
tkr2 = GM.gmbuild kr2 grafotipo [(54,4),(53,3),(52,2)] [(55,5)]
trr2 = GM.gmbuild rr2 grafotipo [(64,4),(63,3),(62,2)] [(65,5),(63,3)]

--span
kr2_lr2 = GM.gmbuild kr2 lr2 [(52,42),(53,43),(54,44)] [(55,45)]
l2 = TGM.typedMorphism tkr2 tlr2 kr2_lr2

kr2_rr2 = GM.gmbuild kr2 rr2 [(54,64),(53,63),(52,62)] [(55,65)]
r2 = TGM.typedMorphism tkr2 trr2 kr2_rr2

getDATA = graphRule l2 r2 []

{-receiveMSG-}
lr3 = build [71,72,73,74] [(75,73,74),(73,72,73)]
kr3 = build [84,83,82,81] [(83,82,83)]
rr3 = build [94,93,92,91] [(91,93,91),(93,92,93)]

--tipagem
tlr3 = GM.gmbuild lr3 grafotipo [(74,4),(73,3),(72,2),(71,1)] [(75,5),(73,3)]
tkr3 = GM.gmbuild kr3 grafotipo [(84,4),(83,3),(82,2),(81,1)] [(83,3)]
trr3 = GM.gmbuild rr3 grafotipo [(94,4),(93,3),(92,2),(91,1)] [(91,1),(93,3)]

--span
kr3_lr3 = GM.gmbuild kr3 lr3 [(84,74),(83,73),(82,72),(81,71)] [(83,73)]
l3 = TGM.typedMorphism tkr3 tlr3 kr3_lr3

kr3_rr3 = GM.gmbuild kr3 rr3 [(84,94),(83,93),(82,92),(81,91)] [(83,93)]
r3 = TGM.typedMorphism tkr3 trr3 kr3_rr3

receiveMSG = graphRule l3 r3 []

{-deleteMSG-}
lr4 = build [101,102,103] [(101,103,101),(103,102,103)]
kr4 = build [112,111] []
rr4 = build [121,122] [(122,122,121)]

--tipagem
tlr4 = GM.gmbuild lr4 grafotipo [(101,1),(102,2),(103,3)] [(101,1),(103,3)]
tkr4 = GM.gmbuild kr4 grafotipo [(112,2),(111,1)] []
trr4 = GM.gmbuild rr4 grafotipo [(121,1),(122,2)] [(122,2)]

--span
kr4_lr4 = GM.gmbuild kr4 lr4 [(111,101),(112,102)] []
l4 = TGM.typedMorphism tkr4 tlr4 kr4_lr4

kr4_rr4 = GM.gmbuild kr4 rr4 [(111,121),(112,122)] []
r4 = TGM.typedMorphism tkr4 trr4 kr4_rr4

deleteMSG = graphRule l4 r4 []

{-teste-}
lr5 = build [200,201] []
kr5 = build [300] []
rr5 = build [400] []

--tipagem
tlr5 = GM.gmbuild lr5 grafotipo [(200,1),(201,1)] []
tkr5 = GM.gmbuild kr5 grafotipo [(300,1)] []
trr5 = GM.gmbuild rr5 grafotipo [(400,1)] []

--span
kr5_lr5 = GM.gmbuild kr5 lr5 [(300,200)] []
l5 = TGM.typedMorphism tkr5 tlr5 kr5_lr5

kr5_rr5 = GM.gmbuild kr5 rr5 [(300,400)] []
r5 = TGM.typedMorphism tkr5 trr5 kr5_rr5

teste = graphRule l5 r5 []

{-testeNac-}
lr6 = build [212] []
kr6 = build [222] []
rr6 = build [232] []

--tipagem
tlr6 = GM.gmbuild lr6 grafotipo [(212,2)] []
tkr6 = GM.gmbuild kr6 grafotipo [(222,2)] []
trr6 = GM.gmbuild rr6 grafotipo [(232,2)] []

--span
kr6_lr6 = GM.gmbuild kr6 lr6 [(222,212)] []
l6 = TGM.typedMorphism tkr6 tlr6 kr6_lr6

kr6_rr6 = GM.gmbuild kr6 rr6 [(222,232)] []
r6 = TGM.typedMorphism tkr6 trr6 kr6_rr6

--nac
ng6 = build [242,241] []
nt6 = GM.gmbuild ng6 grafotipo [(242,2),(241,1)] []
lr6_n6 = GM.gmbuild lr6 ng6 [(212,242)] []
nac6 = TGM.typedMorphism tlr6 nt6 lr6_n6

wnac = graphRule l6 r6 [nac6]

{-testeNac2-}
lr7 = build [312] []
kr7 = build [322] []
rr7 = build [332] []

--tipagem
tlr7 = GM.gmbuild lr7 grafotipo [(312,2)] []
tkr7 = GM.gmbuild kr7 grafotipo [(322,2)] []
trr7 = GM.gmbuild rr7 grafotipo [(332,2)] []

--span
kr7_lr7 = GM.gmbuild kr7 lr7 [(322,312)] []
l7 = TGM.typedMorphism tkr7 tlr7 kr7_lr7

kr7_rr7 = GM.gmbuild kr7 rr7 [(322,332)] []
r7 = TGM.typedMorphism tkr7 trr7 kr7_rr7

wnac2 = graphRule l7 r7 []

{-testeCreate-}
lr8 = build [] []
kr8 = build [] []
rr8 = build [350] []

--tipagem
tlr8 = GM.gmbuild lr8 grafotipo [] []
tkr8 = GM.gmbuild kr8 grafotipo [] []
trr8 = GM.gmbuild rr8 grafotipo [(350,1)] []

--span
kr8_lr8 = GM.gmbuild kr8 lr8 [] []
l8 = TGM.typedMorphism tkr8 tlr8 kr8_lr8

kr8_rr8 = GM.gmbuild kr8 rr8 [] []
r8 = TGM.typedMorphism tkr8 trr8 kr8_rr8

testeCreate = graphRule l8 r8 []

{-Fim das Regras-}

-----

--rules = [sendMsg,getDATA,receiveMSG,deleteMSG]
rules = [sendMsg,getDATA,receiveMSG,deleteMSG,teste,wnac,wnac2,testeCreate]
--rules = [receiveMSG,teste]
--rules = [sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG,sendMsg,getDATA,receiveMSG,deleteMSG]
--10.7s
--15s -- 28s

{-cpRT = criticalPairs receiveMSG teste
mA = m1 (cpRT!!1)
mB = m2 (cpRT!!1)
kToG = M.compose (left receiveMSG) mA
gToK = TGM.typedMorphism (M.codomain kToG) (M.domain kToG) (GM.inverse (TGM.mapping kToG))
gToR = M.compose gToK (right receiveMSG)
rToG = TGM.typedMorphism (M.codomain gToR) (M.domain gToR) (GM.inverse (TGM.mapping gToR))
nvMB = M.compose mB gToR

---------
cps = criticalPairs getDATA sendMsg
cp0 = cps!!0
l = left sendMsg
r = TGM.inverseTGM (right sendMsg)
deleted = M.compose l (m2 cp0)
created = M.compose r deleted
---------}

ri = sendMsg
le = receiveMSG

n = head (nacs ri)
inverseRule = inverseGR le
pairs = createPairs (right le) n
filtPairs = filter (\(m'1,_) -> satsGluingCond inverseRule m'1) pairs
sndPairs = map snd filtPairs
mm1 = map (\(m'1,_) -> RW.comatch (RW.dpo m'1 inverseRule)) filtPairs
filtM1 = filter (satsNacs le) mm1
l' = map (\m1 -> snd (RW.poc m1 (left le))) filtM1
r' = map (\(_,m'1) -> snd (RW.poc m'1 (left inverseRule))) filtPairs
h12 = map (\x -> matches (M.codomain (left ri)) (M.domain x) FREE) l'
filtH12 = map (\(x,y,z) -> validH12 x y z) (zip3 h12 sndPairs r')
adjH12 = ajeita filtH12 filtM1 l'
mm2 = map (\(h,m1,ls) -> (m1,M.compose h ls)) adjH12
filtM2 = filter (\(m1,m2) -> satsGluingCond ri m2) mm2

len = length filtM2

validH12 h12 q r' = filter (\h -> M.compose n q == M.compose h r') h12
ajeita [] _ _ = []
ajeita (h:hs) (m1:m1s) (l:ls) = (if Prelude.null h then [] else [(head h,m1,l)]) ++ (ajeita hs m1s ls)
---

graphEqClass = map (\x -> GP.genEqClass (mixTGM (right getDATA) x)) (nacs sendMsg)
--md = map (\x -> (map (mountTGMBoth (right getDATA) x graphEqClass))) (nacs sendMsg)
ms = map (map (mountTGM (right getDATA) "Right")) (map(\x -> GP.genEqClass (mixTGM x (right getDATA))) (nacs sendMsg))

m  = matrix (length rules) (length rules) (\y -> fst $ countCP (rules!!((fst y)-1)) (rules!!((snd y)-1)))
--mb  = matrix (length rules) (length rules) (\y -> fst $ countCP2 (rules!!((fst y)-1)) (rules!!((snd y)-1)))
mpf = matrix (length rules) (length rules) (\y -> snd $ countCP (rules!!((fst y)-1)) (rules!!((snd y)-1)))
--mpfb = matrix (length rules) (length rules) (\y -> snd $ countCP2 (rules!!((fst y)-1)) (rules!!((snd y)-1)))

--ma2  = matrix (length rules) (length rules) (\y -> criticalPairs  (rules!!((fst y)-1)) (rules!!((snd y)-1)))
--mb2  = matrix (length rules) (length rules) (\y -> criticalPairs2 (rules!!((fst y)-1)) (rules!!((snd y)-1)))

--classes de equivalência dos lados esquerdos das regras
--utilizado apenas no módulo toJPG
gg :: [GP.EqClassGraphMap]
--gg = GP.genEqClass $ mixLeftRule sendMsg sendMsg
gg = GP.genEqClass $ mixTGM (head (nacs sendMsg)) (right sendMsg)

--classes de equivalencia em formato Text
--ggs = gind (map GP.eqGraph gg)

main :: IO ()
main = f2{-
   do
      --f ((length ggs)-1)
      args <- getArgs
      parse args
--      let fileName = head args
--      print (if Prelude.null args then "Error" else ma)
      return ()

parse [] = error "Passe um arquivo, por favor"
parse fs = XML.main2 $ head fs-}

--apaga os .dot
f2 =
   do
      writeFile ("m.txt") ((show (length rules)) ++ "\n" ++ (show m))
      writeFile ("mpf.txt") ((show (length rules)) ++ "\n" ++ (show mpf))
      return ()

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
