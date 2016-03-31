module Graph.NacOperations
( downwardShift
) where

import qualified Abstract.Morphism as M
import qualified CriticalPairs.CriticalPairs as CP
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.GraphMorphism as GM
import qualified Graph.Graph as G

-- | Guven a morphism @m : A -> B@ and a NAC @n : A -> N@, it shifts the NAC over the morphism resulting in a morphism list of type @n' : B -> N'@
downwardShift :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]
downwardShift m n' = newNacs
  where
    pairs = CP.createPairs n' m
    injectiveMorphisms = filter (\(e,_) -> M.monomorphism e) pairs
    validPO = filter (\(e,n) -> M.compose n' e == M.compose m n) injectiveMorphisms
    newNacs = map snd validPO

{- Some tests
tg = G.build [1,2] [(1,1,2),(2,2,2),(3,2,2)]

graphA = G.build [1] []
graphN = G.build [1,2] [(1,1,2),(2,2,2)]
graphB = G.build [1,2,3] [(1,1,3),(3,3,3)]

tgA = GM.gmbuild graphA tg [(1,1)] []
tgN = GM.gmbuild graphN tg [(1,1),(2,2)] [(1,1),(2,2)]
tgB = GM.gmbuild graphB tg [(1,1),(2,1),(3,2)] [(1,1),(3,3)]

mapN' = GM.gmbuild graphA graphN [(1,1)] []
mapM' = GM.gmbuild graphA graphB [(1,2)] []

n' = TGM.typedMorphism tgA tgN mapN'
m = TGM.typedMorphism tgA tgB mapM'

resp = downwardShift m n' -}
