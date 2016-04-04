module Graph.NacOperations
( downwardShift
, leftShiftNac
) where

import           Abstract.Morphism
import           Analysis.CriticalPairs
import           Analysis.EpiPairs
import           Analysis.GluingConditions
import qualified Graph.GraphRule           as GR
import qualified Graph.Rewriting           as RW
import           Graph.TypedGraphMorphism

-- | Given a morphism @m : A -> B@ and a NAC @n : A -> N@, it shifts the NAC over the morphism resulting in a morphism list of type @n' : B -> N'@
downwardShift :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
downwardShift m n' = newNacs
  where
    pairs = createPairs n' m
    injectiveMorphisms = filter (\(e,_) -> monomorphism e) pairs
    validPO = filter (\(e,n) -> compose n' e == compose m n) injectiveMorphisms
    newNacs = map snd validPO

-- | Given a rule @L <-l- K -r-> R@ and a Right NAC morphism @n : R -> N@, it shifts the NAC over the rule resulting in a list of Left NAC morphisms of type @n': L -> N'@
leftShiftNac :: GR.GraphRule a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
leftShiftNac rule n = [RW.comatch n rule | satsGluingCondWithoutNac rule n]

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
