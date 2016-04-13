module Graph.NacOperations
(  downwardShift
, injectiveDownwardShift
, shiftLeftNac
, inverse
) where

import           Abstract.Morphism
import           Analysis.EpiPairs
import           Graph.GraphRule
import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Graph.TypedGraphMorphism

-- | Revert a Rule shifting NACs
inverse :: Bool -> GraphRule a b -> GraphRule a b
inverse inj r = graphRule (right r) (left r) (concatMap (shiftLeftNac inj r) (nacs r))

-- | Given a rule @L <-l- K -r-> R@ and a Left NAC morphism @n : L -> N@, it shifts the NAC over the rule resulting in a list of Right NAC morphisms of type @n': R -> N'@
shiftLeftNac :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
shiftLeftNac inj rule n = [comatch n rule | satsGluing inj n rule]

-- | Given a morphism @m : A -> B@ and a NAC @n : A -> N@, it shifts the NAC over the morphism resulting in a morphism list of type @n' : B -> N'@
downwardShift :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
downwardShift m n' = newNacs
  where
    pairs = commutingPairs n' m
    injectiveMorphisms = filter (\(e,_) -> monomorphism e) pairs
    newNacs = map snd injectiveMorphisms

injectiveDownwardShift :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
injectiveDownwardShift m n' = [fst $ po n' m]


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
