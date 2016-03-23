module CriticalPairs.CriticalSequence where

import qualified Graph.GraphRule as GR 
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.Rewriting as RW
import qualified Abstract.Morphism as M
import qualified CriticalPairs.Matches as MT
import qualified CriticalPairs.CriticalPairs as CP

data CriticalSequence a b = CriticalSequence {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b
    } deriving (Eq,Show)

-- | Revert a Rule shifting NACs
-- stay here until do not resolve cycle imports
inverse :: Bool -> GR.GraphRule a b -> GR.GraphRule a b
inverse nacInj r = GR.graphRule (GR.right r) (GR.left r) (concat (map (invNac nacInj r) (GR.nacs r)))

satsGluingCondWithoutNac :: GR.GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCondWithoutNac rule m = identificationCondition && danglingCondition
    where
        identificationCondition = CP.satsDelItems rule m
        danglingCondition       = CP.satsIncEdges rule m

invNac :: Bool -> GR.GraphRule a b -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]
invNac nacInj rule n = if satsGluingCondWithoutNac rule n then [RW.comatch n rule] else []

allProduceUse :: Bool -> Bool -> GR.GraphRule a b -> GR.GraphRule a b -> [CriticalSequence a b]
allProduceUse nacInj i l r = map (\(m1,m2) -> CriticalSequence m1 m2) prodUse
  where
    invLeft = inverse nacInj l
    pairs = CP.createPairs (GR.left invLeft) (GR.left r)
    inj = filter (\(m1,m2) -> M.monomorphism m1 && M.monomorphism m2) pairs
    gluing = filter (\(m1,m2) -> CP.satsGluingCondBoth nacInj (invLeft,m1) (r,m2)) (if i then inj else pairs)
    prodUse = filter (produceUse invLeft r) gluing

produceUse :: GR.GraphRule a b -> GR.GraphRule a b
           -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)
           -> Bool
produceUse l r (m1',m2) = Prelude.null filt
    where
        (_,e1) = RW.poc m1' (GR.left l)
        l2TOd1 = MT.matches (M.domain m2) (M.domain e1) MT.FREE
        filt = filter (\h -> M.compose h e1 == m2) l2TOd1

allDeliverDelete :: Bool -> Bool -> GR.GraphRule a b -> GR.GraphRule a b -> [CriticalSequence a b]
allDeliverDelete nacInj i l r = map (\(m1,m2) -> CriticalSequence m1 m2) delDel
  where
    invLeft = inverse nacInj l
    pairs = CP.createPairs (GR.left invLeft) (GR.left r)
    inj = filter (\(m1',m2) -> M.monomorphism m1' && M.monomorphism m2) pairs
    gluing = filter (\(m1',m2) -> CP.satsGluingCondBoth nacInj (invLeft,m1') (r,m2)) (if i then inj else pairs)
    delDel = filter (deliverDelete nacInj invLeft r) gluing

deliverDelete :: Bool -> GR.GraphRule a b -> GR.GraphRule a b
           -> (TGM.TypedGraphMorphism a b,TGM.TypedGraphMorphism a b)
           -> Bool
deliverDelete nacInj l r (m1',m2) = (not (Prelude.null filt)) && (not (Prelude.null (GR.nacs r))) && (False `notElem` filtQs)
    where
        (noname,e1) = RW.poc m1' (GR.left l)
        l2TOd1 = MT.matches (M.domain m2) (M.domain e1) MT.FREE
        filt = filter (\h -> M.compose h e1 == m2) l2TOd1
        (m1,d1) = RW.po noname (GR.right l)
        filtQs = map (checkQ nacInj m1 (head filt) d1) (GR.nacs r)

checkQ :: Bool -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> Bool
checkQ nacInj m1 h d n = not $ Prelude.null filtQ
  where
    mats = MT.matches (M.codomain n) (M.codomain m1) MT.FREE
    satsFun = if nacInj then M.monomorphism else TGM.partialInjectiveTGM n
    filtInj = filter satsFun mats
    filtQ = filter (\q -> M.compose n q == M.compose h d) filtInj











