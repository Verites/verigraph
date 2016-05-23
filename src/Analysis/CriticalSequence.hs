{-# LANGUAGE FlexibleInstances #-}

module Analysis.CriticalSequence
 ( CS (..),
   CriticalSequence,
   criticalSequences,
   namedCriticalSequence,
   allProduceUse,
   allDeliverDelete,
   getMatch,
   getComatch,
   getCSNac,
   getCSNacIdx,
   getCS
   ) where

import           Abstract.Morphism
import           Data.List                 (elemIndex)
import           Graph.EpiPairs            ()
import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW hiding (comatch)
import           Graph.FindMorphism        ()

-- | Data representing the type of a 'CriticalPair'
data CS = ProduceUse | DeliverDelete deriving (Eq,Show)

-- | A Critical Sequence is defined as two matches (m1,m2) from the left side of their rules to a same graph.
-- 
-- This diagram shows graphs and morphisms names used in the algorithms below
--
-- l       = production (L1,K1,R1,[N1]) (N1 from L1)
--
-- invLeft = production (R1,K1,L1,[N1]) (N1 from R1)
--
-- r       = production (L2,K2,R2,[N2])
--
-- @
--                    N1    N2
--                    ^      ^
--          l     r   │      │n
--     L1◀─────K1────▶R1    L2◀────K2─────▶R2
--     │       │       \\   /       │       │
--   m1│      k│     m1'\\ /m2'     │       │
--     ▼       ▼         ▼         ▼       ▼
--     P1◀─────D1───────▶G◀───────D2──────▶P2
--         r'       l' 
-- @
-- 
-- m2  :: from L2 to P1
--
-- h21 :: from L2 to D1
--
-- q21 (nacMatch) :: from N2 to P1

data CriticalSequence m = CriticalSequence {
    match :: Maybe (m, m),
    comatch :: (m, m),
    nac :: Maybe (m, Int), --if is DeliverDelete, here is the index of the nac
    cs  :: CS
    } deriving (Eq,Show)

-- | Returns the matches (m1, m2)
getMatch :: CriticalSequence m -> Maybe (m, m)
getMatch = match

-- | Returns the comatches (m1', m2')
getComatch :: CriticalSequence m -> (m, m)
getComatch = comatch

-- | Returns the type of a 'CriticalSequence'
getCS :: CriticalSequence m -> CS
getCS = cs

-- | Returns the nac match of a 'CriticalSequence'
getCSNac :: CriticalSequence m -> Maybe m
getCSNac cs = case nac cs of
                Just (nac,_) -> Just nac
                Nothing -> Nothing

-- | Returns the nac index of a 'CriticalSequence'
getCSNacIdx :: CriticalSequence m -> Maybe Int
getCSNacIdx cs = case nac cs of
                   Just (_,idx) -> Just idx
                   Nothing -> Nothing

-- | Returns the Critical Sequences with rule names
namedCriticalSequence :: (EpiPairs m, DPO m) => Bool -> Bool -> [(String, Production m)] -> [(String,String,[CriticalSequence m])]
namedCriticalSequence nacInj inj r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences nacInj inj r1 r2)

-- | All Critical Sequences
criticalSequences :: (EpiPairs m, DPO m) => Bool -> Bool
                  -> Production m
                  -> Production m
                  -> [CriticalSequence m]
criticalSequences nacInj inj l r = allProduceUse nacInj inj l r ++ allDeliverDelete nacInj inj l r

-- | All ProduceUse caused by the derivation of @l@ before @r@
allProduceUse :: (DPO m, EpiPairs m) => Bool -> Bool -> Production m -> Production m -> [CriticalSequence m]
allProduceUse nacInj i l r = map (\(m1,m2) -> CriticalSequence Nothing (m1,m2) Nothing ProduceUse) prodUse
  where
    invLeft = inverse i l
    pairs = createPairsCodomain i (left invLeft) (left r)
    gluing = filter (\(m1',m2') -> satsGluingNacsBoth nacInj i  (invLeft,m1') (r,m2')) pairs
    prodUse = filter (produceUse invLeft r) gluing

-- | Rule @l@ causes a produce-use dependency with @r@ if rule @l@ creates something that is used by @r@
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2
produceUse :: (AdhesiveHLR m, FindMorphism m, Morphism m, Eq m) => Production m -> Production m
           -> (m, m)
           -> Bool
produceUse l _ (m1',m2') = Prelude.null filt
    where
        (_,l') = RW.poc m1' (left l)
        l2TOd1 = matches ALL (domain m2') (domain l')
        filt = filter (\h -> compose h l' == m2') l2TOd1

-- | All DeliverDelete caused by the derivation of @l@ before @r@
-- rule @l@ causes a deliver-forbid conflict with @r@ if some NAC in @r@ turns satisfied after the aplication of @l@
allDeliverDelete :: (DPO m, EpiPairs m) => Bool -> Bool
                 -> Production m
                 -> Production m
                 -> [CriticalSequence m]
allDeliverDelete nacInj inj l r = concatMap (deliverDelete nacInj inj l inverseLeft r) (zip (nacs r) [0..])
  where
    inverseLeft = inverse inj l

-- | Check DeliverDelete for a NAC @n@ in @r@
deliverDelete :: (EpiPairs m, Morphism m, DPO m) => Bool -> Bool
                      -> Production m -> Production m -> Production m
                      -> (m, Int)
                      -> [CriticalSequence m]
deliverDelete nacInj inj l inverseLeft r (n,idx) = let
        pairs = createPairsNac nacInj inj (codomain (left l)) n
        
        --filtFun = if nacInj then M.monomorphism else partialInjectiveTGM n
        filtPairs = filter (\(m1,_) -> {-(not inj || M.monomorphism m1) && filtFun q &&-}
                                       satsGluingAndNacs nacInj inj l m1
                                       ) pairs

        dpo = map (\(m1,q21) ->
                    let (k,r') = RW.poc m1 (left l)
                        (m1',l') = RW.po k (right l) in
                      (m1,q21,k,r',m1',l'))
                  filtPairs

        filtM1 = filter (\(_,_,_,_,m1',_) -> satsNacs nacInj inj inverseLeft m1') dpo

        h21 = concatMap (\(m1,q21,k,r',m1',l') ->
                  let hs = matches ALL (domain n) (codomain k)
                      list = map (\h -> compose h r' == compose n q21) hs in
                    case elemIndex True list of
                           Just ind -> [(m1,q21,k,r',m1',l',hs!!ind)]
                           Nothing  -> [])
                  filtM1

        defineM2 = map (\(m1,q21,_,r',m1',l',l2d1) -> (q21, m1, compose l2d1 r', m1', compose l2d1 l')) h21

        filtM2 = filter (\(_,_,_,_,m2') -> {-(not inj || M.monomorphism m2) &&-}
                                      satsGluingAndNacs nacInj inj r m2') defineM2

        in map (\(q21,m1,m2,m1',m2') -> CriticalSequence (Just (m1,m2)) (m1',m2') (Just (q21,idx)) DeliverDelete) filtM2
