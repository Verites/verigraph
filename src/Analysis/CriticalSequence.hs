module Analysis.CriticalSequence
 ( CS,
   CriticalSequence,
   namedCriticalSequence,
   allProduceUse,
   allDeliverDelete,
   getM1,
   getM2,
   getCSNac,
   getCS
   ) where

import qualified Abstract.Morphism         as M
import           Data.List                 (elemIndex)
import           Graph.GraphRule
import           Graph.EpiPairs            ()
import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW
import           Graph.TypedGraphMorphism
import           Graph.FindMorphism        ()

-- | Data representing the type of a 'CriticalPair'
data CS = ProduceUse | DeliverDelete deriving(Eq,Show)

data CriticalSequence a b = CriticalSequence {
    m1  :: TypedGraphMorphism a b,
    m2  :: TypedGraphMorphism a b,
    nac :: Maybe Int, --if is DeliverDelete, here is the index of the nac
    cs  :: CS
    } deriving (Eq,Show)

-- | Returns the left morphism of a 'CriticalSequence'
getM1 :: CriticalSequence a b -> TypedGraphMorphism a b
getM1 = m1

-- | Returns the right morphism of a 'CriticalSequence'
getM2 :: CriticalSequence a b -> TypedGraphMorphism a b
getM2 = m2

-- | Returns the type of a 'CriticalSequence'
getCS :: CriticalSequence a b -> CS
getCS = cs

-- | Returns the nac number of a 'CriticalSequence'
getCSNac :: CriticalSequence a b -> Maybe Int
getCSNac = nac

-- | Returns the Critical Sequences with rule names
namedCriticalSequence :: Bool -> Bool -> [(String, GraphRule a b)] -> [(String,String,[CriticalSequence a b])]
namedCriticalSequence nacInj inj r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences nacInj inj r1 r2)

-- | All Critical Sequences
criticalSequences :: Bool -> Bool
                  -> GraphRule a b
                  -> GraphRule a b
                  -> [CriticalSequence a b]
criticalSequences nacInj inj l r = allProduceUse nacInj inj l r ++ allDeliverDelete nacInj inj l r

-- | All ProduceUse caused by the derivation of @l@ before @r@
allProduceUse :: Bool -> Bool -> GraphRule a b -> GraphRule a b -> [CriticalSequence a b]
allProduceUse nacInj i l r = map (\(m1,m2) -> CriticalSequence m1 m2 Nothing ProduceUse) prodUse
  where
    invLeft = inverse i l
    pairs = createPairsCodomain i (left invLeft) (left r)
    gluing = filter (\(m1,m2) -> satsGluingNacsBoth nacInj i  (invLeft,m1) (r,m2)) pairs
    prodUse = filter (produceUse invLeft r) gluing

-- | Rule @l@ causes a produce-use dependency with @r@ if rule @l@ creates something that is used by @r@
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2
produceUse :: GraphRule a b -> GraphRule a b
           -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
           -> Bool
produceUse l _ (m1',m2) = Prelude.null filt
    where
        (_,e1) = RW.poc m1' (left l)
        l2TOd1 = M.matches M.ALL (M.domain m2) (M.domain e1)
        filt = filter (\h -> M.compose h e1 == m2) l2TOd1

-- | All DeliverDelete caused by the derivation of @l@ before @r@
-- rule @l@ causes a deliver-forbid conflict with @r@ if some NAC in @r@ turns satisfied after the aplication of @l@
allDeliverDelete :: Bool -> Bool
                 -> GraphRule a b
                 -> GraphRule a b
                 -> [CriticalSequence a b]
allDeliverDelete nacInj inj l r = concatMap (deliverDelete nacInj inj l inverseLeft r) (zip (nacs r) [0..])
  where
    inverseLeft = inverse inj l

-- | Check DeliverDelete for a NAC @n@ in @r@
deliverDelete :: Bool -> Bool
                      -> GraphRule a b -> GraphRule a b -> GraphRule a b
                      -> (TypedGraphMorphism a b, Int)
                      -> [CriticalSequence a b]
deliverDelete nacInj inj l inverseLeft r (n,idx) = let
        pairs = createPairsNac nacInj inj (codomain (left l)) n
        
        --filtFun = if nacInj then M.monomorphism else partialInjectiveTGM n
        filtPairs = filter (\(m1,_) -> {-(not inj || M.monomorphism m1) && filtFun q &&-}
                                       satsGluingAndNacs nacInj inj l m1
                                       ) pairs

        dpo = map (\(m1,q21) ->
                    let (k,d1) = RW.poc m1 (left l)
                        (m1',e1) = RW.po k (right l) in
                      (m1,q21,k,d1,m1',e1))
                  filtPairs

        filtM1 = filter (\(_,_,_,_,m1',_) -> satsNacs nacInj inj inverseLeft m1') dpo

        h21 = concatMap (\(m1,q21,k,d1,m1',e1) ->
                  let hs = M.matches M.ALL (M.domain n) (M.codomain k)
                      list = map (\h -> M.compose h d1 == M.compose n q21) hs in
                    case elemIndex True list of
                           Just ind -> [(m1,q21,k,d1,m1',e1,hs!!ind)]
                           Nothing  -> [])
                  filtM1

        m1m2 = map (\(_,_,_,_,m1',e1,l2d1) -> (m1', M.compose l2d1 e1)) h21

        filtM2 = filter (\(_,m2) -> {-(not inj || M.monomorphism m2) &&-}
                                      satsGluingAndNacs nacInj inj r m2) m1m2

        in map (\(m1,m2) -> CriticalSequence m1 m2 (Just idx) DeliverDelete) filtM2
