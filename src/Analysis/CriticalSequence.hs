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
import           Analysis.EpiPairs         (createPairs)
import           Analysis.GluingConditions
import qualified Analysis.Matches          as MT
import           Data.List                 (elemIndex)
import           Graph.GraphRule
import qualified Graph.Rewriting           as RW
import           Graph.TypedGraphMorphism

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

namedCriticalSequence :: Bool -> Bool -> [(String, GraphRule a b)] -> [(String,String,[CriticalSequence a b])]
namedCriticalSequence nacInj inj r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences nacInj inj r1 r2)

criticalSequences :: Bool -> Bool
                  -> GraphRule a b
                  -> GraphRule a b
                  -> [CriticalSequence a b]
criticalSequences nacInj inj l r = allProduceUse nacInj inj l r ++ allDeliverDelete nacInj inj l r

-- | Revert a Rule shifting NACs
-- stay here until do not resolve cycle imports
inverse :: Bool -> GraphRule a b -> GraphRule a b
inverse inj r = graphRule (right r) (left r) (concatMap (invNac inj r) (nacs r))

invNac :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
invNac inj rule n = [RW.comatch n rule | satsGluing inj rule n]

allProduceUse :: Bool -> Bool -> GraphRule a b -> GraphRule a b -> [CriticalSequence a b]
allProduceUse nacInj i l r = map (\(m1,m2) -> CriticalSequence m1 m2 Nothing ProduceUse) prodUse
  where
    invLeft = inverse i l
    pairs = createPairs (left invLeft) (left r)
    inj = filter (\(m1,m2) -> M.monomorphism m1 && M.monomorphism m2) pairs
    gluing = filter (\(m1,m2) -> satsGluingNacsBoth nacInj i  (invLeft,m1) (r,m2)) (if i then inj else pairs)
    prodUse = filter (produceUse invLeft r) gluing

produceUse :: GraphRule a b -> GraphRule a b
           -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
           -> Bool
produceUse l r (m1',m2) = Prelude.null filt
    where
        (_,e1) = RW.poc m1' (left l)
        l2TOd1 = MT.matches MT.FREE (M.domain m2) (M.domain e1)
        filt = filter (\h -> M.compose h e1 == m2) l2TOd1

allDeliverDelete :: Bool -> Bool
                 -> GraphRule a b
                 -> GraphRule a b
                 -> [CriticalSequence a b]
allDeliverDelete nacInj inj l r = concatMap (deliverDelete nacInj inj l r) (nacs r)

deliverDelete :: Bool -> Bool
                      -> GraphRule a b -> GraphRule a b
                      -> TypedGraphMorphism a b
                      -> [CriticalSequence a b]
deliverDelete nacInj inj l r n = let
        inverseLeft = inverse inj l

        pairs = createPairs (right inverseLeft) n

        filtFun = if nacInj then M.monomorphism else partialInjectiveTGM n
        filtPairs = filter (\(m1,q) -> (not inj || M.monomorphism m1)
                                    && filtFun q
                                    && satsGluingAndNacs nacInj inj l m1
                                    ) pairs

        dpo = map (\(m1,q21) ->
                    let (k,d1) = RW.poc m1 (right inverseLeft)
                        (m1',e1) = RW.po k (left inverseLeft) in
                      (m1,q21,k,d1,m1',e1))
                  filtPairs

        filtM1 = filter (\(_,_,_,_,m1',_) -> satsNacs nacInj inverseLeft m1') dpo

        h21 = concatMap (\(m1,q21,k,d1,m1',e1) ->
                  let hs = MT.matches MT.FREE (M.domain n) (M.codomain k) in
                      list = map (\h -> M.compose h d1 == M.compose n q21) hs in
                    case elemIndex True list of
                           Just ind -> [(m1,q21,k,d1,m1',e1,hs!!ind)]
                           Nothing  -> [])
                  filtM1
        
        m1m2 = map (\(_,_,_,_,m1',e1,l2d1) -> (m1', M.compose l2d1 e1)) h21

        filtM2 = filter (\(_,m2) -> (not inj || M.monomorphism m2)
                                 && satsGluingAndNacs nacInj inj r m2) m1m2

        idx = elemIndex n (nacs r)

        in map (\(m1,m2) -> CriticalSequence m1 m2 idx DeliverDelete) filtM2
