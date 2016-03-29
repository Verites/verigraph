module CriticalPairs.CriticalSequence where

import qualified Graph.GraphRule as GR 
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.Rewriting as RW
import qualified Abstract.Morphism as M
import qualified CriticalPairs.Matches as MT
import qualified CriticalPairs.CriticalPairs as CP

import Data.List (elemIndex)
import Data.List.Utils (countElem)
import Data.Maybe (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CS = ProduceUse | DeliverDelete deriving(Eq,Show)

data CriticalSequence a b = CriticalSequence {
    m1 :: TGM.TypedGraphMorphism a b,
    m2 :: TGM.TypedGraphMorphism a b,
    nac :: Maybe Int, --if is DeliverDelete, here is the index of the nac
    cs :: CS
    } deriving (Eq,Show)

-- | Returns the left morphism of a 'CriticalSequence'
getM1 :: CriticalSequence a b -> TGM.TypedGraphMorphism a b
getM1 = m1

-- | Returns the right morphism of a 'CriticalSequence'
getM2 :: CriticalSequence a b -> TGM.TypedGraphMorphism a b
getM2 = m2

-- | Returns the type of a 'CriticalSequence'
getCS :: CriticalSequence a b -> CS
getCS = cs

-- | Returns the nac number of a 'CriticalSequence'
getCSNac :: CriticalSequence a b -> Maybe Int
getCSNac = nac

namedCriticalSequence :: Bool -> Bool -> [(String, GR.GraphRule a b)] -> [(String,String,[CriticalSequence a b])]
namedCriticalSequence nacInj inj r = map (\(x,y) -> getCPs x y) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences nacInj inj r1 r2)

criticalSequences :: Bool -> Bool
                  -> GR.GraphRule a b
                  -> GR.GraphRule a b
                  -> [CriticalSequence a b]
criticalSequences nacInj inj l r = (allProduceUse nacInj inj l r) ++ (allDeliverDelete nacInj inj l r)

-- | Revert a Rule shifting NACs
-- stay here until do not resolve cycle imports
inverse :: GR.GraphRule a b -> GR.GraphRule a b
inverse r = GR.graphRule (GR.right r) (GR.left r) (concat (map (invNac r) (GR.nacs r)))

satsGluingCondWithoutNac :: GR.GraphRule a b -> TGM.TypedGraphMorphism a b -> Bool
satsGluingCondWithoutNac rule m = identificationCondition && danglingCondition
    where
        identificationCondition = CP.satsDelItems rule m
        danglingCondition       = CP.satsIncEdges rule m

invNac :: GR.GraphRule a b -> TGM.TypedGraphMorphism a b -> [TGM.TypedGraphMorphism a b]
invNac rule n = if satsGluingCondWithoutNac rule n then [RW.comatch n rule] else []

allProduceUse :: Bool -> Bool -> GR.GraphRule a b -> GR.GraphRule a b -> [CriticalSequence a b]
allProduceUse nacInj i l r = map (\(m1,m2) -> CriticalSequence m1 m2 Nothing ProduceUse) prodUse
  where
    invLeft = inverse l
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

allDeliverDelete :: Bool -> Bool
                 -> GR.GraphRule a b
                 -> GR.GraphRule a b
                 -> [CriticalSequence a b]
allDeliverDelete nacInj inj l r = concat (map (deliverDelete nacInj inj l r) (GR.nacs r))

deliverDelete :: Bool -> Bool
                      -> GR.GraphRule a b -> GR.GraphRule a b
                      -> TGM.TypedGraphMorphism a b
                      -> [CriticalSequence a b]
deliverDelete nacInj inj l r n = let
        inverseLeft = inverse l
        
        pairs = CP.createPairs (GR.right inverseLeft) n
        
        filtFun = if nacInj then M.monomorphism else TGM.partialInjectiveTGM n
        filtMono = filter (\(_,q) -> filtFun q) pairs
        
        filtPairs = filter (\(m1,_) -> CP.satsGluingCond nacInj l m1) filtMono

        poc = map (\(m1,q21) -> let (k,d1) = RW.poc m1 (GR.right inverseLeft) in
                                 (m1,q21,k,d1))
                 filtPairs
        
        po = map (\(m1,q21,k,d1) ->
                   let (m1',e1) = RW.po k (GR.left inverseLeft) in
                     (m1,q21,k,d1,m1',e1))
                 poc

        filtM1 = filter (\(_,_,_,_,m1',_) -> CP.satsNacs nacInj inverseLeft m1') po
        
        h21 = concat $
                map (\(m1,q21,k,d1,m1',e1) ->
                  let hs = MT.matches (M.domain n) (M.codomain k) MT.FREE in
                    case Prelude.null hs of
                      True  -> []
                      False -> [(m1,q21,k,d1,m1',e1,hs)])
                  filtM1

        validH21 = concat $
                     map (\(m1,q21,k,d1,m1',e1,hs) ->
                       let list = map (\h -> M.compose h d1 == M.compose n q21) hs in
                         case (elemIndex True list) of
                           Just ind -> [(m1,q21,k,d1,m1',e1,hs!!ind)]
                           Nothing  -> [])
                       h21
        
        m1m2 = map (\(_,_,_,_,m1',e1,l2d1) -> (m1', M.compose l2d1 e1)) validH21
        
        filtM2 = filter (\(m1,m2) -> CP.satsGluingCond nacInj r m2) m1m2
        
        filtInj = filter (\(m1,m2) -> M.monomorphism m1 && M.monomorphism m2) filtM2
        
        idx = elemIndex n (GR.nacs r)
        
        in map (\(m1,m2) -> CriticalSequence m1 m2 idx DeliverDelete) (if inj then filtInj else filtM2)
