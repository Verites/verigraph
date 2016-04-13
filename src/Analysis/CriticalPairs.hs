module Analysis.CriticalPairs
 ( CP,
   CriticalPair,
   criticalPairs,
   namedCriticalPairs,
   allDeleteUse,
   allProduceForbid,
   allProdEdgeDelNode,
   getM1,
   getM2,
   getCPNac,
   getCP
   ) where

import           Abstract.Morphism
import           Analysis.EpiPairs         (createPairsCodomain)
import           Analysis.GluingConditions
import qualified Analysis.Matches          as MT
import           Data.List                 (elemIndex)
import           Data.List.Utils           (countElem)
import           Data.Maybe                (isJust,mapMaybe)
import qualified Graph.GraphMorphism       as GM
import           Graph.GraphRule
import           Graph.NacOperations
import qualified Graph.Rewriting           as RW
import           Graph.TypedGraphMorphism

-- | Data representing the type of a 'CriticalPair'
data CP = FOL | DeleteUse | ProduceForbid | ProduceEdgeDeleteNode deriving(Eq,Show)

-- | A Critical Pair is defined as two matches (m1,m2) from the left side of their rules to a same graph.
-- It assumes that the derivation of the rule with match @m1@ causes a conflict with the rule with match @m2@
data CriticalPair a b = CriticalPair {
    m1  :: TypedGraphMorphism a b,
    m2  :: TypedGraphMorphism a b,
    nac :: Maybe Int, --if is ProduceForbid, here is the index of the nac
    cp  :: CP
    } deriving (Eq,Show)

-- | Returns the left morphism of a 'CriticalPair'
getM1 :: CriticalPair a b -> TypedGraphMorphism a b
getM1 = m1

-- | Returns the right morphism of a 'CriticalPair'
getM2 :: CriticalPair a b -> TypedGraphMorphism a b
getM2 = m2

-- | Returns the type of a 'CriticalPair'
getCP :: CriticalPair a b -> CP
getCP = cp

-- | Returns the nac number of a 'CriticalPair'
getCPNac :: CriticalPair a b -> Maybe Int
getCPNac = nac

--instance Show (CriticalPair a b) where
--  show (CriticalPair m1 m2 cp) = "{"++(show $ TGM.mapping m1)++(show $ TGM.mapping m2)++(show cp)++"}"

-- | Returns the Critical Pairs with rule names
namedCriticalPairs :: Bool -> Bool -> [(String, GraphRule a b)] -> [(String,String,[CriticalPair a b])]
namedCriticalPairs nacInj inj r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalPairs nacInj inj r1 r2)

-- | All Critical Pairs
criticalPairs :: Bool -> Bool
              -> GraphRule a b
              -> GraphRule a b
              -> [CriticalPair a b]
--criticalPairs nacInj inj l r = (allDeleteUse nacInj inj l r) ++ (allProduceForbid nacInj inj l r) ++ (allProdEdgeDelNode nacInj inj l r)
criticalPairs nacInj inj l r = allDeleteUseAndDangling nacInj inj l r ++ allProduceForbid nacInj inj l r

-- Checks if a pair of morphisms is injective
checkMono :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> Bool
checkMono (m1,m2) = monomorphism m1 && monomorphism m2

---- Delete-Use

-- | Tests DeleteUse and ProdEdgeDelNode for the same pairs
-- more efficient than deal separately
allDeleteUseAndDangling :: Bool -> Bool
                        -> GraphRule a b
                        -> GraphRule a b
                        -> [CriticalPair a b]
allDeleteUseAndDangling nacInj i l r = mapMaybe (deleteUseDangling l r) gluing
  where
    pairs = createPairsCodomain (left l) (left r)
    inj = filter checkMono pairs
    gluing = filter (\(m1,m2) -> satsGluingNacsBoth nacInj i (l,m1) (r,m2)) (if i then inj else pairs)

deleteUseDangling :: GraphRule a b -> GraphRule a b
                  -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
                  -> Maybe (CriticalPair a b)
deleteUseDangling l r (m1,m2) = cp
  where
    (k,l') = RW.poc m1 (left l)
    lTOd = MT.matches MT.ALL (domain m2) (domain l')
    matchD = filter (\x -> m2 == compose x l') lTOd
    (m1',r') = RW.po k (right l)
    m2' = compose (head matchD) r'
    dang = not (satsIncEdges (left r) m2')
    cp = case (null matchD, dang) of
           (True,_)     -> Just (CriticalPair m1 m2 Nothing DeleteUse)
           (False,True) -> Just (CriticalPair m1 m2 Nothing ProduceEdgeDeleteNode)
           _            -> Nothing

-- | All DeleteUse caused by the derivation of @l@ before @r@
allDeleteUse :: Bool -> Bool
             -> GraphRule a b
             -> GraphRule a b
             -> [CriticalPair a b]
allDeleteUse nacInj i l r = map (\(m1,m2) -> CriticalPair m1 m2 Nothing DeleteUse) delUse
    where
        pairs = createPairsCodomain (left l) (left r)                                --get all jointly surjective pairs of L1 and L2
        inj = filter checkMono pairs
        gluing = filter (\(m1,m2) -> satsGluingNacsBoth nacInj i (l,m1) (r,m2)) (if i then inj else pairs) --filter the pairs that not satisfie gluing conditions of L and R
        delUse = filter (deleteUse l r) gluing                               --select just the pairs that are in DeleteUse conflict

-- | Rule @l@ causes a delete-use conflict with @r@ if rule @l@ deletes something that is used by @r@
-- DeleteUse using a most aproximated algorithm of the categorial diagram
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2
deleteUse :: GraphRule a b -> GraphRule a b
           -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
           -> Bool
deleteUse l r (m1,m2) = null matchD
    where
        (_,d1) = RW.poc m1 (left l) --get only the morphism D2 to G
        l2TOd1 = MT.matches MT.ALL (domain m2) (domain d1)
        matchD = filter (\x -> m2 == compose x d1) l2TOd1

-- | Rule @l@ causes a delete-use conflict with @r@ if rule @l@ deletes something that is used by @r@
-- @(m1,m2)@ is a pair of matches on the same graph
{-deleteUse :: GraphRule a b -> GraphRule a b
          -> (TypedGraphMorphism a b,TypedGraphMorphism a b)
          -> Bool
deleteUse l r (m1,m2) = True `elem` (nods ++ edgs)
    where
        nods = deleteUseAux l m1 m2 applyNodeTGM nodesDomain nodesCodomain
        edgs = deleteUseAux l m1 m2 applyEdgeTGM edgesDomain edgesCodomain

-- | Verify if some element in a graph is deleted by @l@ and is in the match of @r@
deleteUseAux :: Eq t => GraphRule a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
                     -> (TypedGraphMorphism a b -> t -> Maybe t)
                     -> (TypedGraphMorphism a b -> [t]) --domain
                     -> (TypedGraphMorphism a b -> [t]) --codomain
                     -> [Bool]
deleteUseAux l m1 m2 apply dom cod = map (\x -> delByLeft x && isInMatchRight x) (cod m1)
    where
        delByLeft = ruleDeletes (left l) m1 apply dom
        isInMatchRight n = Data.Maybe.isJust (apply (invertTGM m2) n)-}

---- Produce Edge Delete Node

allProdEdgeDelNode :: Bool -> Bool
                   -> GraphRule a b
                   -> GraphRule a b
                   -> [CriticalPair a b]
allProdEdgeDelNode nacInj i l r = map (\(m1,m2) -> CriticalPair m1 m2 Nothing ProduceEdgeDeleteNode) conflictPairs
    where
        pairs = createPairsCodomain (left l) (left r)
        inj = filter checkMono pairs --check injective
        gluing = filter (\(m1,m2) -> satsGluingNacsBoth nacInj i (l,m1) (r,m2)) (if i then inj else pairs) --filter the pairs that not satisfie gluing conditions of L and R
        conflictPairs = filter (prodEdgeDelNode l r) gluing

prodEdgeDelNode :: GraphRule a b -> GraphRule a b -> (TypedGraphMorphism a b,TypedGraphMorphism a b) -> Bool
--prodEdgeDelNode l r (m1,m2) = (not (null matchD)) && (not (satsIncEdges r m2') || not (satsDelItems r m2'))
prodEdgeDelNode l r (m1,m2) = not (null matchD) && not (satsIncEdges (left r) m2')
    where
        (k,d1) = RW.poc m1 (left l)
        l2TOd1 = MT.matches MT.ALL (domain m2) (domain d1)
        matchD = filter (\x -> m2 == compose x d1) l2TOd1
        (m1',r') = RW.po k (right l)
        m2' = compose (head matchD) r' --matchD is unique if exists

---- Produce-Forbid

-- | All ProduceForbid caused by the derivation of @l@ before @r@
-- rule @l@ causes a produce-forbid conflict with @r@ if some NAC in @r@ fails to be satisfied after the aplication of @l@
allProduceForbid :: Bool -> Bool
                 -> GraphRule a b
                 -> GraphRule a b
                 -> [CriticalPair a b]
allProduceForbid nacInj inj l r = concatMap (produceForbidOneNac nacInj inj l inverseLeft r) (zip (nacs r) [0..])
  where
    inverseLeft = inverse inj l

-- | Check ProduceForbid for a NAC @n@ in @r@
produceForbidOneNac :: Bool -> Bool
                    -> GraphRule a b -> GraphRule a b -> GraphRule a b
                    -> (TypedGraphMorphism a b,Int)
                    -> [CriticalPair a b]
produceForbidOneNac nacInj inj l inverseLeft r (n,idx) = let
        -- Consider for a NAC n (L2 -> N2) of r any jointly surjective
        -- pair of morphisms (h1: R1 -> P1, q21: N2 -> P1) with q21 (part)inj
        pairs = createPairsCodomain (right l) n

        filtFun = if nacInj then monomorphism else partialInjectiveTGM n
        filtMono = filter (\(_,q) -> filtFun q) pairs --(h1,q21)

        -- Check gluing cond for (h1,r1). Construct PO complement D1.
        -- Construct PO K and abort if m1 not sats NACs l
        filtPairs = filter (\(h1,_) -> satsGluingAndNacs nacInj False inverseLeft h1) filtMono
        
        dpo = map (\(h1,q21) ->
                    let (k,r') = RW.poc h1 (right l)
                        (m1,l') = RW.po k (left l) in
                      (h1,q21,k,r',m1,l'))
                  filtPairs --(h1,q21,k,r',m1,l')

        filtM1 = filter (\(_,_,_,_,m1,_) -> (not inj || monomorphism m1)
                                          && satsNacs nacInj l m1) dpo

        --  Check existence of h21: L2 -> D1 st. e1 . h21 = q21 . n2
        h21 = concatMap (\(h1,q21,k,r',m1,l') ->
                  let hs = MT.matches MT.ALL (domain n) (codomain k)
                      list = map (\h -> compose h r' == compose n q21) hs in
                       case elemIndex True list of
                           Just ind -> [(h1,q21,k,r',m1,l',hs!!ind)]
                           Nothing  -> [])
                    --if null hs then [] else [(h1,q21,k,r',m1,l',hs)])
                  filtM1 --(h1,q21,k,r',m1,l1,[hs])

        -- Define m2 = d1 . h21: L2 -> K and abort if m2 not sats NACs r
        m1m2 = map (\(h1,q21,k,r',m1,l',l2d1) -> (h1,m1, compose l2d1 l')) h21
        --filtM2 = filter (\(m1,m2) -> satsNacs r m2) m1m2

        -- Check gluing condition for m2 and r
        filtM2 = filter (\(_,_,m2) -> (not inj || monomorphism m2)
                                    && satsGluingAndNacs nacInj inj r m2) m1m2

        in map (\(h1,m1,m2) -> CriticalPair h1 m2 (Just idx) ProduceForbid) filtM2
