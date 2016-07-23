module Analysis.CriticalSequence
 ( CS (..),
   CriticalSequence,
   criticalSequences,
   namedCriticalSequences,
   allProduceUse,
   allRemoveDangling,
   allProdUseAndDang,
   allDeleteForbid,
   allDeliverDelete,
   allForbidProduce,
   allDeliverDangling,
   getMatch,
   getComatch,
   getCSNac,
   getCSNacIdx,
   getCS
   ) where

import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW hiding (comatch)
import           Analysis.DiagramAlgorithms
import           Data.Maybe                (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CS =
    DeliverDelete
  | ForbidProduce
  | DeliverDangling
  | ProduceUse
  | DeleteForbid
  | RemoveDangling
  deriving (Eq,Show)

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
    nac :: Maybe (m, Int), --if is DeleteForbid, here is the index of the nac
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
namedCriticalSequences :: (EpiPairs m, DPO m) => DPOConfig
  -> [(String, Production m)] -> [(String, String, [CriticalSequence m])]
namedCriticalSequences config r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences nacInj inj r1 r2)

-- | Create all jointly epimorphic pairs of morphisms from the codomains of
-- the given morphisms.
-- The flag indicates only monomorphic morphisms.
createPairsCodomain :: (EpiPairs m) => MatchRestriction -> m -> m -> [(m, m)]
createPairsCodomain inj m1 m2 = createPairs (inj == MonoMatches) (codomain m1) (codomain m2)

-- | All Critical Sequences
criticalSequences :: (EpiPairs m, DPO m) => DPOConfig
                  -> Production m -> Production m -> [CriticalSequence m]
criticalSequences nacInj inj l r =
  allProdUseAndDang nacInj inj l r ++
  allDeliverDangling nacInj inj l r ++
  allDeleteForbid nacInj inj l r ++
  allDeliverDelete nacInj inj l r ++
  allForbidProduce nacInj inj l r

-- Triggered Dependencies (ProduceUse, DeleteForbid, RemoveDangling)

---- ProduceUse

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @l@ causes a produce-use dependency with @r@ if rule @l@ creates
-- something that is used by @r@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
allProduceUse :: (DPO m, EpiPairs m) => DPOConfig
              -> Production m -> Production m -> [CriticalSequence m]
allProduceUse config l r =
  map
    (\m -> CriticalSequence Nothing m Nothing ProduceUse)
    prodUse
  where
    invLeft = inverse config l
    pairs = createPairsCodomain config (left invLeft) (left r)
    gluing = filter (\(m1',m2') -> satsGluingNacsBoth config (invLeft,m1') (r,m2')) pairs
    prodUse = filter (deleteUse config invLeft) gluing

---- RemoveDangling

allRemoveDangling :: (EpiPairs m, DPO m) => DPOConfig
                  -> Production m -> Production m -> [CriticalSequence m]
allRemoveDangling config l r =
  map
    (\m -> CriticalSequence Nothing m Nothing RemoveDangling)
    remDang
  where
    invLeft = inverse config l
    pairs = createPairsCodomain config (left invLeft) (left r)
    gluing = filter (\(m1,m2) -> satsGluingNacsBoth config (invLeft,m1) (r,m2)) pairs
    remDang = filter (produceDangling config invLeft r) gluing

---- ProduceUse and RemoveDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
allProdUseAndDang :: (EpiPairs m, DPO m) => DPOConfig
                  -> Production m -> Production m -> [CriticalSequence m]
allProdUseAndDang config l r =
  let dependencies = mapMaybe (deleteUseDangling config invLeft r) gluing
  in map
      (\x -> case x of
        (Left m) -> CriticalSequence Nothing m Nothing ProduceUse
        (Right m) -> CriticalSequence Nothing m Nothing RemoveDangling)
      dependencies
  where
    invLeft = inverse config l
    pairs = createPairsCodomain config (left invLeft) (left r)
    gluing = filter (\(m1,m2) -> satsGluingNacsBoth config (invLeft,m1) (r,m2)) pairs

---- DeleteForbid

-- | All DeleteForbid caused by the derivation of @l@ before @r@
-- rule @l@ causes a delete-forbid dependency with @r@ if some NAC in @r@ turns satisfied after the aplication of @l@
allDeleteForbid :: (DPO m, EpiPairs m) => DPOConfig
                 -> Production m -> Production m -> [CriticalSequence m]
allDeleteForbid config l r = concatMap (deleteForbid nacInj inj l inverseLeft r) (zip (nacs r) [0..])
  where
    inverseLeft = inverse config l

-- | Check DeleteForbid for a NAC @n@ in @r@
deleteForbid :: (EpiPairs m, DPO m) => DPOConfig -> Production m
              -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
deleteForbid config l inverseLeft r nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) DeleteForbid)
    (produceForbidOneNac config inverseLeft l r nac)

-- Irreversible Dependencies (DeliverDelete, ForbidProduce)

---- DeliverDelete

-- | All DeliverDelete caused by the derivation of @l@ before @r@.
--
-- Rule @l@ causes a deliver-delete dependency with @r@ if rule @l@ deletes
-- something that is used by @r@, disabling the reverse of the @l@ to be applicated.
-- Verify the non existence of h12: L1 -> D2 such that d2 . h12 = m1'.
allDeliverDelete :: (DPO m, EpiPairs m) => NacSatisfaction -> MatchRestriction
                 -> Production m -> Production m -> [CriticalSequence m]
allDeliverDelete nacInj i l r = 
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDelete)
    delDel
  where
    invLeft = inverse nacInj i l
    pairs = createPairsCodomain i (right l) (left r)
    gluing = filter (\(m1',m2') -> satsGluingNacsBoth nacInj i (invLeft,m1') (r,m2')) pairs
    delDel = filter (\(m1,m2) -> deleteUse i r (m2,m1)) gluing

---- DeliverDangling

-- | All DeliverDangling caused by the derivation of @l@ before @r@.
--
-- Rule @l@ causes a deliver-delete dependency with @r@ if rule @l@ deletes
-- something that is used by @r@, disabling the reverse of the @l@ to be applicated.
-- Verify the non existence of h12: L1 -> D2 such that d2 . h12 = m1'.
allDeliverDangling :: (DPO m, EpiPairs m) => NacSatisfaction -> MatchRestriction
                   -> Production m -> Production m -> [CriticalSequence m]
allDeliverDangling nacInj i l r = 
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDangling)
    delDang
  where
    invLeft = inverse nacInj i l
    pairs = createPairsCodomain i (right l) (left r)
    gluing = filter (\(m1',m2') -> satsGluingNacsBoth nacInj i (invLeft,m1') (r,m2')) pairs
    delDang = filter (\(m1,m2) -> produceDangling nacInj i r invLeft (m2,m1)) gluing

---- ForbidProduce

-- | All ForbidProduce caused by the derivation of @l@ before @r@
-- rule @l@ causes a forbid-produce dependency with @r@ if
-- some NAC in right of @l@ turns satisfied after the aplication of @r@
allForbidProduce :: (DPO m, EpiPairs m) => NacSatisfaction -> MatchRestriction
                 -> Production m -> Production m -> [CriticalSequence m]
allForbidProduce nacInj inj l r =
  concatMap
    (forbidProduce nacInj inj inverseLeft inverseRight r)
    (zip (nacs inverseLeft) [0..])
    where
      inverseLeft = inverse nacInj inj l
      inverseRight = inverse nacInj inj r

-- | Check ForbidProduce for a NAC @n@ in @l@
forbidProduce :: (EpiPairs m, DPO m) => NacSatisfaction -> MatchRestriction -> Production m
              -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
forbidProduce nacInj inj inverseLeft inverseRight r nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) ForbidProduce)
    (produceForbidOneNac nacInj inj r inverseRight inverseLeft nac)
