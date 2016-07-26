module Analysis.CriticalSequence
 ( CS (..),
   CriticalSequence,
   criticalSequences,
   namedCriticalSequences,
   allProduceUse,
   allRemoveDangling,
   allProdUseAndDang,
   allDeliverDelete,
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
data CS = ProduceUse | DeliverDelete | RemoveDangling deriving (Eq,Show)

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
namedCriticalSequences :: (EpiPairs m, DPO m) => DPOConfig
  -> [(String, Production m)] -> [(String, String, [CriticalSequence m])]
namedCriticalSequences config r = map (uncurry getCPs) [(a,b) | a <- r, b <- r]
  where
    getCPs (n1,r1) (n2,r2) = (n1, n2, criticalSequences config r1 r2)

-- | All Critical Sequences
criticalSequences :: (EpiPairs m, DPO m) => DPOConfig
                  -> Production m -> Production m -> [CriticalSequence m]
criticalSequences config l r =
  allProdUseAndDang config l r ++
  allDeliverDelete config l r

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

---- DeliverDelete

-- | All DeliverDelete caused by the derivation of @l@ before @r@
-- rule @l@ causes a deliver-forbid conflict with @r@ if some NAC in @r@ turns satisfied after the aplication of @l@
allDeliverDelete :: (DPO m, EpiPairs m) => DPOConfig
                 -> Production m -> Production m -> [CriticalSequence m]
allDeliverDelete config l r = concatMap (deliverDelete config l inverseLeft r) (zip (nacs r) [0..])
  where
    inverseLeft = inverse config l

-- | Check DeliverDelete for a NAC @n@ in @r@
deliverDelete :: (EpiPairs m, DPO m) => DPOConfig -> Production m
              -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
deliverDelete config l inverseLeft r nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) DeliverDelete)
    (produceForbidOneNac config inverseLeft l r nac)


-- | Create all jointly epimorphic pairs of morphisms from the codomains of
-- the given morphisms.
-- The flag indicates only monomorphic morphisms.
createPairsCodomain :: (EpiPairs m) => DPOConfig -> m -> m -> [(m, m)]
createPairsCodomain config m1 m2 =
  createPairs (matchRestriction config == MonoMatches) (codomain m1) (codomain m2)
