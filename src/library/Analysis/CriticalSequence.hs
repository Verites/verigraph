module Analysis.CriticalSequence
 ( CS (..),
   CriticalSequence,
   criticalSequences,
   triggeredCriticalSequences,
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

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Data.Maybe                 (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CS =
    ProduceUse      -- ^ resp. delete-use
  | RemoveDangling  -- ^ resp. produce-dangling
  | DeleteForbid    -- ^ resp. produce-forbid
  | DeliverDelete   -- ^ resp. inverted delete-use
  | DeliverDangling -- ^ resp. inverted produce-dangling
  | ForbidProduce   -- ^ resp. inverted produce-forbid
  deriving (Eq,Show)

-- | A Critical Sequence is defined as two matches (m1,m2) from the
-- left side of their rules to a same graph.
--
-- This diagram shows graphs and morphisms names used in the algorithms below
--
-- pLeft   = production (L1,K1,R1,[N1]) (N1 from L1)
--
-- invLeft = production (R1,K1,L1,[N1]) (N1 from R1)
--
-- pRight  = production (L2,K2,R2,[N2])
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
    match            :: Maybe (m, m),
    calculateComatch :: (m, m),
    nac              :: Maybe (m, Int), --if is DeleteForbid or ForbidProduce, here is the index of the nac
    cs               :: CS
    } deriving (Eq,Show)

-- | Returns the matches (m1, m2)
getMatch :: CriticalSequence m -> Maybe (m, m)
getMatch = match

-- | Returns the comatches (m1', m2')
getComatch :: CriticalSequence m -> (m, m)
getComatch = calculateComatch

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
namedCriticalSequences :: (EpiPairs m, DPO m)
  => DPOConfig -> [(String, Production m)]
  -> [(String, String, [CriticalSequence m])]
namedCriticalSequences config rules =
  map
    (uncurry getCPs)
    [(a,b) | a <- rules, b <- rules]
  where
    getCPs (n1,r1) (n2,r2) =
      (n1, n2, criticalSequences config r1 r2)

-- | Create all jointly epimorphic pairs of morphisms from the codomains of
-- the given morphisms.
-- The flag indicates only monomorphic morphisms.
createPairsCodomain :: (EpiPairs m) => MatchRestriction -> m -> m -> [(m, m)]
createPairsCodomain inj m1 m2 =
  createJointlyEpimorphicPairs (inj == MonoMatches) (codomain m1) (codomain m2)

-- | All Triggered Critical Sequences
triggeredCriticalSequences :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
triggeredCriticalSequences config pLeft pRight =
  allProdUseAndDang config pLeft pRight ++
  allDeleteForbid config pLeft pRight

-- | All Critical Sequences
criticalSequences :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
criticalSequences config pLeft pRight =
  allProdUseAndDang config pLeft pRight ++
  allDeleteForbid config pLeft pRight ++
  allDeliverDelete config pLeft pRight ++
  allDeliverDangling config pLeft pRight ++
  allForbidProduce config pLeft pRight

-- ** Triggered Dependencies

-- It occurs when pLeft enables pRight.
-- (ProduceUse, RemoveDangling, DeleteForbid)

-- *** ProduceUse

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @pLeft@ causes a produce-use dependency with @pRight@
-- if rule @pLeft@ creates something that is used by @pRight@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
allProduceUse :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allProduceUse config pLeft pRight =
  map
    (\m -> CriticalSequence Nothing m Nothing ProduceUse)
    prodUse
  where
    invLeft = invertProduction config pLeft
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS pRight)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (pRight,m2'))
        pairs
    prodUse = filter (isDeleteUse config invLeft) gluing

-- *** RemoveDangling

-- | All RemoveDangling caused by the derivation of @pLeft@ before @pRight@.
--
-- Rule @pLeft@ causes a remove-dangling dependency with @pRight@
-- if rule @pLeft@ deletes something that enables @pRight@.
allRemoveDangling :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allRemoveDangling config pLeft pRight =
  map
    (\m -> CriticalSequence Nothing m Nothing RemoveDangling)
    remDang
  where
    invLeft = invertProduction config pLeft
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS pRight)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (invLeft,m1) (pRight,m2))
        pairs
    remDang = filter (produceDangling config invLeft pRight) gluing

-- ProduceUse and RemoveDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
allProdUseAndDang :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allProdUseAndDang config pLeft pRight =
  map
    (\x -> case x of
      (Left m) -> CriticalSequence Nothing m Nothing ProduceUse
      (Right m) -> CriticalSequence Nothing m Nothing RemoveDangling)
    dependencies
  where
    invLeft = invertProduction config pLeft
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS pRight)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (invLeft,m1) (pRight,m2))
        pairs
    dependencies = mapMaybe (deleteUseDangling config invLeft pRight) gluing

-- *** DeleteForbid

-- | All DeleteForbid caused by the derivation of @pLeft@ before @r@.
-- Rule @pLeft@ causes a delete-forbid dependency with @pRight@ if
-- some NAC in @pRight@ turns satisfied after the aplication of @pLeft@
allDeleteForbid :: (DPO m, EpiPairs m) => DPOConfig -> Production m -> Production m -> [CriticalSequence m]
allDeleteForbid config pLeft pRight =
  concatMap
    (deleteForbid config pLeft inverseLeft pRight)
    (zip (getNACs pRight) [0..])
  where
    inverseLeft = invertProduction config pLeft

-- | Check DeleteForbid for a NAC @n@ in @pRight@
deleteForbid :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
deleteForbid config pLeft inverseLeft pRight nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) DeleteForbid)
    (produceForbidOneNac config inverseLeft pRight nac)

-- ** Irreversible Dependencies

-- It occurs when applying pLeft and pRight, pLeft invertProduction cannot be applied.
-- Capture cases of two rules only can be applied in a prefixed order.
-- (DeliverDelete, DeliverDangling, ForbidProduce)

-- *** DeliverDelete

-- | All DeliverDelete caused by the derivation of @pLeft@ before @r@.
--
-- Rule @pLeft@ causes a deliver-delete dependency with @pRight@ if
-- rule @pRight@ deletes something that is used by @pRight@,
-- Verify the non existence of h12: L1 -> D2 such that d2 . h12 = m1'.
allDeliverDelete :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allDeliverDelete config pLeft pRight =
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDelete)
    delDel
  where
    invLeft = invertProduction config pLeft
    pairs = createPairsCodomain (matchRestriction config) (getRHS pLeft) (getLHS pRight)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (pRight,m2'))
        pairs
    delDel = filter (\(m1,m2) -> isDeleteUse config pRight (m2,m1)) gluing

-- *** DeliverDangling

-- | All DeliverDangling caused by the derivation of @pLeft@ before @pRight@.
--
-- Rule @pLeft@ causes a deliver-delete dependency with @pRight@ if
-- rule @pRight@ creates something that disables the inverse of @pLeft@.
allDeliverDangling :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allDeliverDangling config pLeft pRight =
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDangling)
    delDang
  where
    invLeft = invertProduction config pLeft
    pairs = createPairsCodomain (matchRestriction config) (getRHS pLeft) (getLHS pRight)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (pRight,m2'))
        pairs
    delDang =
      filter
        (\(m1,m2) -> produceDangling config pRight invLeft (m2,m1))
        gluing

-- TODO: DeliverDelete and DeliverDangling together

-- *** ForbidProduce

-- | All ForbidProduce caused by the derivation of @pLeft@ before @pRight@.
--
-- Rule @pLeft@ causes a forbid-produce dependency with @pRight@ if some
-- NAC in right of @pLeft@ turns satisfied after the aplication of @pRight@.
allForbidProduce :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allForbidProduce config pLeft pRight =
  concatMap
    (forbidProduce config inverseLeft inverseRight pRight)
    (zip (getNACs inverseLeft) [0..])
    where
      inverseLeft = invertProduction config pLeft
      inverseRight = invertProduction config pRight

-- | Check ForbidProduce for a NAC @n@ in right of @pLeft@
forbidProduce :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
forbidProduce config inverseLeft inverseRight pRight nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) ForbidProduce)
    (produceForbidOneNac config pRight inverseLeft nac)
