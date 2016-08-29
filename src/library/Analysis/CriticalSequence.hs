module Analysis.CriticalSequence
 ( CriticalSequenceType (..),
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
   getCriticalSequenceMatches,
   getCriticalSequenceComatches,
   getCSNac,
   getCSNacIdx,
   getCS
   ) where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Data.Maybe                 (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CriticalSequenceType =
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
-- p1   = production (L1,K1,R1,[N1]) (N1 from L1)
--
-- invLeft = production (R1,K1,L1,[N1]) (N1 from R1)
--
-- p2  = production (L2,K2,R2,[N2])
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
    matches          :: Maybe (m, m),
    comatches        :: (m, m),
    nac              :: Maybe (m, Int), --if is DeleteForbid or ForbidProduce, here is the index of the nac
    csType           :: CriticalSequenceType
    } deriving (Eq,Show)

-- | Returns the matches (m1, m2)
getCriticalSequenceMatches :: CriticalSequence m -> Maybe (m, m)
getCriticalSequenceMatches = matches

-- | Returns the comatches (m1', m2')
getCriticalSequenceComatches :: CriticalSequence m -> (m, m)
getCriticalSequenceComatches = comatches

-- | Returns the type of a 'CriticalSequence'
getCS :: CriticalSequence m -> CriticalSequenceType
getCS = csType

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
triggeredCriticalSequences :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalSequence m]
triggeredCriticalSequences config p1 p2 =
  allProdUseAndDang config p1 p2 ++
  allDeleteForbid config p1 p2

-- | All Critical Sequences
criticalSequences :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
criticalSequences config p1 p2 =
  allProdUseAndDang config p1 p2 ++
  allDeleteForbid config p1 p2 ++
  allDeliverDelete config p1 p2 ++
  allDeliverDangling config p1 p2 ++
  allForbidProduce config p1 p2

-- ** Triggered Dependencies

-- It occurs when p1 enables p2.
-- (ProduceUse, RemoveDangling, DeleteForbid)

-- *** ProduceUse

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @p1@ causes a produce-use dependency with @p2@
-- if rule @p1@ creates something that is used by @p2@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
allProduceUse :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allProduceUse config p1 p2 =
  map
    (\m -> CriticalSequence Nothing m Nothing ProduceUse)
    prodUse
  where
    invLeft = invertProduction config p1
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS p2)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (p2,m2'))
        pairs
    prodUse = filter (isDeleteUse config invLeft) gluing

-- *** RemoveDangling

-- | All RemoveDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a remove-dangling dependency with @p2@
-- if rule @p1@ deletes something that enables @p2@.
allRemoveDangling :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allRemoveDangling config p1 p2 =
  map
    (\m -> CriticalSequence Nothing m Nothing RemoveDangling)
    remDang
  where
    invLeft = invertProduction config p1
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS p2)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (invLeft,m1) (p2,m2))
        pairs
    remDang = filter (produceDangling config invLeft p2) gluing

-- ProduceUse and RemoveDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
allProdUseAndDang :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allProdUseAndDang config p1 p2 =
  map
    (\x -> case x of
      (Left m) -> CriticalSequence Nothing m Nothing ProduceUse
      (Right m) -> CriticalSequence Nothing m Nothing RemoveDangling)
    dependencies
  where
    invLeft = invertProduction config p1
    pairs = createPairsCodomain (matchRestriction config) (getLHS invLeft) (getLHS p2)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (invLeft,m1) (p2,m2))
        pairs
    dependencies = mapMaybe (deleteUseDangling config invLeft p2) gluing

-- *** DeleteForbid

-- | All DeleteForbid caused by the derivation of @p1@ before @r@.
-- Rule @p1@ causes a delete-forbid dependency with @p2@ if
-- some NAC in @p2@ turns satisfied after the aplication of @p1@
allDeleteForbid :: (DPO m, EpiPairs m) => DPOConfig -> Production m -> Production m -> [CriticalSequence m]
allDeleteForbid config p1 p2 =
  concatMap
    (deleteForbid config p1 inverseLeft p2)
    (zip (getNACs p2) [0..])
  where
    inverseLeft = invertProduction config p1

-- | Check DeleteForbid for a NAC @n@ in @p2@
deleteForbid :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
deleteForbid config p1 inverseLeft p2 nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) DeleteForbid)
    (produceForbidOneNac config inverseLeft p2 nac)

-- ** Irreversible Dependencies

-- It occurs when applying p1 and p2, p1 invertProduction cannot be applied.
-- Capture cases of two rules only can be applied in a prefixed order.
-- (DeliverDelete, DeliverDangling, ForbidProduce)

-- *** DeliverDelete

-- | All DeliverDelete caused by the derivation of @p1@ before @r@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ deletes something that is used by @p2@,
-- Verify the non existence of h12: L1 -> D2 such that d2 . h12 = m1'.
allDeliverDelete :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allDeliverDelete config p1 p2 =
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDelete)
    delDel
  where
    invLeft = invertProduction config p1
    pairs = createPairsCodomain (matchRestriction config) (getRHS p1) (getLHS p2)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (p2,m2'))
        pairs
    delDel = filter (\(m1,m2) -> isDeleteUse config p2 (m2,m1)) gluing

-- *** DeliverDangling

-- | All DeliverDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ creates something that disables the inverse of @p1@.
allDeliverDangling :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allDeliverDangling config p1 p2 =
  map
    (\m -> CriticalSequence Nothing m Nothing DeliverDangling)
    delDang
  where
    invLeft = invertProduction config p1
    pairs = createPairsCodomain (matchRestriction config) (getRHS p1) (getLHS p2)
    gluing =
      filter
        (\(m1',m2') -> satisfyRewritingConditions config (invLeft,m1') (p2,m2'))
        pairs
    delDang =
      filter
        (\(m1,m2) -> produceDangling config p2 invLeft (m2,m1))
        gluing

-- TODO: DeliverDelete and DeliverDangling together

-- *** ForbidProduce

-- | All ForbidProduce caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a forbid-produce dependency with @p2@ if some
-- NAC in right of @p1@ turns satisfied after the aplication of @p2@.
allForbidProduce :: (DPO m, EpiPairs m) => DPOConfig
  -> Production m -> Production m -> [CriticalSequence m]
allForbidProduce config p1 p2 =
  concatMap
    (forbidProduce config inverseLeft inverseRight p2)
    (zip (getNACs inverseLeft) [0..])
    where
      inverseLeft = invertProduction config p1
      inverseRight = invertProduction config p2

-- | Check ForbidProduce for a NAC @n@ in right of @p1@
forbidProduce :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
forbidProduce config inverseLeft inverseRight p2 nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) ForbidProduce)
    (produceForbidOneNac config p2 inverseLeft nac)
