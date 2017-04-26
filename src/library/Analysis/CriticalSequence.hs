module Analysis.CriticalSequence
 ( CriticalSequenceType (..),
   CriticalSequence,
   getCriticalSequenceMatches,
   getCriticalSequenceComatches,
   getNacMatchOfCriticalSequence,
   getNacIndexOfCriticalSequence,
   getCriticalSequenceType,

   -- * Finding Critical Sequences
   findTriggeringCriticalSequences,
   namedCriticalSequences,
   findAllProduceUse,
   findAllRemoveDangling,
   findAllDeleteForbid,
   findAllDeliverDelete,
   findAllForbidProduce,
   findAllDeliverDangling,
   findAllProduceUseAndRemoveDangling,
   ) where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.CriticalPairs     (findPotentialCriticalPairs)
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

type NamedRule m = (String, Production m)
type NamedCriticalPairs m = (String,String,[CriticalSequence m])

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
    matches   :: Maybe (m, m),
    comatches :: (m, m),
    nac       :: Maybe (m, Int), --if is DeleteForbid or ForbidProduce, here is the index of the nac
    csType    :: CriticalSequenceType
    } deriving (Eq,Show)

-- | Returns the matches (m1, m2)
getCriticalSequenceMatches :: CriticalSequence m -> Maybe (m, m)
getCriticalSequenceMatches = matches

-- | Returns the comatches (m1', m2')
getCriticalSequenceComatches :: CriticalSequence m -> (m, m)
getCriticalSequenceComatches = comatches

-- | Returns the type of a 'CriticalSequence'
getCriticalSequenceType :: CriticalSequence m -> CriticalSequenceType
getCriticalSequenceType = csType

-- | Returns the nac match of a 'CriticalSequence'
getNacMatchOfCriticalSequence :: CriticalSequence m -> Maybe m
getNacMatchOfCriticalSequence cs =
  case nac cs of
    Just (nac,_) -> Just nac
    Nothing      -> Nothing

-- | Returns the nac index of a 'CriticalSequence'
getNacIndexOfCriticalSequence :: CriticalSequence m -> Maybe Int
getNacIndexOfCriticalSequence cs =
  case nac cs of
    Just (_,idx) -> Just idx
    Nothing      -> Nothing

-- | Returns the Critical Sequences with rule names
namedCriticalSequences :: (EpiPairs m, DPO m) => MorphismsConfig -> [NamedRule m] -> [NamedCriticalPairs m]
namedCriticalSequences conf rules =
  map (uncurry getCSs) [(a,b) | a <- rules, b <- rules]
  where
    getCSs (n1,r1) (n2,r2) = (n1, n2, findCriticalSequences conf r1 r2)

-- | Given two productions @p1@ and @p2@, finds the Critical sequences in which the application of @p1@
-- enables the application of @p2@
findTriggeringCriticalSequences :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findTriggeringCriticalSequences conf p1 p2 =
  findAllProduceUseAndRemoveDangling conf p1 p2 ++
  findAllDeleteForbid conf p1 p2

-- TODO : verify if this doc is right
-- | Given two productions @p1@ and @p2@, finds all the Critical Sequences of @p1@ and @p2@ in this order
findCriticalSequences :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findCriticalSequences conf p1 p2 =
  findAllProduceUseAndRemoveDangling conf p1 p2 ++
  findAllDeleteForbid conf p1 p2 ++
  findAllDeliverDelete conf p1 p2 ++
  findAllDeliverDangling conf p1 p2 ++
  findAllForbidProduce conf p1 p2

-- ** Triggering Dependencies

-- It occurs when p1 enables p2.
-- (ProduceUse, RemoveDangling, DeleteForbid)

-- *** ProduceUse

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @p1@ causes a produce-use dependency with @p2@
-- if rule @p1@ creates something that is used by @p2@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
findAllProduceUse :: (DPO m, EpiPairs m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllProduceUse conf p1 p2 =
  map (\m -> CriticalSequence Nothing m Nothing ProduceUse) prodUse
  where
    p1' = invertProduction conf p1
    gluing = findPotentialCriticalPairs conf p1' p2
    prodUse = filter (isDeleteUse conf p1') gluing

-- *** RemoveDangling

-- | All RemoveDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a remove-dangling dependency with @p2@
-- if rule @p1@ deletes something that enables @p2@.
findAllRemoveDangling :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllRemoveDangling conf p1 p2 =
  map (\m -> CriticalSequence Nothing m Nothing RemoveDangling) remDang
  where
    p1' = invertProduction conf p1
    gluing = findPotentialCriticalPairs conf p1' p2
    remDang = filter (isProduceDangling conf p1' p2) gluing

-- ProduceUse and RemoveDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
findAllProduceUseAndRemoveDangling :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllProduceUseAndRemoveDangling conf p1 p2 =
  map categorizeDependency dependencies
  where
    p1' = invertProduction conf p1
    gluing = findPotentialCriticalPairs conf p1' p2
    dependencies = mapMaybe (deleteUseDangling conf p1' p2) gluing
    categorizeDependency x = case x of
      (Left m)  -> CriticalSequence Nothing m Nothing ProduceUse
      (Right m) -> CriticalSequence Nothing m Nothing RemoveDangling

-- *** DeleteForbid

-- | All DeleteForbid caused by the derivation of @p1@ before @r@.
-- Rule @p1@ causes a delete-forbid dependency with @p2@ if
-- some NAC in @p2@ turns satisfied after the aplication of @p1@
findAllDeleteForbid :: (DPO m, EpiPairs m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllDeleteForbid conf p1 p2 =
  concatMap (findDeleteForbidForNAC conf p1' p2) (zip (getNACs p2) [0..])
  where
    p1' = invertProduction conf p1

-- | Check DeleteForbid for a NAC @n@ in @p2@
findDeleteForbidForNAC :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
findDeleteForbidForNAC conf p1' p2 nac =
  map
    (\(m',m,nac) -> CriticalSequence (Just m) m' (Just nac) DeleteForbid)
    (produceForbidOneNac conf p1' p2 nac)

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
findAllDeliverDelete :: (DPO m, EpiPairs m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllDeliverDelete conf p1 p2 =
  map (\m -> CriticalSequence Nothing m Nothing DeliverDelete) delDel
  where
    p1' = invertProduction conf p1
    gluing = findPotentialCriticalPairs conf p1' p2
    delDel = filter (\(m1,m2) -> isDeleteUse conf p2 (m2,m1)) gluing

-- *** DeliverDangling

-- | All DeliverDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ creates something that disables the inverse of @p1@.
findAllDeliverDangling :: (DPO m, EpiPairs m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllDeliverDangling conf p1 p2 =
  map (\m -> CriticalSequence Nothing m Nothing DeliverDangling) delDang
  where
    p1' = invertProduction conf p1
    gluing = findPotentialCriticalPairs conf p1' p2
    delDang = filter (\(m1,m2) -> isProduceDangling conf p2 p1' (m2,m1)) gluing

-- TODO: DeliverDelete and DeliverDangling together

-- *** ForbidProduce

-- | All ForbidProduce caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a forbid-produce dependency with @p2@ if some
-- NAC in right of @p1@ turns satisfied after the aplication of @p2@.
findAllForbidProduce :: (DPO m, EpiPairs m) => MorphismsConfig -> Production m -> Production m -> [CriticalSequence m]
findAllForbidProduce conf p1 p2 =
  concatMap (findForbidProduceForNAC conf p1' p2) (zip (getNACs p1') [0..])
    where
      p1' = invertProduction conf p1

-- | Check ForbidProduce for a NAC @n@ in right of @p1@
findForbidProduceForNAC :: (EpiPairs m, DPO m) => MorphismsConfig -> Production m -> Production m -> (m, Int) -> [CriticalSequence m]
findForbidProduceForNAC conf p1' p2 nac =
  map
    (\(m,m',nac) -> CriticalSequence (Just m) m' (Just nac) ForbidProduce)
    (produceForbidOneNac conf p2 p1' nac)
