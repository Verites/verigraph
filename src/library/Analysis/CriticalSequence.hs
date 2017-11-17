module Analysis.CriticalSequence
 ( CriticalSequenceType (..),
   CriticalSequence,
   getCriticalSequenceMatches,
   getCriticalSequenceComatches,
   getNacMatchOfCriticalSequence,
   getNacIndexOfCriticalSequence,
   getCriticalSequenceType,
   isProduceUse,
   isRemoveDangling,
   isDeleteForbid,
   isDeliverDelete,
   isDeliverDangling,
   isForbidProduce,

   -- * Finding Critical Sequences
   findTriggeredCriticalSequences,
   findCriticalSequences,
   findAllProduceUse,
   findAllRemoveDangling,
   findAllDeleteForbid,
   findAllDeliverDelete,
   findAllForbidProduce,
   findAllDeliverDangling,

   findAllProduceUseAndRemoveDangling,
   findAllDeliverDeleteAndDeliverDangling,
   ) where

import           Abstract.Category.Finitary
import           Abstract.Constraint
import           Abstract.Rewriting.DPO     hiding (calculateComatch)
import           Analysis.CriticalPairs     hiding (comatches, matches)


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

data CriticalSequence morph = CriticalSequence {
    matches   :: Maybe (morph,morph),
    comatches :: (morph,morph),
    nac       :: Maybe (morph, Int), --if is DeleteForbid or ForbidProduce, here is the index of the nac
    csType    :: CriticalSequenceType
    } deriving (Eq,Show)


isProduceUse :: CriticalSequence morph -> Bool
isProduceUse (CriticalSequence _ _ _ ProduceUse) = True
isProduceUse _                                   = False

isRemoveDangling :: CriticalSequence morph -> Bool
isRemoveDangling (CriticalSequence _ _ _ RemoveDangling) = True
isRemoveDangling _                                       = False

isDeleteForbid :: CriticalSequence morph -> Bool
isDeleteForbid (CriticalSequence _ _ _ DeleteForbid) = True
isDeleteForbid _                                     = False

isDeliverDelete :: CriticalSequence morph -> Bool
isDeliverDelete (CriticalSequence _ _ _ DeliverDelete) = True
isDeliverDelete _                                      = False

isDeliverDangling :: CriticalSequence morph -> Bool
isDeliverDangling (CriticalSequence _ _ _ DeliverDangling) = True
isDeliverDangling _                                        = False

isForbidProduce :: CriticalSequence morph -> Bool
isForbidProduce (CriticalSequence _ _ _ ForbidProduce) = True
isForbidProduce _                                      = False

-- | Returns the matches (m1, m2)
getCriticalSequenceMatches :: CriticalSequence morph -> Maybe (morph,morph)
getCriticalSequenceMatches = matches

-- | Returns the comatches (m1', m2')
getCriticalSequenceComatches :: CriticalSequence morph -> (morph,morph)
getCriticalSequenceComatches = comatches

-- | Returns the type of a 'CriticalSequence'
getCriticalSequenceType :: CriticalSequence morph -> CriticalSequenceType
getCriticalSequenceType = csType

-- | Returns the nac match of a 'CriticalSequence'
getNacMatchOfCriticalSequence :: CriticalSequence morph -> Maybe morph
getNacMatchOfCriticalSequence cs =
  case nac cs of
    Just (nac,_) -> Just nac
    Nothing      -> Nothing

-- | Returns the nac index of a 'CriticalSequence'
getNacIndexOfCriticalSequence :: CriticalSequence morph -> Maybe Int
getNacIndexOfCriticalSequence cs =
  case nac cs of
    Just (_,idx) -> Just idx
    Nothing      -> Nothing

-- | Given two productions @p1@ and @p2@, finds the Critical sequences
-- in which the application of @p1@ enables the application of @p2@
findTriggeredCriticalSequences :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findTriggeredCriticalSequences conf constraints p1 p2 =
  findAllProduceUseAndRemoveDangling conf constraints p1 p2 ++
  findAllDeleteForbid conf constraints p1 p2

-- | Given two productions @p1@ and @p2@, it finds all Critical Sequences of @p1@ and @p2@ (in this order)
findCriticalSequences :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findCriticalSequences conf constraints p1 p2 =
  findAllProduceUseAndRemoveDangling conf constraints p1 p2 ++
  findAllDeleteForbid conf constraints p1 p2 ++
  findAllDeliverDeleteAndDeliverDangling conf constraints p1 p2 ++
  findAllForbidProduce conf constraints p1 p2

-- ** Triggering Dependencies

-- It occurs when p1 enables p2.
-- (ProduceUse, RemoveDangling, DeleteForbid)

-- Equavalent to conflicts between (inverse p1) and p2.

asTriggeringDependency :: (DPO morph) => (MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]) -> MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
asTriggeringDependency f conf constraints p1 p2 = concatMap convertResult $ f conf constraints (invertProduction conf p1) p2
  where
    convertResult (CriticalPair _ _ _ FreeOverlap) = []
    convertResult (CriticalPair matches comatches nac kind) =
      [CriticalSequence comatches matches nac (convertKind kind)]
    convertKind DeleteUse       = ProduceUse
    convertKind ProduceDangling = RemoveDangling
    convertKind ProduceForbid   = DeleteForbid
    convertKind FreeOverlap     = error "asTriggeringDependency: unreachable code"

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @p1@ causes a produce-use dependency with @p2@
-- if rule @p1@ creates something that is used by @p2@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
findAllProduceUse :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllProduceUse = asTriggeringDependency findAllDeleteUse

-- | All RemoveDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a remove-dangling dependency with @p2@
-- if rule @p1@ deletes something that enables @p2@.
findAllRemoveDangling :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllRemoveDangling = asTriggeringDependency findAllProduceDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
findAllProduceUseAndRemoveDangling :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllProduceUseAndRemoveDangling = asTriggeringDependency findAllDeleteUseAndProduceDangling

-- | All DeleteForbid caused by the derivation of @p1@ before @r@.
-- Rule @p1@ causes a delete-forbid dependency with @p2@ if
-- some NAC in @p2@ turns satisfied after the aplication of @p1@
findAllDeleteForbid :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllDeleteForbid = asTriggeringDependency findAllProduceForbid

-- ** Irreversible Dependencies

-- It occurs when applying p1 and p2, p1 invertProduction cannot be applied.
-- Capture cases of two rules only can be applied in a prefixed order.
-- (DeliverDelete, DeliverDangling, ForbidProduce)

-- Equavalent to conflicts between p2 and (inverse p1).

asIrreversibleDependency :: (DPO morph) => (MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]) -> MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
asIrreversibleDependency f conf constraints p1 p2 = concatMap convertResult $ f conf constraints p2 (invertProduction conf p1)
  where
    convertResult (CriticalPair _ _ _ FreeOverlap) = []
    convertResult (CriticalPair matches comatches nac kind) =
      [CriticalSequence comatches matches nac (convertKind kind)]
    convertKind DeleteUse       = DeliverDelete
    convertKind ProduceDangling = DeliverDangling
    convertKind ProduceForbid   = ForbidProduce
    convertKind FreeOverlap     = error "asIrreversibleDependency: unreachable code"

-- | All DeliverDelete caused by the derivation of @p1@ before @r@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ deletes something that is used by @p2@,
-- Verify the non existence of h12: L1 -> D2 such that d2 . h12 = m1'.
findAllDeliverDelete :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllDeliverDelete = asIrreversibleDependency findAllDeleteUse

-- | All DeliverDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ creates something that disables the inverse of @p1@.
findAllDeliverDangling :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllDeliverDangling = asIrreversibleDependency findAllProduceDangling

-- | Tests DeliverDelete and DeliverDangling for the same overlapping pairs
findAllDeliverDeleteAndDeliverDangling :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllDeliverDeleteAndDeliverDangling = asIrreversibleDependency findAllDeleteUseAndProduceDangling

-- | All ForbidProduce caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a forbid-produce dependency with @p2@ if some
-- NAC in right of @p1@ turns satisfied after the aplication of @p2@.
findAllForbidProduce :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalSequence morph]
findAllForbidProduce = asIrreversibleDependency findAllProduceForbid
