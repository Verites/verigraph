{-# LANGUAGE TupleSections #-}
module Analysis.CriticalSequence
 ( CriticalSequenceType (..),
   CriticalSequence,
   getCriticalSequenceMatches,
   getCriticalSequenceComatches,
   getNacMatchOfCriticalSequence,
   getNacIndexOfCriticalSequence,
   getCriticalSequenceType,

   -- * Finding Critical Sequences
   findTriggeredCriticalSequences,
   findCriticalSequences,
   namedCriticalSequences,
   findAllProduceUse,
   findAllRemoveDangling,
   findAllDeleteForbid,
   findAllDeliverDelete,
   findAllForbidProduce,
   findAllDeliverDangling,

   findAllProduceUseAndRemoveDangling,
   findAllDeliverDeleteAndDeliverDangling,
   ) where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO                   hiding (calculateComatch)
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Analysis.CriticalPairs                   (findPotentialCriticalPairs)

-- | Data representing the type of a 'CriticalPair'
data CriticalSequenceType =
    ProduceUse      -- ^ resp. delete-use
  | RemoveDangling  -- ^ resp. produce-dangling
  | DeleteForbid    -- ^ resp. produce-forbid
  | DeliverDelete   -- ^ resp. inverted delete-use
  | DeliverDangling -- ^ resp. inverted produce-dangling
  | ForbidProduce   -- ^ resp. inverted produce-forbid
  deriving (Eq,Show)

type NamedRule cat morph = (String, Production cat morph)
type NamedCriticalPairs morph = (String,String,[CriticalSequence morph])

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

-- | Returns the Critical Sequences with rule names
namedCriticalSequences :: (EM'PairFactorizable cat morph, DPO cat morph) => [NamedRule cat morph] -> cat [NamedCriticalPairs morph]
namedCriticalSequences rules = mapM (uncurry findCriticalSequences') [ (a,b) | a <- rules, b <- rules ]
  where findCriticalSequences' (n1,r1) (n2,r2) = (n1, n2,) <$> findCriticalSequences r1 r2

-- | Given two productions @p1@ and @p2@, finds the Critical sequences
-- in which the application of @p1@ enables the application of @p2@
findTriggeredCriticalSequences :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findTriggeredCriticalSequences p1 p2 = (++) <$> findAllProduceUseAndRemoveDangling p1 p2 <*> findAllDeleteForbid p1 p2

-- | Given two productions @p1@ and @p2@, it finds all Critical Sequences of @p1@ and @p2@ (in this order)
findCriticalSequences :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findCriticalSequences p1 p2 = concat <$> sequence
    [ findAllProduceUseAndRemoveDangling p1 p2
    , findAllDeleteForbid p1 p2
    , findAllDeliverDeleteAndDeliverDangling p1 p2
    , findAllForbidProduce p1 p2 ]

-- ** Triggering Dependencies

-- It occurs when p1 enables p2.
-- (ProduceUse, RemoveDangling, DeleteForbid)

-- *** ProduceUse

-- | All ProduceUse caused by the derivation of @l@ before @r@.
--
-- Rule @p1@ causes a produce-use dependency with @p2@
-- if rule @p1@ creates something that is used by @p2@.
-- Verify the non existence of h21: L2 -> D1 such that d1 . h21 = m2'.
findAllProduceUse :: (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllProduceUse p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p1' p2
  dependencies <- filterM (isDeleteUse p1') matchPairs
  return [ CriticalSequence Nothing m Nothing ProduceUse | m <- dependencies ]

-- *** RemoveDangling

-- | All RemoveDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a remove-dangling dependency with @p2@
-- if rule @p1@ deletes something that enables @p2@.
findAllRemoveDangling :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllRemoveDangling p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p1' p2
  dependencies <- filterM (isProduceDangling p1' p2) matchPairs
  return [ CriticalSequence Nothing m Nothing RemoveDangling | m <- dependencies ]

-- ProduceUse and RemoveDangling

-- | Tests ProduceUse and RemoveDangling for the same pairs,
-- more efficient than deal separately.
findAllProduceUseAndRemoveDangling :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllProduceUseAndRemoveDangling p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p1' p2
  dependencies <- mapM (deleteUseDangling p1' p2) matchPairs
  return [ categorizeDependency x | Just x <- dependencies ]
  where
    categorizeDependency x = case x of
      (Left m)  -> CriticalSequence Nothing m Nothing ProduceUse
      (Right m) -> CriticalSequence Nothing m Nothing RemoveDangling

-- *** DeleteForbid

-- | All DeleteForbid caused by the derivation of @p1@ before @r@.
-- Rule @p1@ causes a delete-forbid dependency with @p2@ if
-- some NAC in @p2@ turns satisfied after the aplication of @p1@
findAllDeleteForbid :: (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllDeleteForbid p1 p2 = do
  p1' <- invertProduction p1
  dependencies <- mapM (produceForbidOneNac p1' p2) $ zip (nacs p2) [0..]
  return [ CriticalSequence (Just m) m' (Just nac) DeleteForbid
             | (m', m, nac) <- concat dependencies ]

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
findAllDeliverDelete :: (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllDeliverDelete p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p2 p1'
  dependencies <- filterM (isDeleteUse p2) matchPairs
  return [ CriticalSequence Nothing m Nothing DeliverDelete | m <- dependencies ]

-- *** DeliverDangling

-- | All DeliverDangling caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a deliver-delete dependency with @p2@ if
-- rule @p2@ creates something that disables the inverse of @p1@.
findAllDeliverDangling :: (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllDeliverDangling p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p2 p1'
  dependencies <- filterM (isProduceDangling p2 p1') matchPairs
  return [ CriticalSequence Nothing m Nothing DeliverDangling | m <- dependencies ]

-- DeliverDelete and DeliverDangling

-- | Tests DeliverDelete and DeliverDangling for the same overlapping pairs
findAllDeliverDeleteAndDeliverDangling :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllDeliverDeleteAndDeliverDangling p1 p2 = do
  p1' <- invertProduction p1
  matchPairs <- findPotentialCriticalPairs p2 p1'
  dependencies <- mapM (deleteUseDangling p2 p1') matchPairs
  return [ categorizeDependency x | Just x <- dependencies ]
  where
    categorizeDependency x = case x of
      (Left m)  -> CriticalSequence Nothing m Nothing DeliverDelete
      (Right m) -> CriticalSequence Nothing m Nothing DeliverDangling

-- *** ForbidProduce

-- | All ForbidProduce caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a forbid-produce dependency with @p2@ if some
-- NAC in right of @p1@ turns satisfied after the aplication of @p2@.
findAllForbidProduce :: (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [CriticalSequence morph]
findAllForbidProduce p1 p2 = do
  p1' <- invertProduction p1
  conflicts <- mapM (produceForbidOneNac p1' p2) (zip (nacs p1') [0..])
  return [ CriticalSequence (Just matches) comatches (Just nac) ForbidProduce
            | (matches, comatches, nac) <- concat conflicts ]

