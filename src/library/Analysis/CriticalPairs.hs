module Analysis.CriticalPairs
 ( CriticalPairType (..),
   CriticalPair,
   findCriticalPairs,
   namedCriticalPairs,
   findAllDeleteUse,
   findAllProduceForbid,
   findAllProduceDangling,
   getCriticalPairMatches,
   getCriticalPairComatches,
   getNacIndexOfCriticalPair,
   getNacMatchOfCriticalPair,
   getCriticalPairType,
   isDeleteUse
   ) where

import           Abstract.AdhesiveHLR       as RW
import           Abstract.DPO               as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Analysis.EpimorphicPairs
import           Data.Maybe                 (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CriticalPairType =
    FreeOverlap
  | DeleteUse
  | ProduceForbid
  | ProduceDangling
  deriving(Eq,Show)

type NamedRule m = (String, Production m)
type NamedCriticalPairs m = (String,String,[CriticalPair m])

-- | A Critical Pair is defined as two matches (m1,m2) from the left
-- side of their rules to a same graph.
-- It assumes that the derivation of the rule with match @m1@ causes
-- a conflict with the rule with match @m2@
--
-- This diagram shows graphs and morphisms names used in the algorithms below
--
-- p1 = production (L1,K1,R1,[N1])
--
-- p2 = production (L2,K2,R2,[N2])
--
-- @
--                    N1    N2
--                    ^      ^
--          r     l   │      │n
--     R1◀─────K1────▶L1    L2◀────K2─────▶R2
--     │       │       \\   /       │       │
--  m1'│      k│      m1\\ /m2      │       │
--     ▼       ▼         ▼         ▼       ▼
--     P1◀─────D1───────▶G◀───────D2──────▶P2
--         r'       l'
-- @
--
-- m2' :: from L2 to P1
--
-- h21 :: from L2 to D1
--
-- q21 (nacMatch) :: from N2 to P1

data CriticalPair m = CriticalPair {
    matches          :: (m, m),
    comatches        :: Maybe (m, m),
    nacMatch         :: Maybe (m, Int), --if it is a ProduceForbid, here is the index of the nac
    cpType           :: CriticalPairType
    } deriving (Eq,Show)

-- | Returns the matches (m1,m2)
getCriticalPairMatches :: CriticalPair m -> (m, m)
getCriticalPairMatches = matches

-- | Returns the comatches (m1',m2')
getCriticalPairComatches :: CriticalPair m -> Maybe (m, m)
getCriticalPairComatches = comatches

-- | Returns the type of a Critical Pair
getCriticalPairType :: CriticalPair m -> CriticalPairType
getCriticalPairType = cpType

-- | Returns the nac match of a 'CriticalPair'
getNacMatchOfCriticalPair :: CriticalPair m -> Maybe m
getNacMatchOfCriticalPair criticalPair =
  case nacMatch criticalPair of
    Just (nac,_) -> Just nac
    Nothing -> Nothing

-- | Returns the nac index of a 'CriticalPair'
getNacIndexOfCriticalPair :: CriticalPair m -> Maybe Int
getNacIndexOfCriticalPair criticalPair =
  case nacMatch criticalPair of
    Just (_,idx) -> Just idx
    Nothing -> Nothing

-- | Returns the Critical Pairs with rule names
namedCriticalPairs :: (EpiPairs m, DPO m) => DPOConfig -> [NamedRule m] -> [NamedCriticalPairs m]
namedCriticalPairs conf namedRules =
  map (uncurry getCPs) [(a,b) | a <- namedRules, b <- namedRules]
    where
      getCPs (n1,r1) (n2,r2) =
        (n1, n2, findCriticalPairs conf r1 r2)

-- TODO: Use this as an auxiliary function to optimize the search for critical pairs
findPotentialCriticalPairs :: (DPO m, EpiPairs m) => DPOConfig -> Production m -> Production m -> [(m, m)]
findPotentialCriticalPairs conf p1 p2 = satisfyingPairs
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs

-- | Finds all Critical Pairs between two given Productions
findCriticalPairs :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalPair m]
findCriticalPairs conf p1 p2 =
  findAllDeleteUseAndProduceDangling conf p1 p2 ++ findAllProduceForbid conf p1 p2

-- ** Conflicts

-- *** Delete-Use

-- | All DeleteUse caused by the derivation of @p1@ before @p2@.
-- It occurs when @p1@ deletes something used by @p2@.
findAllDeleteUse :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllDeleteUse conf p1 p2 =
  map (\m -> CriticalPair m Nothing Nothing DeleteUse) deleteUsePairs
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    deleteUsePairs = filter (isDeleteUse conf p1) satisfyingPairs

-- *** Produce-Dangling

-- | All ProduceDangling caused by the derivation of @p1@ before @p2@.
-- It occurs when @p1@ creates something that unable @p2@.
findAllProduceDangling :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllProduceDangling conf p1 p2 =
  map (\m -> CriticalPair m Nothing Nothing ProduceDangling) produceDanglingPairs
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    satisfyingPairs = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    produceDanglingPairs = filter (isProduceDangling conf p1 p2) satisfyingPairs

-- DeleteUse and Produce-Dangling

-- | Tests DeleteUse and ProduceDangling for the same pairs,
-- more efficient than deal separately.
findAllDeleteUseAndProduceDangling :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllDeleteUseAndProduceDangling conf p1 p2 =
  map categorizeConflict conflicts
  where
    pairs = createJointlyEpimorphicPairsFromCodomains (matchRestriction conf) (getLHS p1) (getLHS p2)
    gluing = filter (\(m1,m2) -> satisfyRewritingConditions conf (p1,m1) (p2,m2)) pairs
    conflicts = mapMaybe (deleteUseDangling conf p1 p2) gluing
    categorizeConflict x = case x of
      (Left m) -> CriticalPair m Nothing Nothing DeleteUse
      (Right m) -> CriticalPair m Nothing Nothing ProduceDangling

-- *** Produce-Forbid

-- | All ProduceForbid caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a produce-forbid conflict with @p2@ if some
-- NAC in @p2@ fails to be satisfied after the aplication of @p1@.
findAllProduceForbid :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> [CriticalPair m]
findAllProduceForbid conf p1 p2 =
  concatMap (findProduceForbidForNAC conf p1 p2) (zip (getNACs p2) [0..])

-- | Check ProduceForbid for a NAC @n@ in @p2@.
findProduceForbidForNAC :: (EpiPairs m, DPO m) => DPOConfig -> Production m -> Production m -> (m, Int) -> [CriticalPair m]
findProduceForbidForNAC conf p1 p2 nac =
  map
    (\(m,m',nac) -> CriticalPair m (Just m') (Just nac) ProduceForbid)
    (produceForbidOneNac conf p1 p2 nac)
