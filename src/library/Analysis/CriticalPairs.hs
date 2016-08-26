module Analysis.CriticalPairs
 ( CP (..),
   CriticalPair,
   criticalPairs,
   namedCriticalPairs,
   allDeleteUse,
   allProduceForbid,
   allProduceDangling,
   getMatch,
   getComatch,
   getCPNacIdx,
   getCPNac,
   getCP,
   deleteUse
   ) where

import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW hiding (calculateComatch)
import           Analysis.DiagramAlgorithms
import           Data.Maybe                (mapMaybe)

-- | Data representing the type of a 'CriticalPair'
data CP = 
    FOL
  | DeleteUse
  | ProduceForbid
  | ProduceDangling
  deriving(Eq,Show)

-- | A Critical Pair is defined as two matches (m1,m2) from the left
-- side of their rules to a same graph.
-- It assumes that the derivation of the rule with match @m1@ causes
-- a conflict with the rule with match @m2@
--
-- This diagram shows graphs and morphisms names used in the algorithms below
--
-- pLeft = production (L1,K1,R1,[N1])
--
-- pRight = production (L2,K2,R2,[N2])
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
    match  :: (m, m),
    calculateComatch :: Maybe (m, m),
    nacMatch :: Maybe (m, Int), --if is ProduceForbid, here is the index of the nac
    cp  :: CP
    } deriving (Eq,Show)

-- | Returns the matches (m1,m2)
getMatch :: CriticalPair m -> (m, m)
getMatch = match

-- | Returns the comatches (m1',m2')
getComatch :: CriticalPair m -> Maybe (m, m)
getComatch = calculateComatch

-- | Returns the type of a 'CriticalPair'
getCP :: CriticalPair m -> CP
getCP = cp

-- | Returns the nac match of a 'CriticalPair'
getCPNac :: CriticalPair m -> Maybe m
getCPNac cp =
  case nacMatch cp of
    Just (nac,_) -> Just nac
    Nothing -> Nothing

-- | Returns the nac index of a 'CriticalPair'
getCPNacIdx :: CriticalPair m -> Maybe Int
getCPNacIdx cp =
  case nacMatch cp of
    Just (_,idx) -> Just idx
    Nothing -> Nothing

-- | Create all jointly epimorphic pairs of morphisms from the codomains of
-- the given morphisms.
-- The flag indicates only monomorphic morphisms.
createPairsCodomain :: (EpiPairs m) => MatchRestriction -> m -> m -> [(m, m)]
createPairsCodomain inj m1 m2 =
  createPairs (inj == MonoMatches) (codomain m1) (codomain m2)

-- | Returns the Critical Pairs with rule names
namedCriticalPairs :: (EpiPairs m, DPO m) => DPOConfig
  -> [(String, Production m)] -> [(String,String,[CriticalPair m])]
namedCriticalPairs config rules =
  map (uncurry getCPs) [(a,b) | a <- rules, b <- rules]
    where
      getCPs (n1,r1) (n2,r2) =
        (n1, n2, criticalPairs config r1 r2)

-- | All Critical Pairs
criticalPairs :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalPair m]
criticalPairs config pLeft pRight =
  allDeleteUseAndDang config pLeft pRight ++
  allProduceForbid config pLeft pRight

-- ** Conflicts

-- *** Delete-Use

-- | All DeleteUse caused by the derivation of @pLeft@ before @pRight@.
-- It occurs when @pLeft@ deletes something used by @pRight@.
allDeleteUse :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalPair m]
allDeleteUse config pLeft pRight =
  map
    (\m -> CriticalPair m Nothing Nothing DeleteUse)
    delUse
  where
    pairs = createPairsCodomain (matchRestriction config) (left pLeft) (left pRight)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (pLeft,m1) (pRight,m2))
        pairs
    delUse = filter (deleteUse config pLeft) gluing

-- *** Produce-Dangling

-- | All ProduceDangling caused by the derivation of @pLeft@ before @pRight@.
-- It occurs when @pLeft@ creates something that unable @pRight@.
allProduceDangling :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalPair m]
allProduceDangling config pLeft pRight =
  map
    (\m -> CriticalPair m Nothing Nothing ProduceDangling)
    prodDang
  where
    pairs = createPairsCodomain (matchRestriction config) (left pLeft) (left pRight)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (pLeft,m1) (pRight,m2))
        pairs
    prodDang = filter (produceDangling config pLeft pRight) gluing

-- DeleteUse and Produce-Dangling

-- | Tests DeleteUse and ProduceDangling for the same pairs,
-- more efficient than deal separately.
allDeleteUseAndDang :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalPair m]
allDeleteUseAndDang config pLeft pRight =
  map
    (\x -> case x of
      (Left m) -> CriticalPair m Nothing Nothing DeleteUse
      (Right m) -> CriticalPair m Nothing Nothing ProduceDangling)
    conflicts
  where
    pairs = createPairsCodomain (matchRestriction config) (left pLeft) (left pRight)
    gluing =
      filter
        (\(m1,m2) -> satisfyRewritingConditions config (pLeft,m1) (pRight,m2))
        pairs
    conflicts = mapMaybe (deleteUseDangling config pLeft pRight) gluing

-- *** Produce-Forbid

-- | All ProduceForbid caused by the derivation of @pLeft@ before @pRight@.
--
-- Rule @pLeft@ causes a produce-forbid conflict with @pRight@ if some
-- NAC in @pRight@ fails to be satisfied after the aplication of @pLeft@.
allProduceForbid :: (EpiPairs m, DPO m) => DPOConfig
  -> Production m -> Production m -> [CriticalPair m]
allProduceForbid config pLeft pRight =
  concatMap
    (produceForbid config pLeft inverseLeft pRight)
    (zip (nacs pRight) [0..])
  where
    inverseLeft = inverse config pLeft

-- | Check ProduceForbid for a NAC @n@ in @pRight@.
produceForbid :: (EpiPairs m, DPO m) => DPOConfig -> Production m
  -> Production m -> Production m -> (m, Int) -> [CriticalPair m]
produceForbid config pLeft inverseLeft pRight nac =
  map
    (\(m,m',nac) -> CriticalPair m (Just m') (Just nac) ProduceForbid)
    (produceForbidOneNac config pLeft inverseLeft pRight nac)
