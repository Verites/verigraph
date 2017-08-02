{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Analysis.CriticalPairs
 ( CriticalPairType (..),
   CriticalPair (..),
   getCriticalPairMatches,
   getCriticalPairComatches,
   getNacIndexOfCriticalPair,
   getNacMatchOfCriticalPair,
   getCriticalPairType,

   -- * Finding Critical Pairs
   findCriticalPairs,
   findPotentialCriticalPairs,
   namedCriticalPairs,
   findAllDeleteUse,
   findAllProduceForbid,
   findAllProduceDangling,
   findAllDeleteUseAndProduceDangling
   ) where

import           Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO                   hiding (calculateComatch)
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Util.Monad


-- | Data representing the type of a 'CriticalPair'
data CriticalPairType =
    FreeOverlap
  | DeleteUse
  | ProduceForbid
  | ProduceDangling
  deriving(Eq,Show)

type NamedRule cat morph = (String, Production cat morph)
type NamedCriticalPairs morph = (String,String,[CriticalPair morph])

-- | A Critical Pair is defined as two matches (m1,m2) from the left
-- side of their productions to a same graph.
-- It assumes that the derivation of the production with match @m1@ causes
-- a conflict with the production with match @m2@
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

-- TODO: Restructure types so that there are no 'Maybe' fields dependent on other fields.
data CriticalPair morph = CriticalPair {
    matches   :: (morph,morph),
    comatches :: Maybe (morph,morph),
    nacMatch  :: Maybe (morph, Int), --if it is a ProduceForbid, here is the index of the nac
    cpType    :: CriticalPairType
    } deriving (Eq,Show)

-- | Returns the matches (m1,m2)
getCriticalPairMatches :: CriticalPair morph -> (morph,morph)
getCriticalPairMatches = matches

-- | Returns the comatches (m1',m2')
getCriticalPairComatches :: CriticalPair morph -> Maybe (morph,morph)
getCriticalPairComatches = comatches

-- | Returns the type of a Critical Pair
getCriticalPairType :: CriticalPair morph -> CriticalPairType
getCriticalPairType = cpType

-- | Returns the nac match of a 'CriticalPair'
getNacMatchOfCriticalPair :: CriticalPair morph -> Maybe morph
getNacMatchOfCriticalPair criticalPair =
  case nacMatch criticalPair of
    Just (nac,_) -> Just nac
    Nothing      -> Nothing

-- | Returns the nac index of a 'CriticalPair'
getNacIndexOfCriticalPair :: CriticalPair morph -> Maybe Int
getNacIndexOfCriticalPair criticalPair =
  case nacMatch criticalPair of
    Just (_,idx) -> Just idx
    Nothing      -> Nothing

-- | Returns the Critical Pairs with rule names
namedCriticalPairs :: (EM'PairFactorizable cat morph, DPO cat morph) => [NamedRule cat morph] -> cat [NamedCriticalPairs morph]
namedCriticalPairs namedRules = mapM (uncurry findCriticalPairs') [ (a,b) | a <- namedRules, b <- namedRules ]
  where findCriticalPairs' (n1,r1) (n2,r2) = (n1, n2,) <$> findCriticalPairs r1 r2

-- TODO: Use this as an auxiliary function to optimize the search for critical pairs
-- | Returns a list of morphisms from left side of rules to all valid overlapping pairs
findPotentialCriticalPairs :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph -> Production cat morph -> cat [(morph,morph)]
findPotentialCriticalPairs p1 p2 = do
  pairsOfMatches <- findJointlyEpicPairs (matchMorphism @cat, leftObject p1) (matchMorphism @cat, leftObject p2)
  filterM validMatches pairsOfMatches
  where validMatches (m1, m2) = satisfiesRewritingConditions p1 m1 `andM` satisfiesRewritingConditions p2 m2

-- | Finds all Critical Pairs between two given Productions
findCriticalPairs :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findCriticalPairs p1 p2 = (++) <$> findAllDeleteUseAndProduceDangling p1 p2 <*> findAllProduceForbid p1 p2

-- ** Conflicts

-- *** Delete-Use

-- | All DeleteUse caused by the derivation of @p1@ before @p2@.
-- It occurs when @p1@ deletes something used by @p2@.
findAllDeleteUse :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findAllDeleteUse p1 p2 = do
  matchPairs <- findPotentialCriticalPairs p1 p2
  deleteUsePairs <- filterM (isDeleteUse p1) matchPairs
  return [ CriticalPair m Nothing Nothing DeleteUse | m <- deleteUsePairs ]

-- *** Produce-Dangling

-- | All ProduceDangling caused by the derivation of @p1@ before @p2@.
-- It occurs when @p1@ creates something that unable @p2@.
findAllProduceDangling :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findAllProduceDangling p1 p2 = do
  matchPairs <- findPotentialCriticalPairs p1 p2
  produceDanglingPairs <- filterM (isProduceDangling p1 p2) matchPairs
  return [ CriticalPair m Nothing Nothing ProduceDangling | m <- produceDanglingPairs ]

-- DeleteUse and Produce-Dangling

-- | Tests DeleteUse and ProduceDangling for the same pairs,
-- more efficient than deal separately.
findAllDeleteUseAndProduceDangling :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findAllDeleteUseAndProduceDangling p1 p2 = do
  matchPairs <- findPotentialCriticalPairs p1 p2
  conflicts <- mapM (deleteUseDangling p1 p2) matchPairs
  return [ categorizeConflict x | Just x <- conflicts ]
  where
    categorizeConflict x = case x of
      (Left m)  -> CriticalPair m Nothing Nothing DeleteUse
      (Right m) -> CriticalPair m Nothing Nothing ProduceDangling

-- *** Produce-Forbid

-- | All ProduceForbid caused by the derivation of @p1@ before @p2@.
--
-- Rule @p1@ causes a produce-forbid conflict with @p2@ if some
-- NAC in @p2@ fails to be satisfied after the aplication of @p1@.
findAllProduceForbid :: (EM'PairFactorizable cat morph, DPO cat morph) => Production cat morph -> Production cat morph -> cat [CriticalPair morph]
findAllProduceForbid p1 p2 = do
  conflicts <- mapM (produceForbidOneNac p1 p2) (zip (nacs p2) [0..])
  return [ CriticalPair matches (Just comatches) (Just nac) ProduceForbid
            | (matches, comatches, nac) <- concat conflicts ]
