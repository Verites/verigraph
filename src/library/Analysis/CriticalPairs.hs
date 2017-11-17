module Analysis.CriticalPairs
  ( CriticalPairType (..)
  , CriticalPair (..)
  , getCriticalPairMatches
  , getCriticalPairComatches
  , getNacIndexOfCriticalPair
  , getNacMatchOfCriticalPair
  , getCriticalPairType
  , isDeleteUse
  , isProduceDangling
  , isProduceForbid

   -- * Finding Critical Pairs
  , findCriticalPairs
  , findAllDeleteUse
  , findAllProduceForbid
  , findAllProduceDangling
  , findAllDeleteUseAndProduceDangling
  ) where

import           Data.Maybe                               (mapMaybe)

import           Abstract.Category
import           Abstract.Category.Finitary
import           Abstract.Constraint
import           Abstract.Rewriting.DPO                   hiding (calculateComatch)
import qualified Abstract.Rewriting.DPO.DiagramAlgorithms as Diagram

-- | Data representing the type of a 'CriticalPair'
data CriticalPairType =
    FreeOverlap
  | DeleteUse
  | ProduceForbid
  | ProduceDangling
  deriving(Eq,Show)

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

data CriticalPair morph = CriticalPair {
    matches   :: (morph,morph),
    comatches :: Maybe (morph,morph),
    nacMatch  :: Maybe (morph, Int), --if it is a ProduceForbid, here is the index of the nac
    cpType    :: CriticalPairType
    } deriving (Eq,Show)

isDeleteUse :: CriticalPair morph -> Bool
isDeleteUse (CriticalPair _ _ _ DeleteUse) = True
isDeleteUse _                              = False

isProduceDangling :: CriticalPair morph -> Bool
isProduceDangling (CriticalPair _ _ _ ProduceDangling) = True
isProduceDangling _                                    = False

isProduceForbid :: CriticalPair morph -> Bool
isProduceForbid (CriticalPair _ _ _ ProduceForbid) = True
isProduceForbid _                                  = False

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

-- | Finds all Critical Pairs between two given Productions, excluding those
-- whose initial graph violates some of the given constraints.
findCriticalPairs :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]
findCriticalPairs conf constraints p1 p2 =
  findAllDeleteUseAndProduceDangling conf constraints p1 p2 ++ findAllProduceForbid conf constraints p1 p2

-- ** Conflicts

-- *** Delete-Use

-- | Enumerate all delete-use conflicts caused by the application of the first
-- given rule before the second, excluding those whose initial graph violates
-- some of the given constraints.
--
-- This occurs when the first rule deletes some element that the second rule
-- uses.
findAllDeleteUse :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]
findAllDeleteUse conf constraints p1 p2 =
  let matchPairs = findJointSurjectiveApplicableMatches conf p1 p2
  in [ CriticalPair (m1,m2) Nothing Nothing DeleteUse
        | (m1,m2) <- matchPairs, codomain m1 `satisfiesAllConstraints` constraints
        , Diagram.isDeleteUse conf p1 (m1,m2) ]

-- *** Produce-Dangling

-- | Enumerate all produce-dangling conflicts caused by the application of the
-- first given rule before the second, excluding those whose initial graph
-- violates some of the given constraints.
--
-- This occurs when the first rule creates an edge onto a node that is deleted
-- by the second.
findAllProduceDangling :: (E'PairCofinitary morph, DPO morph) =>  MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]
findAllProduceDangling conf constraints p1 p2 =
  let matchPairs = findJointSurjectiveApplicableMatches conf p1 p2
  in [ CriticalPair (m1,m2) Nothing Nothing ProduceDangling
        | (m1,m2) <- matchPairs, codomain m1 `satisfiesAllConstraints` constraints
        , Diagram.isProduceDangling conf p1 p2 (m1,m2) ]

-- DeleteUse and Produce-Dangling

-- | Enumerate all delete-use and produce-dangling conflicts caused by the
-- application of the first given rule before the second, excluding those whose
-- initial graph violates some of the given constraints.
--
-- This is more efficient than calculating each kind of conflict separately.
findAllDeleteUseAndProduceDangling :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]
findAllDeleteUseAndProduceDangling conf constraints p1 p2 =
  let
    matchPairs = findJointSurjectiveApplicableMatches conf p1 p2
    isValidPair (m1,_)
      | null constraints = True
      | otherwise = codomain m1 `satisfiesAllConstraints` constraints
    conflicts = mapMaybe (Diagram.deleteUseDangling conf p1 p2) $ filter isValidPair matchPairs
  in map categorizeConflict conflicts
  where
    categorizeConflict x = case x of
      (Left m)  -> CriticalPair m Nothing Nothing DeleteUse
      (Right m) -> CriticalPair m Nothing Nothing ProduceDangling

-- *** Produce-Forbid

-- | Enumerate all produce-forbid conflicts caused by the application of the
-- first given rule before the second, excluding those whose initial graph
-- violates some of the given constraints.
--
-- These are caused when the application of the first rule causes
-- the violation of a NAC for the second rule.
findAllProduceForbid :: (E'PairCofinitary morph, DPO morph) => MorphismsConfig morph -> [Constraint morph] -> Production morph -> Production morph -> [CriticalPair morph]
findAllProduceForbid conf constraints p1 p2 =
  let conflicts = concatMap (Diagram.produceForbidOneNac conf constraints p1 p2) (zip (nacs p2) [0..])
  in [ CriticalPair matches (Just comatches) (Just nac) ProduceForbid
        | (matches, comatches, nac) <- conflicts ]
