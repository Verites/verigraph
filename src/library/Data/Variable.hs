{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Variable
  ( VarId
  , Variable(..)
  , Substitution
  , Renaming
  -- * Values with free variables
  , FreeVariables(..)
  , freeVariablesOf
  , freeVariableSet
  , freeVariableIdsOf
  ) where

import           Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import           Data.EnumSet (EnumSet)
import           Data.Text    (Text)


-- | Type of variable indentifiers, which are just 'Int's.
newtype VarId = VarId Int
  deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show VarId where
  show = show . fromEnum


-- | Variables are identified by integers, containing also a list of names that the user may
-- expect for it.
--
-- Variables are __not__ 'Eq'. If you want to compare them, compare their identifiers.
data Variable = Variable
  { varId        :: VarId -- ^ Integer identifier for the variable.
  , varNameHints :: [Text] -- ^ List of names that are expected for the variable.
  } deriving Show

-- | Substitution mapping variables to terms of some type.
type Substitution t = EnumMap VarId t

-- | Substitution mapping variables to other variables.
type Renaming = Substitution Variable


-- | Type class for data types whose values contain free variables.
--
-- Values may contain multiple occurrences of the same variable identifier. Be careful to maintain
-- consistency between these occurrences, i.e. other information about the variable (name hints)
-- should be the same for all occurrences.
class FreeVariables a where
  {-# MINIMAL freeVariableMap, renameVariables #-}

  -- | Map of variables that occur free in the given value.
  freeVariableMap :: a -> EnumMap VarId Variable

  -- | Replace all free occurrences of the first variable by the second variable.
  --
  -- __Note:__ this can cause name clashes.
  renameVariable :: VarId -> Variable -> a -> a
  renameVariable x y = renameVariables (EnumMap.fromList [(x, y)])

  -- | Replace free occurrences of variables according to the given substitution.
  -- If a free variable is not mapped by the substitution, it remains the same.
  --
  -- __Note:__ this can cause name clashes.
  renameVariables :: Renaming -> a -> a


-- | Set of variable identifiers that occur free in the given value.
freeVariableSet :: FreeVariables a => a -> EnumSet VarId
freeVariableSet = EnumMap.keysSet . freeVariableMap
{-# INLINE freeVariableSet #-}

-- | List of variable identifiers that occur free in the given value.
freeVariableIdsOf :: FreeVariables a => a -> [VarId]
freeVariableIdsOf = EnumMap.keys . freeVariableMap
{-# INLINE freeVariableIdsOf #-}

-- | List of variables that occur free in the given value.
freeVariablesOf :: FreeVariables a => a -> [Variable]
freeVariablesOf = EnumMap.elems . freeVariableMap
{-# INLINE freeVariablesOf #-}

instance FreeVariables a => FreeVariables [a] where

  freeVariableMap ts = EnumMap.unions (map freeVariableMap ts)
  {-# INLINE freeVariableMap #-}

  renameVariable x y = map (renameVariable x y)
  {-# INLINE renameVariable #-}

  renameVariables subst = map (renameVariables subst)
  {-# INLINE renameVariables #-}


instance (FreeVariables a, FreeVariables b) => FreeVariables (a, b) where

  freeVariableMap (t1, t2) = freeVariableMap t1 `EnumMap.union` freeVariableMap t2
  {-# INLINE freeVariableMap #-}

  renameVariable x y (t1, t2) = (renameVariable x y t1, renameVariable x y t2)
  {-# INLINE renameVariable #-}

  renameVariables subst (t1, t2) = (renameVariables subst t1, renameVariables subst t2)
  {-# INLINE renameVariables #-}
