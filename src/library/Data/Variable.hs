module Data.Variable where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (Text)


-- | Variables are represented by strings.
type Variable = Text

-- | Substitution mapping variables to terms of some type.
type Substitution t = Map Variable t

-- | Substitution mapping variables to other variables.
type Renaming = Substitution Variable


-- | Type class for data types whose values contain free variables.
class FreeVariables a where
  {-# MINIMAL freeVariableSet, renameVariables #-}

  -- | Set of variables that occur free in the given value.
  freeVariableSet :: a -> Set Variable

  -- | Replace all free occurrences of the first variable by the second variable.
  --
  -- __Note:__ this can cause name clashes.
  renameVariable :: Variable -> Variable -> a -> a
  renameVariable x y = renameVariables (Map.fromList [(x, y)])

  -- | Replace free occurrences of variables according to the given substitution.
  -- If a free variable is not mapped by the substitution, it remains the same.
  --
  -- __Note:__ this can cause name clashes.
  renameVariables :: Renaming -> a -> a


-- | List of veriables that occur free in the given value.
freeVariablesOf :: FreeVariables a => a -> [Variable]
freeVariablesOf = Set.toList . freeVariableSet
{-# INLINE freeVariablesOf #-}


instance FreeVariables a => FreeVariables [a] where

  freeVariableSet ts = Set.unions (map freeVariableSet ts)
  {-# INLINE freeVariableSet #-}

  renameVariable x y = map (renameVariable x y)
  {-# INLINE renameVariable #-}

  renameVariables subst = map (renameVariables subst)
  {-# INLINE renameVariables #-}


instance (FreeVariables a, FreeVariables b) => FreeVariables (a, b) where

  freeVariableSet (t1, t2) = freeVariableSet t1 `Set.union` freeVariableSet t2
  {-# INLINE freeVariableSet #-}

  renameVariable x y (t1, t2) = (renameVariable x y t1, renameVariable x y t2)
  {-# INLINE renameVariable #-}

  renameVariables subst (t1, t2) = (renameVariables subst t1, renameVariables subst t2)
  {-# INLINE renameVariables #-}
