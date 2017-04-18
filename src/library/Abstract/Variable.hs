module Abstract.Variable where

import           Data.Map  (Map)
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (Text)

type Variable =
  Text


type Substitution =
  Map Variable Variable


class FreeVariables a where

  freeVariableSet :: a -> Set Variable
  renameVariables :: Substitution -> a -> a

  {-# MINIMAL freeVariableSet, renameVariables #-}


freeVariablesOf :: FreeVariables a => a -> [Variable]
{-# INLINE freeVariablesOf #-}
freeVariablesOf =
  Set.toList . freeVariableSet



instance FreeVariables a => FreeVariables [a] where

  {-# INLINE freeVariableSet #-}
  freeVariableSet xs =
    Set.unions (map freeVariableSet xs)


  {-# INLINE renameVariables #-}
  renameVariables subst =
    map (renameVariables subst)



instance (FreeVariables a, FreeVariables b) => FreeVariables (a, b) where

  {-# INLINE freeVariableSet #-}
  freeVariableSet (x, y) =
    freeVariableSet x `Set.union` freeVariableSet y


  {-# INLINE renameVariables #-}
  renameVariables subst (x, y) =
    (renameVariables subst x, renameVariables subst y)
