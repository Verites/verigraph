{-|
Description : Abstract syntax for the data algebra, its expressions and restrictions.
-}
module SymbolicGraph.DataAlgebra.Syntax
  ( Restriction(..)
  , Predicate(..)
  , Expr(..)
  , typeCheck
  , isWellFormed
  , Constant
  , Variable
  , Operation(..)
  , OpKind(..)
  , OpAssociativity(..)
  , nameOf
  , kindOf
  , arityOf
  , Substitution
  ) where


import           Abstract.Variable

import qualified Data.Map          as Map
import qualified Data.Set          as Set



data Expr
  = EVariable Variable
  | EConstant Constant
  | EApplication Operation [Expr]
  deriving (Eq, Show)


type Constant =
  Integer


data Operation
  = OpAdd
  | OpSub
  | OpMul
  deriving (Eq, Show)


data OpKind
  = PrefixOp
  | InfixOp Int OpAssociativity
  | FunctOp Int


data OpAssociativity
  = AssocLeft
  | AssocRight
  | AssocNone


data Restriction =
  Restriction Predicate Expr Expr
  deriving (Eq, Show)


data Predicate
  = Equal
  | NotEqual
  | LessEqual
  | Less
  deriving (Eq, Show)


nameOf :: Operation -> String
nameOf OpAdd = "+"
nameOf OpSub = "-"
nameOf OpMul = "*"


kindOf :: Operation -> OpKind
kindOf OpAdd = InfixOp 6 AssocLeft
kindOf OpSub = InfixOp 6 AssocLeft
kindOf OpMul = InfixOp 7 AssocLeft


arityOf :: Operation -> Int
arityOf operation =
  case kindOf operation of
    PrefixOp      -> 1
    InfixOp _ _   -> 2
    FunctOp arity -> arity


typeCheck :: Expr -> Either String ()
typeCheck expr =
  case expr of
    EVariable _ ->
      Right ()

    EConstant _ ->
      Right ()

    EApplication operation args ->
      if length args == arityOf operation then
        Right ()
      else
        Left ( "Operation '" ++ nameOf operation ++ "' takes " ++ show (arityOf operation)
               ++ "arguments, but was given " ++ show (length args) )


isWellFormed :: Expr -> Bool
isWellFormed expr =
  case typeCheck expr of
    Left _  -> False
    Right _ -> True



instance FreeVariables Restriction where

  {-# INLINE freeVariableSet #-}
  freeVariableSet (Restriction _ e1 e2) =
    freeVariableSet e1 `Set.union` freeVariableSet e2


  {-# INLINE renameVariables #-}
  renameVariables subst (Restriction relation e1 e2) =
    Restriction relation (renameVariables subst e1) (renameVariables subst e2)



instance FreeVariables Expr where

  freeVariableSet (EVariable var) = Set.singleton var
  freeVariableSet (EConstant _) = Set.empty
  freeVariableSet (EApplication _ args) = Set.unions (map freeVariableSet args)


  renameVariables subst expr =
    case expr of
      EConstant _ ->
        expr

      EApplication operator args ->
        EApplication operator $ map (renameVariables subst) args

      EVariable var ->
        case Map.lookup var subst of
          Just newVar ->
            EVariable newVar

          Nothing ->
            expr
