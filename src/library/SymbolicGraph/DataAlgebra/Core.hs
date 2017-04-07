module SymbolicGraph.DataAlgebra.Core
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
  , FreeVariables
  , freeVariablesOf
  , renameVariables
  ) where

{-|
Description : Datatypes for representing the base algebra.
Copyright   : (c) Guilherme Azzi, 2017
License     : MIT
Stability   : experimental

The base algebra used for symbolic graphs is the algebra of integers with
addition, subtraction and multiplication.
-}

import           Data.Set  (Set)
import qualified Data.Set  as Set

import           Data.Map  (Map)
import qualified Data.Map  as Map

import           Data.Text (Text)



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


type Variable =
  Text


data Restriction =
  Restriction Predicate Expr Expr
  deriving (Show)


data Predicate
  = Equal
  | NotEqual
  | LessEqual
  | Less
  deriving (Eq, Show)


type Substitution =
  Map Variable Variable


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


class FreeVariables t where

  freeVariablesOf :: t -> Set Variable

  renameVariables :: Substitution -> t -> t



instance FreeVariables a => FreeVariables [a] where

  freeVariablesOf xs =
    Set.unions (map freeVariablesOf xs)


  renameVariables subst =
    map (renameVariables subst)



instance (FreeVariables a, FreeVariables b) => FreeVariables (a, b) where

  freeVariablesOf (x, y) =
    freeVariablesOf x `Set.union` freeVariablesOf y

  renameVariables subst (x, y) =
    (renameVariables subst x, renameVariables subst y)



instance FreeVariables Restriction where

  freeVariablesOf (Restriction _ e1 e2) =
    freeVariablesOf e1 `Set.union` freeVariablesOf e2


  renameVariables subst (Restriction relation e1 e2) =
    Restriction relation (renameVariables subst e1) (renameVariables subst e2)



instance FreeVariables Expr where

  freeVariablesOf (EVariable var) = Set.singleton var
  freeVariablesOf (EConstant _) = Set.empty
  freeVariablesOf (EApplication _ args) = Set.unions (map freeVariablesOf args)


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
