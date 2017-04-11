module SymbolicGraph.DataAlgebra.QuickCheck where

import           SymbolicGraph.DataAlgebra.Core

import qualified Data.Text                      as T
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen


instance Arbitrary Expr where

  arbitrary = do
    numVariables <- choose (0, 10)
    let variables = map T.singleton $ take numVariables ['a'..'z']
    expression variables


  shrink =
    shrinkExpression


instance Arbitrary Restriction where

  arbitrary = do
    numVariables <- choose (0, 10)
    let variables = map T.singleton $ take numVariables ['a'..'z']
    restriction variables


  shrink =
    shrinkRestriction



-- | Generate a random expression with the given free variables
expression :: [Variable] -> Gen Expr
expression vars =
    sized expression'

  where
    expression' depth =
        oneof $
            [ constant ]
            ++ [ variable | vars /= [] ]
            ++ [ application depth | depth > 0 ]

    variable =
      EVariable <$> elements vars

    constant =
      EConstant <$> arbitrary

    application depth = do
      operation <- elements [OpAdd, OpSub, OpMul]
      let numArgs = arityOf operation

      arguments <- vectorOf numArgs (expression' depth)
      return (EApplication operation arguments)


shrinkExpression :: Expr -> [Expr]
shrinkExpression expr =
  case expr of
    EVariable _ ->
      [expr]

    EConstant _ ->
      [expr]

    EApplication _ args ->
      concatMap shrink args ++ [expr]



restriction :: [Variable] -> Gen Restriction
restriction vars =
  Restriction
    <$> elements [Equal, NotEqual, Less, LessEqual]
    <*> expression vars
    <*> expression vars


shrinkRestriction :: Restriction -> [Restriction]
shrinkRestriction (Restriction pred e1 e2) =
  [ Restriction pred e1' e2' | (e1', e2') <- shrink (e1, e2) ]
