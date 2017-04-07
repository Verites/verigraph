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


  shrink expr =
    case expr of
      EVariable _ ->
        [expr]

      EConstant _ ->
        [expr]

      EApplication _ args ->
        concatMap shrink args ++ [expr]



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
