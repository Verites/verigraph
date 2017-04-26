module Logic.Ctl.TestUtils where

import           Test.QuickCheck

import           Logic.Ctl


instance Arbitrary Expr where
  arbitrary =
    sized (genExpr . (`div` 5))


genExpr :: Int -> Gen Expr
genExpr 0 =
  oneof
    [ Literal <$> arbitrary
    , Atom <$> genIdentifier
    ]


genExpr n =
  let
    n' = n - 1
  in
    oneof
      [ Literal <$> arbitrary
      , Atom <$> genIdentifier
      , Not <$> genExpr n'
      , And <$> genExpr n' <*> genExpr n'
      , Or  <$> genExpr n' <*> genExpr n'
      , Implies <$> genExpr n' <*> genExpr n'
      , Equiv   <$> genExpr n' <*> genExpr n'
      , Temporal <$> genPathQuantified (genExpr n')
      ]


genPathQuantified :: Gen e -> Gen (PathQuantified e)
genPathQuantified expr =
  oneof
    [ A <$> genStateQuantified expr
    , E <$> genStateQuantified expr
    ]

genStateQuantified :: Gen e -> Gen (StateQuantified e)
genStateQuantified expr =
  oneof
    [ X <$> expr
    , G <$> expr
    , F <$> expr
    , U <$> expr <*> expr
    ]

genIdentifier :: Gen String
genIdentifier =
  (:) <$> startChar <*> listOf midChar

  where
    startChar =
      elements startChars

    midChar =
      elements midChars

    startChars =
      ['a'..'z'] ++ "_"

    midChars =
      startChars ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'"
