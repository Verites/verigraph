module XML.ConstraintFormulas () where

import           Text.Parsec
import           Text.Parsec.Token

data Expr = Term
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          deriving (Eq, Show)

formulaDef :: LanguageDef()
formulaDef = LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments = True
  , identStart = alphaNum
  , identLetter = alphaNum
  , opStart = oneOf ""
  , opLetter = oneOf ""
  , reservedOpNames = ["!", "&amp;", "|"]
  , reservedNames = []
  , caseSensitive = False
  }

table :: OperatorTable String () Identity Expr
table = [[infixOp "&amp;" And, infixOp "|" Or, prefixOp "!" Not]]
