module XML.Formulas
( Formula(..)
,  parseFormula
)

where

import           Control.Monad

import           Data.Functor.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

data Formula = IntConst Integer
          | Not Formula
          | And Formula Formula
          | Or  Formula Formula
          deriving (Show)

languageDef :: T.LanguageDef()
languageDef = T.LanguageDef
  { T.commentLine     = "#"
  , T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.identStart      = alphaNum
  , T.identLetter     = alphaNum
  , T.reservedNames   = ["!", "&amp;", "|"]
  , T.reservedOpNames = ["!", "&amp;", "|"]
  , T.opLetter        = oneOf ""
  }

lexer      = T.makeTokenParser languageDef
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
integer    = T.integer    lexer

operators = [
  [Prefix (reservedOp "!" >> return (Not             ))          ]
 ,[Infix  (reservedOp "|"     >> return (Or     )) AssocLeft, Infix  (reservedOp "&amp;" >> return (And    )) AssocLeft]]

terms = parens formula
      <|> liftM IntConst integer

formula :: Parser Formula
formula = buildExpressionParser operators terms

parseFormula :: String -> Formula
parseFormula str =
  case parse formula "" str of
    Left  e -> error $ show e
    Right r -> r
