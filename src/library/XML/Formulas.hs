module XML.Formulas
( Formula(..)
,  parseFormula
)

where

import           Data.Functor.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as T

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
  , T.nestedComments  = False
  , T.opStart         = oneOf ""
  , T.opLetter        = oneOf ""
  , T.caseSensitive   = False
  -- Everything above this comment is unused and it is here just because of the library warnings
  , T.identStart      = alphaNum
  , T.identLetter     = alphaNum
  , T.reservedNames   = ["!", "&amp;", "|", "&"]
  , T.reservedOpNames = ["!", "&amp;", "|", "&"]
  }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser languageDef

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

integer :: Parser Integer
integer = T.integer lexer

operators :: OperatorTable String () Identity Formula
operators = [
  [Prefix (reservedOp "!"     >> return Not )          ],
  [Infix  (reservedOp "|"     >> return Or  ) AssocLeft
  ,Infix  (reservedOp "&amp;" >> return And ) AssocLeft
  ,Infix  (reservedOp "&"     >> return And ) AssocLeft]]

terms :: Parser Formula
terms = parens formula
      <|> fmap IntConst integer

formula :: Parser Formula
formula = buildExpressionParser operators terms

parseFormula :: String -> Formula
parseFormula str =
  case parse formula "" str of
    Left  e -> error $ show e ++ " when parsing the following expression: " ++ str
    Right r -> r
