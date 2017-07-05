{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Logic.Ctl.Parser (parseExpr) where

import           Data.Functor.Identity
import           Text.Parsec
import qualified Text.Parsec.Token     as P

import           Logic.Ctl.Base


type Parser a = Parsec String () a


-- | Parse a CTL expressions from the given string.
--
-- The first parameter will be used to identify the source of the
-- text in error messages.
--
-- This parser is compatible with the pretty printer of expressions,
-- that is, pretty printed expressions will be parseable by this
-- (unless they contain illegal identifiers for atomic propositions).
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr =
  parse (whiteSpace *> expr <* eof)


expr :: Parser Expr
expr =
  exprImplicative


exprImplicative :: Parser Expr
exprImplicative =
  do
    e1 <- exprBoolean
    exprImplies e1 <|> exprEquiv e1 <|> pure e1
  where
    exprImplies =
      exprAssocRight exprBoolean (reservedOp "->") Implies

    exprEquiv e1 =
      Equiv e1 <$> (reservedOp "<->" *> exprBoolean)


exprBoolean :: Parser Expr
exprBoolean =
  do
    e1 <- exprUnary
    exprAnd e1 <|> exprOr e1 <|> pure e1
  where
    exprAnd =
      exprAssocRight exprUnary (reservedOp "&&") And

    exprOr =
      exprAssocRight exprUnary (reservedOp "||") Or


exprAssocRight :: Parser a -> Parser b -> (a -> a -> a) -> a -> Parser a
exprAssocRight item sep combine =
  let
    go e1 =
      do
        e2 <- sep *> item
        combine e1 <$> (go e2 <|> pure e2)
  in
    go


exprUnary :: Parser Expr
exprUnary =
  exprNot <|> exprUnaryTemporal <|> exprBinaryTemporal <|> exprAtomic


exprNot, exprUnaryTemporal, exprBinaryTemporal :: Parser Expr
exprNot =
  Not <$> (reservedOp "~" *> exprUnary)

exprUnaryTemporal =
  Temporal <$> (stateQuantifier <*> exprUnary)

exprBinaryTemporal =
  let
    pathQuantifier =
      (reserved "A" *> pure A) <|> (reserved "E" *> pure E) <?> "temporal quantifier"

    stateQuantifier =
      reserved "U" *> pure U
  in
    Temporal <$> (pathQuantifier <*> brackets (exprInfix expr stateQuantifier))


stateQuantifier :: Parser (e -> PathQuantified e)
stateQuantifier =
  (reserved "AX" *> pure (A . X))
  <|> (reserved "AF" *> pure (A . F))
  <|> (reserved "AG" *> pure (A . G))
  <|> (reserved "EX" *> pure (E . X))
  <|> (reserved "EF" *> pure (E . F))
  <|> (reserved "EG" *> pure (E . G))
  <?> "temporal quantifier"


exprInfix :: Parser a -> Parser (a -> a -> b) -> Parser b
exprInfix item operator =
  do
    e1 <- item
    op <- operator
    e2 <- item
    return (e1 `op` e2)


exprAtomic :: Parser Expr
exprAtomic =
  literal <|> atom <|> parens expr


atom, literal :: Parser Expr
atom =
  Atom <$> identifier <?> "atomic formula"

literal =
  let
    true =
      reserved "true"  *> pure True <?> "true"

    false =
      reserved "false" *> pure False <?> "false"
  in
    Literal <$> (true <|> false)


lexer :: (Stream s Identity Char) => P.GenTokenParser s u Identity
lexer =
  P.makeTokenParser ctlDef


brackets, parens :: Parser a -> Parser a
brackets =
  P.brackets lexer

parens =
  P.parens lexer


reserved, reservedOp :: String -> Parser ()
reserved =
  P.reserved lexer

reservedOp =
  P.reservedOp lexer


identifier :: Parser String
identifier =
  P.identifier lexer


whiteSpace :: Parser ()
whiteSpace =
  P.whiteSpace lexer


ctlDef :: (Stream s Identity Char) => P.GenLanguageDef s u Identity
ctlDef =
  P.LanguageDef
    { P.commentStart =
        "{-"

    , P.commentEnd =
        "-}"

    , P.commentLine =
        "--"

    , P.nestedComments =
        True

    , P.identStart =
        lower <|> oneOf "_"

    , P.identLetter =
        alphaNum <|> oneOf "_'"

    , P.opStart =
        oneOf "&|-<~"

    , P.opLetter =
        oneOf "&|->"

    , P.reservedOpNames =
        ["&&", "||", "~", "->", "<->"]

    , P.reservedNames =
        ["true", "false", "A", "E", "U", "W"]
        ++ [pq++sq | pq <- ["A", "E"], sq <- ["X", "F", "G"]]

    , P.caseSensitive =
        True
    }
