{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module SymbolicGraph.DataAlgebra.Parser where

import SymbolicGraph.DataAlgebra
import SymbolicGraph.DataAlgebra.PrettyPrint ()

import Data.String (fromString)

import Text.Parsec
import qualified Text.Parsec.Expr as Parsec
import Text.Parsec.Token (GenLanguageDef(..), GenTokenParser)
import qualified Text.Parsec.Token as Tok

import qualified Data.List as List
import qualified Data.IntMap as IntMap

import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.Text.Lazy (Text)


data Command
  = CheckSatisfability [Restriction]
  | CheckStrengthening [Restriction] [Restriction]
  | FindRenamings [Restriction] [Restriction]
  | FindOverlappings [Restriction] [Restriction]


test :: Text -> Either ParseError Text
test input =
  PP.displayT . PP.renderPretty 0.8 100 . PP.pretty <$> parse expr' "<input>" input


expr' :: (Stream s m Char) => ParsecT s u m Expr
expr' =
  expr [OpAdd, OpSub, OpMul] <* eof


command :: (Stream s m Char) => ParsecT s u m Command
command =
    (reserved "checkSatisfability" CheckSatisfability <*> restrictionSet)
    <|> (reserved "checkStrengthening" CheckStrengthening <*> restrictionSet <*> restrictionSet)
    <|> (reserved "findRenamings" FindRenamings <*> restrictionSet <*> restrictionSet)
    <|> (reserved "findOverlappings" FindOverlappings <*> restrictionSet <*> restrictionSet)
    <?> "command"

  where
    restrictionSet =
      Tok.braces tokenParser (restriction [OpAdd, OpSub, OpMul] `sepBy` Tok.semi tokenParser)

    reserved word value =
      Tok.reserved tokenParser word *> pure value


restriction :: (Stream s m Char) => [Operation] -> ParsecT s u m Restriction
restriction operations =
    makeRestriction <$> expr operations <*> predicate <*> expr operations

  where
    makeRestriction e1 pred e2 =
      Restriction pred e1 e2


predicate :: (Stream s m Char) => ParsecT s u m Predicate
predicate =
    (reservedOp "==" *> pure Equal)
    <|> (reservedOp "!=" *> pure NotEqual)
    <|> (reservedOp "<=" *> pure LessEqual)
    <|> (reservedOp "<" *> pure Less)

  where
    reservedOp = Tok.reservedOp tokenParser


expr :: (Stream s m Char) => [Operation] -> ParsecT s u m Expr
expr operations =
    Parsec.buildExpressionParser table (term operations)
    <?> "expression"
  where
    table = makeOperatorTable operations


makeOperatorTable :: (Stream s m Char) => [Operation] -> [[Parsec.Operator s u m Expr]]
makeOperatorTable operators =
  let
    table =
      List.foldl' addOperator IntMap.empty operators

    addOperator table op =
      case resolveOperator op of
        Just (precedence, operator) ->
          IntMap.alter (prepend operator) precedence table

        Nothing ->
          table

    prepend x Nothing = Just [x]
    prepend x (Just xs) = Just (x:xs)

  in
    filter (not . List.null) [ table IntMap.! prec | prec <- reverse (IntMap.keys table) ]


resolveOperator :: (Stream s m Char) => Operation -> Maybe (Int, Parsec.Operator s u m Expr)
resolveOperator op =
  case kindOf op of
    PrefixOp ->
      Just
        ( 10
        , Parsec.Prefix (operator op *> pure (\e -> EApplication op [e]))
        )

    InfixOp prec assoc ->
      let
        assoc' =
          case assoc of
            AssocLeft -> Parsec.AssocLeft
            AssocRight -> Parsec.AssocRight
            AssocNone -> Parsec.AssocNone
      in
        Just
          ( prec
          , Parsec.Infix (operator op *> pure (\e1 e2 -> EApplication op [e1, e2])) assoc'
          )

    FunctOp _ ->
      Nothing


operator :: (Stream s m Char) => Operation -> ParsecT s u m ()
operator =
  Tok.reservedOp tokenParser . nameOf


term :: (Stream s m Char) => [Operation] -> ParsecT s u m Expr
term operations =
  (EConstant <$> literal)
  <|> (EVariable <$> variable)
  <|> Tok.parens tokenParser (expr operations)


literal :: (Stream s m Char) => ParsecT s u m Constant
literal =
  Tok.integer tokenParser
  <?> "integer literal"


variable :: (Stream s m Char) => ParsecT s u m Variable
variable =
  (fromString <$> Tok.identifier tokenParser)
  <?> "variable"




tokenParser :: (Stream s m Char) => GenTokenParser s u m
tokenParser =
  Tok.makeTokenParser language


language :: (Stream s m Char) => GenLanguageDef s u m
language =
  LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter language
    , opLetter        = oneOf "*+-=<!"
    , reservedOpNames = ["+", "-", "*"]
    , reservedNames   = []
    , caseSensitive   = True
    }
