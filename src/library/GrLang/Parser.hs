{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GrLang.Parser (parseTopLevel) where

import           Data.Functor.Identity
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Text.Parsec           hiding (optional)
import qualified Text.Parsec.Token     as P

import           Base.Annotation       (Annotated (..), Located)
import qualified Base.Annotation       as Ann
import           GrLang.AST

parseTopLevel :: SourceName -> String -> Either ParseError [TopLevelDeclaration]
parseTopLevel = parse (whiteSpace *> many topLevelDecl <* eof)

topLevelDecl :: Stream s Identity Char => Parsec s u TopLevelDeclaration
topLevelDecl = choice
  [ importDecl <?> "import"
  , nodeType <?> "node type"
  , edgeType <?> "edge type"
  , graph <?> "graph" ]
  where
    importDecl =
      reserved "import" >> Import <$> located filePath

    nodeType =
      reserved "node" >> reserved "type" >> DeclNodeType <$> located identifier

    edgeType =
      reserved "edge" >> reserved "type" >>
      DeclEdgeType
        <$> located identifier
        <*> (reservedOp ":" *> located identifier)
        <*> (reservedOp "->" *> located identifier)

    graph =
      reserved "graph" >>
      DeclGraph
        <$> located identifier
        <*> braces (many graphDecl)

graphDecl :: Stream s Identity Char => Parsec s u GraphDeclaration
graphDecl = (located identifier >>= \n -> edge n <|> node n) <?> "node or edge"
  where
    node n = do
      ns <- many (comma *> located identifier)
      DeclNodes (n:ns) <$> (reservedOp ":" *> located identifier)

    edge src =
      DeclEdges src
        <$> (reservedOp "-" *> (try multipleTypes <|> singleType) <* reservedOp "->")
        <*> located identifier

    singleType =
      SingleType
        <$> commaSep1 (located identifier)
        <*> (reservedOp ":" *> located identifier)

    multipleTypes =
      MultipleTypes <$> commaSep1 (located singleEdge)

    singleEdge =
      (,) <$> optional identifier <*> (reservedOp ":" *> identifier)


lexer :: Stream s Identity Char => P.GenTokenParser s u Identity
lexer =
  P.makeTokenParser langDef

optional :: Stream s Identity Char => Parsec s u a -> Parsec s u (Maybe a)
optional = optionMaybe . try

located :: Stream s Identity Char => Parsec s u a -> Parsec s u (Located a)
located parser = annotate <$> getPosition <*> parser
  where annotate pos = A $ Just (Ann.Position (sourceLine pos) (sourceColumn pos), sourceName pos)

parens, brackets, braces :: Stream s Identity Char => Parsec s u a -> Parsec s u a
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer

commaSep, commaSep1 :: Stream s Identity Char => Parsec s u a -> Parsec s u [a]
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer

reserved, reservedOp :: Stream s Identity Char => String -> Parsec s u ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

identifier :: Stream s Identity Char => Parsec s u Text
identifier = Text.pack <$> P.identifier lexer

filePath :: Stream s Identity Char => Parsec s u FilePath
filePath = P.stringLiteral lexer

whiteSpace, comma :: Stream s Identity Char => Parsec s u ()
whiteSpace = P.whiteSpace lexer
comma = P.comma lexer *> pure ()


langDef :: (Stream s Identity Char) => P.GenLanguageDef s u Identity
langDef =
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
        letter <|> oneOf "_?"

    , P.identLetter =
        alphaNum <|> oneOf "_'"

    , P.opStart =
        oneOf ":-"

    , P.opLetter =
        oneOf ">"

    , P.reservedOpNames =
        [":", "-", "->"]

    , P.reservedNames =
        ["graph", "node", "edge", "type"]

    , P.caseSensitive =
        True
    }
