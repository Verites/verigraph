{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GrLang.Parser (parseModule, parseGraph, parseMorphism, parseRule, reservedNames, makeIdentifier) where

import qualified Data.Char                 as Char
import           Data.Functor              (($>))
import           Data.Functor.Identity
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as PP
import           Text.Parsec
import           Text.Parsec.Error
import qualified Text.Parsec.Token         as P

import           Base.Annotation           (Located, at)
import           Base.Location
import           GrLang.AST
import           GrLang.Monad

parseModule :: (Monad m, Stream s Identity Char) => SourceName -> s -> ExceptT Error m [TopLevelDeclaration]
parseModule = parseGrLang (many topLevelDecl)

parseGraph :: (Monad m, Stream s Identity Char) => SourceName -> s -> ExceptT Error m [GraphDeclaration]
parseGraph = parseGrLang (many graphDecl)

parseMorphism :: (Monad m, Stream s Identity Char) => SourceName -> s -> ExceptT Error m [MorphismDeclaration]
parseMorphism = parseGrLang (many morphismDecl)

parseRule :: (Monad m, Stream s Identity Char) => SourceName -> s -> ExceptT Error m [RuleDeclaration]
parseRule = parseGrLang (many ruleDecl)

parseGrLang :: (Monad m, Stream s Identity Char) => Parsec s () a -> SourceName -> s -> ExceptT Error m a
parseGrLang parser srcName source =
  case parse (whiteSpace *> parser <* eof) srcName source of
    Right result -> return result
    Left err -> throwError (Just . locationFromParsec $ errorPos err) (errorFromParsec err)
  where
    errorFromParsec = reflow . showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    locationFromParsec pos = Location (sourceName pos) $ Position (sourceLine pos) (sourceColumn pos)
    reflow = PP.fillSep . map pretty . words

topLevelDecl :: Stream s Identity Char => Parsec s u TopLevelDeclaration
topLevelDecl = choice
  [ importDecl <?> "import"
  , nodeType <?> "node type"
  , edgeType <?> "edge type"
  , namedBlock "graph" DeclGraph graphDecl <?> "graph"
  , block "morphism" (nameFromTo DeclMorphism) morphismDecl <?> "morphism"
  , namedBlock "rule" DeclRule ruleDecl <?> "rule"
  ] <* optional semi
  where
    importDecl =
      reserved "import" >> Import <$> located filePath

    nodeType =
      reserved "node" >> reserved "type" >> DeclNodeType <$> located identifier

    edgeType =
      reserved "edge" >> reserved "type" >> nameFromTo DeclEdgeType

nameFromTo :: Stream s Identity Char => (Located Text -> Located Text -> Located Text -> a) -> Parsec s u a
nameFromTo build =
  build
    <$> located identifier
    <*> (reservedOp ":" *> located identifier)
    <*> (reservedOp "->" *> located identifier)

namedBlock :: Stream s Identity Char => String -> (Located Text -> [a] -> b) -> Parsec s u a -> Parsec s u b
namedBlock kind build = block kind (build <$> located identifier)

block :: Stream s Identity Char => String -> Parsec s u ([a] -> b) -> Parsec s u a -> Parsec s u b
block kind header item =
  reserved kind >> header <*> braces (many item)

graphDecl :: Stream s Identity Char => Parsec s u GraphDeclaration
graphDecl = (located identifier >>= \n -> edge n <|> node n) <* optional semi <?> "node or edge"
  where
    node n = do
      ns <- many (located identifier)
      DeclNodes (n:ns) <$> (reservedOp ":" *> located identifier)

    edge src =
      DeclEdges src
        <$> (reservedOp "-" *> commaSep1 edgesWithTypes <* reservedOp "->")
        <*> located identifier

    edgesWithTypes =
      (,) <$> (edgesSameType <|> pure AnonymousEdge) <*> (reservedOp ":" *> located identifier)

    edgesSameType =
      NamedEdges <$> many1 (located identifier)

morphismDecl :: Stream s Identity Char => Parsec s u MorphismDeclaration
morphismDecl = (mapping <?> "mapping of elements") <* optional semi
  where
    mapping = DeclMapping
      <$> many1 (located identifier)
      <*> (reservedOp "->" *> located identifier <* optional semi)

ruleDecl :: Stream s Identity Char => Parsec s u RuleDeclaration
ruleDecl = choice
  [ reserved "match" >> DeclMatch <$> graphFragment <?> "match"
  , reserved "forbid" >> DeclForbid <$> optionMaybe (located identifier) <*> graphFragment <?> "forbid"
  , reserved "create" >> DeclCreate <$> graphFragment <?> "create"
  , reserved "delete" >> DeclDelete <$> many1 (located identifier) <*> deleteMode <?> "delete"
  , clone <?> "clone"
  , join <?> "join"
  ] <* optional semi
  where
    clone = DeclClone
      <$> (reserved "clone" *> located identifier)
      <*> (reserved "as" *> many1 (located identifier))

    join = DeclJoin
      <$> (reserved "join" *> many2 (located identifier))
      <*> optionMaybe (reserved "as" *> located identifier)
      where many2 p = many1 p >>= \elems ->
              if length elems < 2
              then fail $ "Expected at least two elements to be joined" ++ show elems
              else return elems

    deleteMode =
      (reserved "with" *> reserved "matched" *> reserved "edges" $> WithMatchedEdges)
      <|> pure Isolated

    graphFragment = braces (many graphDecl) <|> ((:[]) <$> graphDecl)


lexer :: Stream s Identity Char => P.GenTokenParser s u Identity
lexer =
  P.makeTokenParser langDef

located :: Stream s Identity Char => Parsec s u a -> Parsec s u (Located a)
located parser = do
  pos <- getPosition
  val <- parser
  let pos' = Position (sourceLine pos) (sourceColumn pos)
  return (val `at` Location (sourceName pos) pos')

braces :: Stream s Identity Char => Parsec s u a -> Parsec s u a
braces = P.braces lexer

semi :: Stream s Identity Char => Parsec s u String
semi = P.semi lexer

commaSep1 :: Stream s Identity Char => Parsec s u a -> Parsec s u [a]
commaSep1 = P.commaSep1 lexer

reserved, reservedOp :: Stream s Identity Char => String -> Parsec s u ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

identifier :: Stream s Identity Char => Parsec s u Text
identifier = Text.pack <$> P.identifier lexer

filePath :: Stream s Identity Char => Parsec s u FilePath
filePath = P.stringLiteral lexer

whiteSpace :: Stream s Identity Char => Parsec s u ()
whiteSpace = P.whiteSpace lexer

-- | Given any string, replace illegal characters to make it a valid identifier.
makeIdentifier :: String -> String
makeIdentifier "" = "_"
makeIdentifier (c:cs) = prefix c ++ map escapeInvalid (c:cs)
  where
    prefix c
      | Char.isLetter c || c == '_' = ""
      | otherwise = "_"
    escapeInvalid c
      | Char.isAlphaNum c || c == '_' || c == '\'' = c
      | otherwise = '_'

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
        reservedNames

    , P.caseSensitive =
        True
    }

reservedNames :: [String]
reservedNames = words
  " graph morphism node edge type rule \
  \ match forbid create delete with matched edges clone join as "
