{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GrLang.Parser (parseModule, parseGraph) where

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
  , graph <?> "graph"
  , rule <?> "rule"
  ] <* optional semi
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

    rule =
      reserved "rule" >>
      DeclRule
        <$> located identifier
        <*> braces (many ruleDecl)

graphDecl :: Stream s Identity Char => Parsec s u GraphDeclaration
graphDecl = (located identifier >>= \n -> edge n <|> node n) <* optional semi <?> "node or edge"
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
      (,) <$> optionMaybe identifier <*> (reservedOp ":" *> identifier)

ruleDecl :: Stream s Identity Char => Parsec s u RuleDeclaration
ruleDecl = choice
  [ reserved "match" >> DeclMatch <$> graphFragment <?> "match"
  , reserved "forbid" >> DeclForbid <$> optionMaybe (located identifier) <*> graphFragment <?> "forbid"
  , reserved "create" >> DeclCreate <$> graphFragment <?> "create"
  , reserved "delete" >> DeclDelete <$> commaSep1 deletedElem <?> "delete"
  , clone <?> "clone"
  , join <?> "join"
  ] <* optional semi
  where
    clone = DeclClone
      <$> (reserved "clone" *> located identifier)
      <*> (reserved "as" *> commaSep1 (located identifier))

    join = DeclJoin
      <$> (reserved "join" *> commaSep2 (located identifier))
      <*> optionMaybe (reserved "as" *> located identifier)
      where commaSep2 p = commaSep1 p >>= \elems ->
              if length elems < 2
              then fail $ "Expected at least two elements to be joined" ++ show elems
              else return elems

    deletedElem = (,) <$> located identifier <*> deleteMode
    deleteMode =
      (reserved "with" >> reserved "matched" >> reserved "edges" *> pure WithMatchedEdges)
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

    , P.reservedNames = words
        " graph node edge type \
        \ match forbid create delete with matched edges clone "

    , P.caseSensitive =
        True
    }
