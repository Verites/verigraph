{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GrLang.Parser where

import           Data.Functor.Identity
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Text.Parsec
import qualified Text.Parsec.Token     as P

import           Data.Graphs           (Edge (..), EdgeId, Graph, Node (..), NodeId)
import qualified Data.Graphs           as Graph
import           Data.TypedGraph
import           GrLang.Metadata


type Parser a = Parsec String ParserState a


data ParserState = St
  { currTypeGraph :: TypeGraph
  , nodeTypes     :: Map Text NodeId
  , edgeTypes     :: Map (Text, NodeId, NodeId) EdgeId
  }

findNodeType name = do
  st <- getState
  return $ do
    nodeId <- Map.lookup name (nodeTypes st)
    Graph.lookupNode nodeId (currTypeGraph st)

findEdgeType name srcId tgtId = do
  st <- getState
  return $ do
    edgeId <- Map.lookup (name, srcId, tgtId) (edgeTypes st)
    Graph.lookupEdge edgeId (currTypeGraph st)

parseTypeGraph :: SourceName -> String -> Either ParseError TypeGraph
parseTypeGraph = runParser (whiteSpace *> pTypeGraph <* eof) (St Graph.empty Map.empty Map.empty)


pTypeGraph :: Parser TypeGraph
pTypeGraph = many (nodeType <|> edgeType) *> (currTypeGraph <$> getState)

nodeType :: Parser ()
nodeType = do
  pos <- getPosition
  typeName <- reserved "node" *> reserved "type" *> identifier
  existingNodeType <- findNodeType typeName
  case existingNodeType of
    Just (Node _ maybeData) ->
      fail $ "Duplicate declaration of node type " ++ show typeName ++
        case maybeData of
          Just n -> ", already declared at " ++ show (sourcePos n)
          Nothing -> ""
    Nothing -> do
      st <- getState
      let id:_ = Graph.newNodes (currTypeGraph st)
      setState $ st
        { currTypeGraph = Graph.insertNodeWithPayload id (Just $ Metadata typeName pos) (currTypeGraph st)
        , nodeTypes = Map.insert typeName id (nodeTypes st)
        }

edgeType :: Parser ()
edgeType = do
  pos <- getPosition
  typeName <- reserved "edge" *> reserved "type" *> identifier
  (Node srcType _) <- reservedOp ":" *> existingNodeType
  (Node tgtType _) <- reservedOp "->" *> existingNodeType

  existingEdgeType <- findEdgeType typeName srcType tgtType
  case existingEdgeType of
    Just (Edge _ _ _ maybeData) ->
      fail $ "Duplicate declaration of edge type " ++ show typeName ++
        case maybeData of
          Just e -> ", already declared at " ++ show (sourcePos e)
          Nothing -> ""
    Nothing -> do
      st <- getState
      let id:_ = Graph.newEdges (currTypeGraph st)
      setState $ st
        { currTypeGraph = Graph.insertEdgeWithPayload id srcType tgtType (Just $ Metadata typeName pos) (currTypeGraph st)
        , edgeTypes = Map.insert (typeName, srcType, tgtType) id (edgeTypes st)
        }

existingNodeType :: Parser (Node (Maybe Metadata))
existingNodeType = do
  name <- identifier
  maybeType <- findNodeType name
  case maybeType of
    Just node -> return node
    Nothing -> fail $ "Undefined node type " ++ show name

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


identifier :: Parser Text
identifier =
  Text.pack <$> P.identifier lexer


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
        letter <|> oneOf "_"

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
