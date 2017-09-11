module GrLang.Metadata where

import           Data.Text       (Text)
import           Text.Parsec.Pos

import           Data.Graphs     (Graph)

data Metadata = Metadata
  { name      :: Text
  , sourcePos :: SourcePos }

type TypeGraph = Graph (Maybe Metadata) (Maybe Metadata)
