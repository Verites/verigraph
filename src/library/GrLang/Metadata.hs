module GrLang.Metadata where

import           Data.Text       (Text)
import           Text.Parsec.Pos

import           Base.Annotation (Position (..))
import           Data.Graphs     (Edge (..), Graph, Node (..))

data Metadata = Metadata
  { name      :: Maybe Text
  , sourcePos :: Maybe (Position, FilePath)  }

type TypeGraph = Graph (Maybe Metadata) (Maybe Metadata)
type NodeType = Node (Maybe Metadata)
type EdgeType = Edge (Maybe Metadata)
