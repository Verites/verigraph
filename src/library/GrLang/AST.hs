{-# LANGUAGE OverloadedStrings #-}
module GrLang.AST where

import           Prelude                   hiding (drop)

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..), (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc as PP

import Base.Annotation (Annotated(..), Located)


data TopLevelDeclaration
  = Import (Located FilePath)
  | DeclNodeType (Located Text)
  | DeclEdgeType (Located Text) (Located Text) (Located Text)
  | DeclGraph (Located Text) [GraphDeclaration]
  deriving (Eq, Show)

data GraphDeclaration
  = DeclNodes [Located Text] (Located Text)
  | DeclEdges (Located Text) ParallelEdgesDeclaration (Located Text)
  deriving (Eq, Show)

data ParallelEdgesDeclaration
  = SingleType [Located Text] (Located Text)
  | MultipleTypes [Located (Maybe Text, Text)]
  deriving (Eq, Show)


instance Pretty TopLevelDeclaration where
  pretty (Import (A _ p)) = "import" <+> PP.dquotes (pretty p)
  pretty (DeclNodeType (A _ n)) = "node type" <+> pretty n
  pretty (DeclEdgeType (A _ e) (A _ src) (A _ tgt)) = 
    PP.hsep ["edge type", pretty e, ":", pretty src, "->", pretty tgt]
  pretty (DeclGraph (A _ name) body) = PP.vsep 
    [ "graph" <+> pretty name <+> "{"
    , PP.indent 2 prettyBody
    , "}" ]
    where prettyBody = PP.vsep (map pretty body)

instance Pretty GraphDeclaration where
  pretty (DeclNodes [] _) = PP.emptyDoc
  pretty (DeclNodes ns (A _ t)) = PP.group . PP.nest 4 . PP.vsep $ nodeNames ++ typeSig
    where
      nodeNames = PP.punctuate "," [ pretty n | A _ n <- ns ]
      typeSig = [":" <+> pretty t]
  pretty (DeclEdges _ (SingleType [] _) _) = PP.emptyDoc
  pretty (DeclEdges _ (MultipleTypes []) _) = PP.emptyDoc
  pretty (DeclEdges (A _ src) es (A _ tgt)) =
    PP.group . PP.nest 4 . PP.vsep $
      [ pretty src <+> "-" ]
      ++ prettyEdges es
      ++ [ "->" <+> pretty tgt ]
    where
      prettyEdges (SingleType es (A _ t)) = PP.punctuate "," [ pretty e | A _ e <- es ] ++ [":" <+> pretty t]
      prettyEdges (MultipleTypes es) = PP.punctuate ", " [ prettyEdge e | A _ e <- es ]
      prettyEdge (Just e, t) = pretty e <> ":" <> pretty t
      prettyEdge (Nothing, t) = ":" <> pretty t
