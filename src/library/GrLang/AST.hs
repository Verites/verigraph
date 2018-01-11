{-# LANGUAGE OverloadedStrings #-}
module GrLang.AST where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc as PP

import           Base.Annotation           (Annotated (..), Located)
import qualified Base.Annotation           as Ann

data Command
  = CDecl TopLevelDeclaration
  | CSaveDot (Located Text) (Located FilePath)
  deriving (Eq, Show)

data TopLevelDeclaration
  = Import (Located FilePath)
  | DeclNodeType (Located Text)
  | DeclEdgeType (Located Text) (Located Text) (Located Text)
  | DeclGraph (Located Text) [GraphDeclaration]
  | DeclRule (Located Text) [RuleDeclaration]
  deriving (Eq, Show)

data GraphDeclaration
  = DeclNodes [Located Text] (Located Text)
  | DeclEdges (Located Text) ParallelEdgesDeclaration (Located Text)
  deriving (Eq, Show)

data RuleDeclaration
  = DeclMatch [GraphDeclaration]
  | DeclForbid (Maybe (Located Text)) [GraphDeclaration]
  | DeclCreate [GraphDeclaration]
  | DeclDelete [(Located Text, DeletionMode)]
  | DeclClone (Located Text) [Located Text]
  | DeclJoin [Located Text] (Maybe (Located Text))
  deriving (Eq, Show)

data DeletionMode = Isolated | WithMatchedEdges
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
  pretty (DeclGraph name body) = namedBlock "graph" name (map pretty body)
  pretty (DeclRule name body) = namedBlock "rule" name (map pretty body)

instance Pretty GraphDeclaration where
  pretty (DeclNodes [] _) = PP.emptyDoc
  pretty (DeclNodes ns (A _ t)) = PP.group . PP.nest 4 . PP.fillSep $ nodeNames ++ typeSig
    where
      nodeNames = PP.punctuate "," [ pretty n | A _ n <- ns ]
      typeSig = [":" <+> pretty t]
  pretty (DeclEdges _ (SingleType [] _) _) = PP.emptyDoc
  pretty (DeclEdges _ (MultipleTypes []) _) = PP.emptyDoc
  pretty (DeclEdges (A _ src) es (A _ tgt)) =
    PP.group . PP.nest 4 . PP.fillSep $
      [ pretty src <+> "-" ]
      ++ prettyEdges es
      ++ [ "->" <+> pretty tgt ]
    where
      prettyEdges (SingleType es (A _ t)) = PP.punctuate "," [ pretty e | A _ e <- es ] ++ [":" <+> pretty t]
      prettyEdges (MultipleTypes es) = PP.punctuate ", " [ prettyEdge e | A _ e <- es ]
      prettyEdge (Just e, t)  = pretty e <> ":" <> pretty t
      prettyEdge (Nothing, t) = ":" <> pretty t

instance Pretty RuleDeclaration where
  pretty (DeclMatch elems) = blockOrSingle "match" (map pretty elems)
  pretty (DeclForbid maybeName elems) = block ("forbid" <> name) (map pretty elems)
    where name = case maybeName of
            Nothing -> PP.emptyDoc
            Just (A _ name) -> PP.space <> pretty name
  pretty (DeclCreate elems) = blockOrSingle "create" (map pretty elems)
  pretty (DeclDelete []) = PP.emptyDoc
  pretty (DeclDelete names) = PP.fillSep $ "delete" : PP.punctuate "," (map prettyDeleted names)
    where
      prettyDeleted (A _ name, Isolated)         = pretty name
      prettyDeleted (A _ name, WithMatchedEdges) = pretty name <+> "with matched edges"
  pretty (DeclClone (A _ name) clones) =
    PP.fillSep $ "clone" : pretty name : "as" : PP.punctuate "," (map (pretty . Ann.drop) clones)
  pretty (DeclJoin [] _) = PP.emptyDoc
  pretty (DeclJoin [_] _) = PP.emptyDoc
  pretty (DeclJoin joined name) = PP.fillSep $ "join" : prettyJoined ++ prettyName
    where
      prettyJoined = PP.punctuate "," $ map (pretty . Ann.drop) joined
      prettyName = case name of
        Just (A _ n) -> ["as", pretty n]
        Nothing -> []

namedBlock :: (Pretty a) => Doc ann -> Located a -> [Doc ann] -> Doc ann
namedBlock kind (A _ name) = block (kind <+> pretty name)

block :: Doc ann -> [Doc ann] -> Doc ann
block prefix body = PP.vsep
  [ prefix <+> "{"
  , PP.indent 2 (PP.vsep body)
  , "}" ]

nonEmptyBlock :: Doc ann -> [Doc ann] -> Doc ann
nonEmptyBlock _ []        = PP.emptyDoc
nonEmptyBlock prefix body = block prefix body

blockOrSingle :: Doc ann -> [Doc ann] -> Doc ann
blockOrSingle _ []        = PP.emptyDoc
blockOrSingle prefix [x]  = prefix <+> x
blockOrSingle prefix body = block prefix body
