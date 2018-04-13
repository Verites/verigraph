{-# LANGUAGE OverloadedStrings #-}
module GrLang.AST
  ( TopLevelDeclaration(..)
  , GraphDeclaration(..)
  , MorphismDeclaration(..)
  , RuleDeclaration(..)
  , DeletionMode(..)
  , ParallelEdgesDeclaration(..)
  ) where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc as PP

import           Base.Annotation           (Annotated (..), Located)
import qualified Base.Annotation           as Ann

data TopLevelDeclaration
  = Import (Located FilePath) -- ^ Declares that another file should be imported, with a path relative to the current file.
  | DeclNodeType (Located Text) -- ^ Declares a node type.
  | DeclEdgeType (Located Text) (Located Text) (Located Text) -- ^ Declares an edge type with source and target node types.
  | DeclGraph (Located Text) [GraphDeclaration] -- ^ Declares a graph whose elements are declared in its body.
  | DeclMorphism (Located Text) (Located Text) (Located Text) [MorphismDeclaration] -- ^ Declares a morphism with a named domain and codomain, and mappings declared in its body.
  | DeclRule (Located Text) [RuleDeclaration] -- ^ Declares a rule whose elements are declared in its body.
  deriving (Eq, Show)

data GraphDeclaration
  = DeclNodes [Located Text] (Located Text) -- ^ Declares nodes of a given type, with the given names.
  | DeclEdges (Located Text) [(ParallelEdgesDeclaration, Located Text)] (Located Text) -- ^ Declares edges with given source and targets.
  deriving (Eq, Show)

data MorphismDeclaration
  = DeclMapping [Located Text] (Located Text) -- ^ Declares that a group of elements of the domain is mapped to an element of the codomain.
  deriving (Eq, Show)

data ParallelEdgesDeclaration = AnonymousEdge | NamedEdges [Located Text]
  deriving (Eq, Show)

data RuleDeclaration
  = DeclMatch [GraphDeclaration] -- ^ Declares elements that must be matched by the rule.
  | DeclForbid (Maybe (Located Text)) [GraphDeclaration] -- ^ Declares elements that must not be matched by the rule.
  | DeclCreate [GraphDeclaration] -- ^ Declares elements that will be created by the rule.
  | DeclDelete [Located Text] DeletionMode -- ^ Declares elements that were matched and will be deleted by the rule.
  | DeclClone (Located Text) [Located Text] -- ^ Declares clones of a matched element with given names.
  | DeclJoin [Located Text] (Maybe (Located Text)) -- ^ Declares that matched elements will be joined into a single one.
  deriving (Eq, Show)

-- | When deleting nodes, they may be deleted with all incident matched edges.
-- If this is not explicitly declared, and the node is not isolated, then the
-- rule declaration is invalid.
data DeletionMode = Isolated | WithMatchedEdges
  deriving (Eq, Show)


instance Pretty TopLevelDeclaration where
  pretty (Import (A _ p)) = "import" <+> PP.dquotes (pretty p)
  pretty (DeclNodeType (A _ n)) = "node type" <+> pretty n
  pretty (DeclEdgeType (A _ e) (A _ src) (A _ tgt)) =
    PP.hsep ["edge type", pretty e, ":", pretty src, "->", pretty tgt]
  pretty (DeclGraph name body) = namedBlock "graph" name (map pretty body)
  pretty (DeclMorphism (A _ name) (A _ domName) (A _ codName) body) =
    block (PP.hsep ["morphism", pretty name, ":", pretty domName, "->", pretty codName]) (map pretty body)
  pretty (DeclRule name body) = namedBlock "rule" name (map pretty body)

instance Pretty GraphDeclaration where
  pretty (DeclNodes [] _) = PP.emptyDoc
  pretty (DeclNodes ns (A _ t)) = PP.group . PP.nest 4 . PP.fillSep $
    [ pretty n | A _ n <- ns ] ++ [":" <+> pretty t]
  pretty (DeclEdges (A _ src) edgeDecls (A _ tgt))
    | null nonEmptyDecls =  PP.emptyDoc
    | otherwise = PP.group . PP.nest 4 . PP.fillSep $
        [ pretty src <+> "-" ]
        ++ PP.punctuate "," (map prettyEdges nonEmptyDecls)
        ++ [ "->" <+> pretty tgt ]
    where
      nonEmptyDecls = filter (not . isEmpty) edgeDecls
      isEmpty (AnonymousEdge, _) = False
      isEmpty (NamedEdges es, _) = null es

      prettyEdges (AnonymousEdge, A _ t) = ":" <> pretty t
      prettyEdges (NamedEdges es, A _ t) = PP.hsep $ [ pretty e | A _ e <- es ] ++ [":" <+> pretty t]

instance Pretty MorphismDeclaration where
  pretty (DeclMapping [] _) = PP.emptyDoc
  pretty (DeclMapping froms (A _ to)) = PP.hsep [ pretty e | A _ e <- froms ] <+> "->" <+> pretty to

instance Pretty RuleDeclaration where
  pretty (DeclMatch elems) = blockOrSingle "match" (map pretty elems)
  pretty (DeclForbid maybeName elems) = block ("forbid" <> name) (map pretty elems)
    where name = case maybeName of
            Nothing -> PP.emptyDoc
            Just (A _ name) -> PP.space <> pretty name
  pretty (DeclCreate elems) = blockOrSingle "create" (map pretty elems)
  pretty (DeclDelete [] _) = PP.emptyDoc
  pretty (DeclDelete names mode) = PP.fillSep $ "delete" : map (pretty . Ann.drop) names ++ prettyMode mode
    where
      prettyMode Isolated         = []
      prettyMode WithMatchedEdges = ["with matched edges"]
  pretty (DeclClone (A _ name) clones) =
    PP.fillSep $ "clone" : pretty name : "as" : map (pretty . Ann.drop) clones
  pretty (DeclJoin [] _) = PP.emptyDoc
  pretty (DeclJoin [_] _) = PP.emptyDoc
  pretty (DeclJoin joined name) = PP.fillSep $ "join" : prettyJoined ++ prettyName
    where
      prettyJoined = map (pretty . Ann.drop) joined
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

blockOrSingle :: Doc ann -> [Doc ann] -> Doc ann
blockOrSingle _ []        = PP.emptyDoc
blockOrSingle prefix [x]  = prefix <+> x
blockOrSingle prefix body = block prefix body
