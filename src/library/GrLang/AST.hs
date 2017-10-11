{-# LANGUAGE OverloadedStrings #-}
module GrLang.AST where

import           Prelude                   hiding (drop)

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..), (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc as PP

data Position =
  Position { line :: Int, column :: Int }

data Annotated annotation a =
  A annotation a

instance Functor (Annotated annotation) where
  fmap f (A ann x) = A ann (f x)

drop :: Annotated annotation a -> a
drop (A _ x) = x

type Located a =
  Annotated (Position, Maybe FilePath) a

data TopLevelDeclaration
  = Import FilePath
  | DeclNodeType Text
  | DeclEdgeType Text Text Text
  | DeclGraph Text [Located GraphDeclaration]

data GraphDeclaration
  = DeclNodes [Located Text] (Located Text)
  | DeclEdges (Located Text) [Located (Text, Text)] (Located Text)

data EdgesDeclaration
  = SingleType [Located Text] (Located Text)
  | MultipleTypes [Located (Text, Text)]

instance Pretty TopLevelDeclaration where
  pretty (Import p) = "import" <+> PP.dquotes (pretty p)
  pretty (DeclNodeType n) = "node type" <+> pretty n
  pretty (DeclEdgeType e src tgt) = PP.hsep ["edge type", pretty e, ":", pretty src, "->", pretty tgt]
  pretty (DeclGraph name body) = PP.hsep ["graph", pretty name, PP.braces (PP.hang 2 prettyBody)]
    where prettyBody = PP.line <+> PP.vsep (map (pretty . drop) body)

instance Pretty GraphDeclaration where
  pretty (DeclNodes [] _) = PP.emptyDoc
  pretty (DeclNodes ns t) = PP.group . PP.nest 2 . PP.vsep $ nodeNames ++ typeSig
    where
      nodeNames = PP.punctuate ", " . map (pretty . drop) $ ns
      typeSig = [":" <+> pretty (drop t)]
  pretty (DeclEdges _ [] _) = PP.emptyDoc
  pretty (DeclEdges (A _ src) es (A _ tgt)) =
    PP.sep [ pretty src
        , PP.align $ PP.vsep [ "-" <+> PP.align (prettyEdges es), "->" <+> pretty tgt ] ]
    where
      prettyEdges = PP.vsep . prettyEdges' . map drop
      prettyEdges' [] = []
      prettyEdges' edges@((_,t):_)
        | all ((==t) . snd) edges = PP.punctuate "," [ pretty e | (e,_) <- edges ] ++ [":" <+> pretty t]
        | otherwise               = PP.punctuate ", " [ pretty e <> ":" <> pretty t | (e,t) <- edges ]
