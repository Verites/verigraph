{-# LANGUAGE OverloadedStrings #-}
module SymbolicGraph.DataAlgebra.PrettyPrint where

import           Prelude                          hiding ((<$>))

import           Data.String                      (fromString)
import qualified Data.Text.Lazy                   as LazyText
import           SymbolicGraph.DataAlgebra.Syntax
import           Text.PrettyPrint.Leijen.Text


instance Pretty Restriction where
  pretty (Restriction pred e1 e2) =
    pretty e1 <+> pretty pred <+> pretty e2

  prettyList restrictions =
    braces . align $ vcat [ pretty r <> semi | r <- restrictions ]


instance Pretty Predicate where
  pretty Equal     = text "=="
  pretty NotEqual  = text "!="
  pretty LessEqual = text "<="
  pretty Less      = text "<"


instance Pretty Expr where
  pretty =
    prettyMixfix 0


prettyMixfix :: Int -> Expr -> Doc
prettyMixfix d expr =
  case expr of
    EVariable var ->
      text (LazyText.fromStrict var)

    EConstant k ->
      integer k

    EApplication operation args ->
      prettyApplication d operation args


prettyApplication :: Int -> Operation -> [Expr] -> Doc
prettyApplication d operation args =
  case (kindOf operation, args) of
    (PrefixOp, [e]) ->
      prettyOp operation <> prettyMixfix 11 e

    (InfixOp prec AssocLeft, [e1, e2]) ->
      parensWhen (d > prec) $
        prettyMixfix prec e1 <+> prettyOp operation <+> prettyMixfix (prec + 1) e2

    (InfixOp prec AssocRight, [e1, e2]) ->
      parensWhen (d > prec) $
        prettyMixfix (prec + 1) e1 <+> prettyOp operation <+> prettyMixfix prec e2

    (InfixOp prec AssocNone, [e1, e2]) ->
      parensWhen (d > prec) $
        prettyMixfix (prec + 1) e1 <+> prettyOp operation <+> prettyMixfix (prec + 1) e2

    (FunctOp _, _) ->
      prettyOp operation <> tupled [ prettyMixfix 0 arg | arg <- args ]

    _ ->
      let (Left cause) = typeCheck (EApplication operation args)
      in error $ "Invalid expression: " ++ cause

  where
    prettyOp = text . fromString . nameOf



parensWhen :: Bool -> Doc -> Doc
parensWhen True  = align . parens
parensWhen False = id
