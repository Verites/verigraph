module Logic.Ctl.Base
  ( Expr(..)
  , PathQuantified(..)
  , StateQuantified(..)
  ) where

import Text.PrettyPrint.Leijen as PP


data Expr
  = Literal Bool
  | Atom String
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Implies Expr Expr
  | Equiv Expr Expr
  | Temporal (PathQuantified Expr)
  deriving (Eq, Show, Read)


data PathQuantified e
  = A (StateQuantified e)
  | E (StateQuantified e)
  deriving (Eq, Show, Read)


data StateQuantified e
  = X e
  | G e
  | F e
  | U e e
  deriving (Eq, Show, Read)


instance Pretty Expr where
  pretty =
    ppImplicative


ppImplicative :: Expr -> Doc
ppImplicative (Implies e1 e2) =
  let
    ppImplication (Implies e1 e2) =
      ppBoolean e1 </> text "->" <+> ppImplication e2

    ppImplication e =
      ppBoolean e
  in
    ppBoolean e1 </> align (text "->" <+> ppImplication e2)

ppImplicative (Equiv e1 e2) =
  ppInfix (ppBoolean e1) "<->" (ppBoolean e2)

ppImplicative e =
  ppBoolean e


ppBoolean :: Expr -> Doc
ppBoolean (And e1 e2) =
  let
    ppAnd (And e1 e2) =
      ppUnary e1 </> text "&&" <+> ppAnd e2

    ppAnd e =
      ppUnary e
  in
    ppUnary e1 </> align (text "&&" <+> ppAnd e2)

ppBoolean (Or e1 e2) =
  let
    ppOr (Or e1 e2) =
      ppUnary e1 </> text "||" <+> ppOr e2

    ppOr e =
      ppUnary e
  in
    ppUnary e1 </> align (text "||" <+> ppOr e2)

ppBoolean e =
  ppUnary e


ppUnary :: Expr -> Doc
ppUnary (Not e) =
  text "~" <> ppUnary e

ppUnary (Temporal e) =
  ppTemporal e

ppUnary e =
  ppAtomic e


ppAtomic :: Expr -> Doc
ppAtomic (Literal True) =
  text "true"

ppAtomic (Literal False) =
  text "false"

ppAtomic (Atom prop) =
  text prop

ppAtomic e =
  parens (pretty e)


ppTemporal :: PathQuantified Expr -> Doc
ppTemporal (A e) =
  ppTemporal' 'A' e

ppTemporal (E e) =
  ppTemporal' 'E' e


ppTemporal' :: Char -> StateQuantified Expr -> Doc
ppTemporal' pathQuant (X e) =
  text (pathQuant:"X") <+> ppUnary e

ppTemporal' pathQuant (F e) =
  text (pathQuant:"F") <+> ppUnary e

ppTemporal' pathQuant (G e) =
  text (pathQuant:"G") <+> ppUnary e

ppTemporal' pathQuant (U e1 e2) =
  let
    infixExpr = pretty e1 </> char 'U' <+> pretty e2
    withBrackets = align . brackets
  in
    char pathQuant <> withBrackets infixExpr


ppInfix :: Doc -> String -> Doc -> Doc
ppInfix e1 op e2 =
  e1 </> text op <+> e2
