{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module SymbolicGraph.DataAlgebra.Solver.SmtLib (makeSmtLibSolver) where

import           SymbolicGraph.DataAlgebra
import           SymbolicGraph.DataAlgebra.Solver

import qualified Data.List                        as List
import qualified Data.Set                         as Set

import           Data.Monoid
import qualified Data.Text                        as T
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy.Builder           as TB


makeSmtLibSolver :: (Text -> IO (Maybe Bool)) -> Solver
makeSmtLibSolver checkSat =
  let
    checkSatisfability restrictions =
      let
        variables =
          freeVariablesOf restrictions
      in
        checkSat . concatLines $
          [ TB.fromText "(set-logic QF_NIA)" ]
          ++ map (\var -> declareConst var "Int") (Set.toList variables)
          ++ map assert restrictions
          ++ [ TB.fromText "(check-sat)"
             , TB.fromText "(exit)"
             ]

    checkStrengthening weaker stronger =
      let
        variables =
          freeVariablesOf (weaker, stronger)

        premises = stronger

        conclusion =
          makeSExpr $ TB.fromText "and" : map toSExpr weaker

        negatedConclusion =
          makeSExpr [ TB.fromText "not", conclusion ]
      in
        fmap (fmap not) . checkSat . concatLines $
          [ TB.fromText "(set-logic QF_NIA)" ]
          ++ map (\var -> declareConst var "Int") (Set.toList variables)
          ++ map assert premises
          ++ [ assert negatedConclusion
             , TB.fromText "(check-sat)"
             , TB.fromText "(exit)"
             ]

  in
    Solver checkSatisfability checkStrengthening



makeSExpr :: [TB.Builder] -> TB.Builder
makeSExpr items =
    TB.singleton '(' <> concatSpaces items <> TB.singleton ')'
  where
    concatSpaces = mconcat . List.intersperse (TB.singleton ' ')


declareConst :: T.Text -> T.Text -> TB.Builder
declareConst name typ =
  makeSExpr [TB.fromText "declare-const", TB.fromText name, TB.fromText typ]


assert :: ToSExpr a => a -> TB.Builder
assert expr =
  makeSExpr [TB.fromText "assert", toSExpr expr]


concatLines :: [TB.Builder] -> Text
concatLines =
   TB.toLazyText . mconcat . List.intersperse (TB.singleton '\n')



class ToSExpr a where
  toSExpr :: a -> TB.Builder



instance ToSExpr Restriction where

  toSExpr (Restriction predicate lhs rhs) =
    case predicate of
      Equal ->
        makeSExpr [ TB.singleton '=', toSExpr lhs, toSExpr rhs ]

      NotEqual ->
        makeSExpr [ TB.fromText "not"
                  , makeSExpr [ TB.singleton '=', toSExpr lhs, toSExpr rhs ] ]

      Less ->
        makeSExpr [ TB.singleton '<', toSExpr lhs, toSExpr rhs ]

      LessEqual ->
        makeSExpr [ TB.fromText "<=", toSExpr lhs, toSExpr rhs ]



instance ToSExpr Expr where

  toSExpr expr =
    case expr of
      EVariable var ->
        TB.fromText var

      EConstant n ->
        TB.fromString (show n)

      EApplication operation args ->
        makeSExpr (toSExpr operation : map toSExpr args)



instance ToSExpr TB.Builder where
  toSExpr = id



instance ToSExpr Operation where
  toSExpr OpAdd = TB.singleton '+'
  toSExpr OpSub = TB.singleton '-'
  toSExpr OpMul = TB.singleton '*'
