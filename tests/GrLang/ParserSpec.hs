{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GrLang.ParserSpec where

import           Control.Monad.Except      (runExceptT)
import           Data.Functor.Identity
import           Data.String               (IsString (..))
import           Data.Text                 ()
import           Data.Text.Prettyprint.Doc (Pretty (..), vsep)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (modifyMaxSuccess)
import           Test.QuickCheck

import           Base.Annotation           (Annotated (..), Located)
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.TestUtils

loc :: a -> Located a
loc = A Nothing

instance IsString a => IsString (Located a) where
  fromString = loc . fromString

parse :: String -> Either Error [TopLevelDeclaration]
parse = runIdentity . runExceptT . parseModule "<test>"

shouldParseTo :: String -> [TopLevelDeclaration] -> Expectation
shouldParseTo src expected =
  case parse src of
    Left err -> failWithError err
    Right val -> val `shouldBe` expected

spec :: Spec
spec = do

  modifyMaxSuccess (const 50) $
    it "always parses the result of pretty printing" $
    property $ \topLevel ->
      let printed = show . vsep . map pretty $ topLevel
      in case parse printed of
        Left errs -> quickCheckError errs
        Right result -> property $ result == topLevel

  it "parses node and edge types" $
    "node type foo; edge type bar : foo -> foo" `shouldParseTo`
      [ DeclNodeType "foo"
      , DeclEdgeType "bar" "foo" "foo"
      ]

  it "parses a graph with individual nodes and edges" $
    "graph g { n1 : N; n2 : N; n1 -e1:E-> n1; n2 -e2:E-> n1 }" `shouldParseTo`
      [ DeclGraph "g"
        [ DeclNodes ["n1"] "N"
        , DeclNodes ["n2"] "N"
        , DeclEdges "n1" (MultipleTypes [loc (Just "e1", "E")]) "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Just "e2", "E")]) "n1"
        ]
      ]

  it "parses a graph with multiple nodes and edges" $
    "graph g { n1, n2 : N; n1 -e1,e2:E-> n1; n2 -e3:E,e4:E'-> n1 }" `shouldParseTo`
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" (SingleType ["e1", "e2"] "E") "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Just "e3", "E"), loc (Just "e4", "E'")]) "n1"
        ]
      ]

  it "parses a graph with anonymous edges" $
    "graph g { n1, n2 : N; n1 -:E-> n1; n2 -:E,:E'-> n1 }" `shouldParseTo`
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" (MultipleTypes [loc (Nothing, "E")]) "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Nothing, "E"), loc (Nothing, "E'")]) "n1"
        ]
      ]
