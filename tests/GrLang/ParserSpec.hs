{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GrLang.ParserSpec where

import           Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Data.Text.Prettyprint.Doc (Pretty(..), vsep)
import Data.String

import Base.Annotation (Annotated(..), Located)
import           GrLang.AST
import           GrLang.Parser
import           GrLang.QuickCheck ()

loc :: a -> Located a
loc = A Nothing

instance IsString a => IsString (Located a) where
  fromString = loc . fromString

spec :: Spec
spec = do

  modifyMaxSuccess (const 50) $
    it "always parses the result of pretty printing" $
    property $ \topLevel -> 
      parseTopLevel "" (show . vsep . map pretty $ topLevel) == Right topLevel

  it "parses node and edge types" $
    parseTopLevel "" "node type foo edge type bar : foo -> foo" `shouldBe` Right 
      [ DeclNodeType "foo"
      , DeclEdgeType "bar" "foo" "foo"
      ]

  it "parses a graph with individual nodes and edges" $
    parseTopLevel "" "graph g { n1 : N n2 : N n1 -e1:E-> n1 n2 -e2:E-> n1 }" `shouldBe` Right 
      [ DeclGraph "g"
        [ DeclNodes ["n1"] "N"
        , DeclNodes ["n2"] "N"
        , DeclEdges "n1" (MultipleTypes [loc (Just "e1", "E")]) "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Just "e2", "E")]) "n1"
        ]
      ]
      
  it "parses a graph with multiple nodes and edges" $
    parseTopLevel "" "graph g { n1, n2 : N n1 -e1,e2:E-> n1 n2 -e3:E,e4:E'-> n1 }" `shouldBe` Right 
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" (SingleType ["e1", "e2"] "E") "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Just "e3", "E"), loc (Just "e4", "E'")]) "n1"
        ]
      ]
      
  it "parses a graph with anonymous edges" $
    parseTopLevel "" "graph g { n1, n2 : N n1 -:E-> n1 n2 -:E,:E'-> n1 }" `shouldBe` Right 
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" (MultipleTypes [loc (Nothing, "E")]) "n1"
        , DeclEdges "n2" (MultipleTypes [loc (Nothing, "E"), loc (Nothing, "E'")]) "n1"
        ]
      ]
