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
import           Test.Hspec.QuickCheck     (modifyMaxSize, modifyMaxSuccess)
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

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

parse :: String -> Either Error [TopLevelDeclaration]
parse = runIdentity . runExceptT . parseModule "<test>"

shouldParseTo :: String -> [TopLevelDeclaration] -> Expectation
shouldParseTo src expected =
  case parse src of
    Left err -> expectationFailure $ show (prettyError err) ++ "\n" ++ src
    Right val -> val `shouldBe` expected

spec :: Spec
spec = do

  modifyMaxSuccess (const 50) . modifyMaxSize (const 10) .
    it "always parses the result of pretty printing" .
    property $ \topLevel ->
      let printed = show . vsep . map pretty $ topLevel
      in counterexample printed $ case parse printed of
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
        , DeclEdges "n1" [(NamedEdges ["e1"], "E")] "n1"
        , DeclEdges "n2" [(NamedEdges ["e2"], "E")] "n1"
        ]
      ]

  it "parses a graph with multiple nodes and edges" $
    "graph g { n1 n2 : N; n1 -e1 e2:E-> n1; n2 -e3:E, e4:E'-> n1 }" `shouldParseTo`
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" [(NamedEdges ["e1", "e2"], "E")] "n1"
        , DeclEdges "n2" [(NamedEdges ["e3"], "E"), (NamedEdges ["e4"], "E'")] "n1"
        ]
      ]

  it "parses a graph with anonymous edges" $
    "graph g { n1 n2 : N; n1 -:E-> n1; n2 -:E,:E'-> n1 }" `shouldParseTo`
      [ DeclGraph "g"
        [ DeclNodes ["n1", "n2"] "N"
        , DeclEdges "n1" [(AnonymousEdge, "E")] "n1"
        , DeclEdges "n2" [(AnonymousEdge, "E"), (AnonymousEdge, "E'")] "n1"
        ]
      ]

  it "parses a morphism with individual and group mappings" $
    "morphism f : dom -> cod { n1 -> m1; e1 e2 -> e12 }" `shouldParseTo`
      [ DeclMorphism "f" "dom" "cod"
        [ DeclMapping ["n1"] "m1"
        , DeclMapping ["e1", "e2"] "e12"
        ]
      ]

  it "parses a rule with single and block matches" $
    "rule r { match n1 : N; match n1-:E->n1; match { n2:N; n1-:E->n2; } }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclMatch [DeclNodes ["n1"] "N"]
        , DeclMatch [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
        , DeclMatch [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
        ]
      ]

  it "parses a rule with named and anonymous forbids" $
    "rule r { forbid {n1 : N}; forbid nodeAndEdge { n2:N; n1-:E->n2; } }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclForbid Nothing [DeclNodes ["n1"] "N"]
        , DeclForbid "nodeAndEdge" [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
        ]
      ]

  it "parses a rule with single and block creates" $
    "rule r { create n1 : N; create n1-:E->n1; create { n2:N; n1-:E->n2; } }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclCreate [DeclNodes ["n1"] "N"]
        , DeclCreate [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
        , DeclCreate [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
        ]
      ]

  it "parses a rule with deletes" $
    "rule r { delete n1; delete n3 n4; delete n5 with matched edges; delete n6 n7 with matched edges }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclDelete ["n1"] Isolated
        , DeclDelete ["n3", "n4"] Isolated
        , DeclDelete ["n5"] WithMatchedEdges
        , DeclDelete ["n6", "n7"] WithMatchedEdges
        ]
      ]

  it "parses a rule with clones" $
    "rule r { clone n1 as n1'; clone n2 as n3 n4 n5 }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclClone "n1" ["n1'"]
        , DeclClone "n2" ["n3", "n4", "n5"]
        ]
      ]

  it "parses a rule with joined elements" $
    "rule r { join n1 n2 as n; join n3 n4 n5 }" `shouldParseTo`
      [ DeclRule "r"
        [ DeclJoin ["n1", "n2"] "n"
        , DeclJoin ["n3", "n4", "n5"] Nothing ]
      ]
