{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GrLang.CompilerSpec where

import qualified Data.Map        as Map
import           Data.String
import           Data.Text       (Text)
import           Test.Hspec

import           Base.Annotation (Annotated (..), Located)
import qualified Data.Graphs     as TypeGraph
import           Data.TypedGraph
import           GrLang.AST
import           GrLang.Compiler
import           GrLang.Metadata

loc :: a -> Located a
loc = A Nothing

instance IsString a => IsString (Located a) where
  fromString = loc . fromString

instance IsString (Maybe Text) where
  fromString = Just . fromString

spec :: Spec
spec = do
  let types =
        [ DeclNodeType "Revision"
        , DeclNodeType "Deps"
        , DeclEdgeType "Dep" "Deps" "Revision"
        , DeclEdgeType "Count" "Revision" "Revision" ]
  let tgraph = TypeGraph.fromNodesAndEdges
        [ Node 1 $ Just $ Metadata "Revision" Nothing, Node 2 $ Just $ Metadata "Deps" Nothing ]
        [ Edge 1 2 1 $ Just $ Metadata "Dep" Nothing, Edge 2 1 1 $ Just $ Metadata "Count" Nothing ]

  it "compiles a type graph" $ do
    result <- compile types
    case result of
      Left err -> expectationFailure (showErrors err)
      Right result' -> compiledTypeGraph result' `shouldBe` tgraph

  it "compiles a graph" $ do
    result <- compile $ types ++
      [ DeclGraph "g"
        [ DeclNodes ["r1", "r2"] "Revision"
        , DeclNodes ["d1", "d2"] "Deps"
        , DeclNodes ["r3"] "Revision"
        , DeclEdges "d1" (MultipleTypes [loc ("d12", "Dep")]) "r2"
        , DeclEdges "d2" (MultipleTypes [loc (Nothing, "Dep")]) "r3"
        , DeclEdges "r1" (SingleType ["c1", "c2", "c3"] "Count") "r2"
        ]
      ]
    let g = fromNodesAndEdges tgraph
          [ (Node 0 $ Just $ Metadata "r1" Nothing, 1), (Node 1 $ Just $ Metadata "r2" Nothing, 1)
          , (Node 2 $ Just $ Metadata "d1" Nothing, 2), (Node 3 $ Just $ Metadata "d2" Nothing, 2)
          , (Node 4 $ Just $ Metadata "r3" Nothing, 1) ]
          [ (Edge 0 2 1 $ Just $ Metadata "d12" Nothing, 1)
          , (Edge 1 3 4 $ Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 0 1 $ Just $ Metadata "c1" Nothing, 2), (Edge 3 0 1 $ Just $ Metadata "c2" Nothing, 2), (Edge 4 0 1 $ Just $ Metadata "c3" Nothing, 2)
          ]
    case result of
      Left err -> expectationFailure (showErrors err)
      Right result' -> do
        Map.keys (compiledGraphs result') `shouldBe` ["g"]
        compiledGraphs result' Map.! "g" `shouldBe` loc g
