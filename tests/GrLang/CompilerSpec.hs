{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GrLang.CompilerSpec where

import Data.Map                  (Map)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Data.String
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc as PP
import           Test.Hspec
import System.FilePath

import Base.Valid
import           Base.Annotation           (Annotated (..), Located)
import qualified Base.Annotation as Ann
import qualified Data.Graphs               as TypeGraph
import           Data.TypedGraph
import           GrLang.AST
import           GrLang.Compiler
import           GrLang.Monad
import           GrLang.Value

loc :: a -> Located a
loc = A Nothing

instance IsString a => IsString (Located a) where
  fromString = loc . fromString

instance IsString (Maybe Text) where
  fromString = Just . fromString

compileSuccess :: FilePath -> IO (TypeGraph, Map Text (Located Value))
compileSuccess path = do
  result <- ioCompile path
  case result of
    Left err -> expectationFailure (show . PP.vsep . map prettyError $ err) >> fail "error"
    Right result' -> return result'

ioCompile :: FilePath -> IO (Either [Error] (TypeGraph, Map Text (Located Value)))
ioCompile path = runGrLangT emptyState $ do
  withLocalState Set.empty (compileFile $ "tests/GrLang/CompilerSpec" </> path)
  (,) <$> getTypeGraph <*> getValueContext

spec :: Spec
spec = do
  let vcsTypes = TypeGraph.fromNodesAndEdges
        [ Node 1 $ Just $ Metadata "Revision" Nothing, Node 2 $ Just $ Metadata "Deps" Nothing ]
        [ Edge 1 1 2 $ Just $ Metadata "MDeps" Nothing
        , Edge 2 2 1 $ Just $ Metadata "Dep" Nothing ]

  it "compiles a type graph" $ do
    (compiledTGraph, _) <- compileSuccess "vcs-types.grl"
    compiledTGraph `shouldBe` vcsTypes

  it "compiles a graph" $ do
    (tgraph, values) <- compileSuccess "case1.grl"
    Map.keys values `shouldBe` ["g"]
    let VGraph g = Ann.drop (values Map.! "g")
    g `shouldBe` fromNodesAndEdges tgraph
        [ (Node 0 $ Just $ Metadata "r1" Nothing, 1), (Node 1 $ Just $ Metadata "r2" Nothing, 1)
        , (Node 2 $ Just $ Metadata "d1" Nothing, 2), (Node 3 $ Just $ Metadata "d2" Nothing, 2)
        , (Node 4 $ Just $ Metadata "r3" Nothing, 1) ]
        [ (Edge 0 0 2 $ Just $ Metadata Nothing Nothing, 1)
        , (Edge 1 1 3 $ Just $ Metadata Nothing Nothing, 1)
        , (Edge 2 2 1 $ Just $ Metadata "d12" Nothing, 2)
        , (Edge 3 3 4 $ Just $ Metadata Nothing Nothing, 2)
        , (Edge 4 0 0 $ Just $ Metadata "f1" Nothing, 3)
        , (Edge 5 0 0 $ Just $ Metadata "f2" Nothing, 3)
        , (Edge 6 0 0 $ Just $ Metadata "f3" Nothing, 3)
        ]

  it "allows declaration of new types after graphs" $ do
    (tgraph, values) <- compileSuccess "case2.grl"
    Map.keys values `shouldBe` ["g"]
    let VGraph g = Ann.drop $ values Map.! "g"
    typeGraph g `shouldBe` tgraph
    validate g `shouldBe` IsValid

  it "has access to elements of transitive imports" $ do
    (tgraph, values) <- compileSuccess "case3/main.grl"
    Map.keys values `shouldBe` ["g"]
    let VGraph g = Ann.drop $ values Map.! "g"
    g `shouldBe` fromNodesAndEdges tgraph 
      [ (Node 0 $ Just $ Metadata "n1" Nothing, 1)
      , (Node 1 $ Just $ Metadata "n2" Nothing, 1) ]
      [ (Edge 0 0 1 $ Just $ Metadata "e1" Nothing, 1)
      , (Edge 1 0 1 $ Just $ Metadata "e2" Nothing, 1) ]

  it "resolves imported modules relative to the current module's parent" $ do
    (tgraph, values) <- compileSuccess "case4/main.grl"
    Map.keys values `shouldBe` ["g"]
    let VGraph g = Ann.drop $ values Map.! "g"
    g `shouldBe` fromNodesAndEdges tgraph 
      [ (Node 0 $ Just $ Metadata "n1" Nothing, 1)
      , (Node 1 $ Just $ Metadata "n2" Nothing, 1) ]
      [ (Edge 0 0 1 $ Just $ Metadata "e1" Nothing, 1)
      , (Edge 1 0 1 $ Just $ Metadata "e2" Nothing, 1) ]