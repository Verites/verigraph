{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GrLang.CompilerSpec where

import           Control.Monad
import           Control.Monad.Except      (ExceptT)
import           Control.Monad.Trans
import           Data.Functor.Identity
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.String
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..))
import           System.FilePath
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Abstract.Category
import           Base.Annotation           (Annotated (..), Located)
import qualified Base.Annotation           as Ann
import           Base.Isomorphic
import           Base.Valid
import qualified Data.DList                as DList
import qualified Data.Graphs               as TypeGraph
import           Data.TypedGraph
import qualified Data.TypedGraph.Morphism  as TGraph
import           GrLang.AST
import           GrLang.Compiler
import           GrLang.Monad              hiding (GrLangState (..))
import qualified GrLang.Monad              as GrLang
import           GrLang.TestUtils
import           GrLang.Value
import           Rewriting.DPO.TypedGraph

loc :: a -> Located a
loc = A Nothing

instance IsString a => IsString (Located a) where
  fromString = loc . fromString

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

compileSuccess :: FilePath -> IO (TypeGraph, Map Text (Located Value))
compileSuccess = runSuccess . ioCompile

compileFailure :: FilePath -> IO Error
compileFailure = runFailure . ioCompile

ioCompile :: MonadIO m => FilePath -> ExceptT Error (GrLangT m) (TypeGraph, Map Text (Located Value))
ioCompile path = do
  compileFile $ "tests/GrLang/CompilerSpec" </> path
  (,) <$> getTypeGraph <*> getValueContext

spec :: Spec
spec = do
  let vcsTypes = TypeGraph.fromNodesAndEdges
        [ Node 1 . Just $ Metadata "Revision" Nothing, Node 2 . Just $ Metadata "Deps" Nothing ]
        [ Edge 1 1 2 . Just $ Metadata "MDeps" Nothing
        , Edge 2 2 1 . Just $ Metadata "Dep" Nothing ]

  describe "compileGraph" $ do
    it "always compiles the result of generation" .
      property . monadic runIdentity $ do
        let tgraph = TypeGraph.fromNodesAndEdges
              [ Node 1 . Just $ Metadata "Revision" Nothing, Node 2 . Just $ Metadata "Deps" Nothing ]
              [ Edge 1 1 2 . Just $ Metadata "MDeps" Nothing
              , Edge 2 2 1 . Just $ Metadata "Dep" Nothing
              , Edge 3 1 1 . Just $ Metadata "Foo" Nothing ]
        -- TODO: replace explicit test cases with pseudo-random generation
        let testCases =
              [ fromNodesAndEdges tgraph
                [ (Node 0 . Just $ Metadata "r1" Nothing, 1), (Node 1 . Just $ Metadata "r2" Nothing, 1)
                , (Node 2 . Just $ Metadata "d1" Nothing, 2), (Node 3 . Just $ Metadata "d2" Nothing, 2)
                , (Node 4 . Just $ Metadata "r3" Nothing, 1) ]
                [ (Edge 0 0 2 . Just $ Metadata Nothing Nothing, 1)
                , (Edge 1 1 3 . Just $ Metadata Nothing Nothing, 1)
                , (Edge 2 2 1 . Just $ Metadata "d12" Nothing, 2)
                , (Edge 3 3 4 . Just $ Metadata Nothing Nothing, 2)
                , (Edge 4 0 0 . Just $ Metadata "f1" Nothing, 3)
                , (Edge 5 0 0 . Just $ Metadata "f2" Nothing, 3)
                , (Edge 6 0 0 . Just $ Metadata "f3" Nothing, 3) ]
              , fromNodesAndEdges tgraph
                [ (Node 0 . Just $ Metadata "n1" Nothing, 1)
                , (Node 1 . Just $ Metadata "n2" Nothing, 2) ]
                [ (Edge 0 0 1 . Just $ Metadata "e1" Nothing, 1)
                , (Edge 1 1 0 . Just $ Metadata "e2" Nothing, 2) ]
              ]
        graph <- pick (elements testCases)
        testRoundTrip generateGraph compileGraph validate typeGraph tgraph graph

    it "compiles a type graph" $ do
      (compiledTGraph, _) <- compileSuccess "vcs-types.grl"
      compiledTGraph `shouldBe` vcsTypes

    it "compiles a graph" $ do
      (tgraph, values) <- compileSuccess "case1.grl"
      Map.keys values `shouldBe` ["g"]
      let VGraph g = Ann.drop (values Map.! "g")
      g `shouldBe` fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1), (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2), (Node 3 . Just $ Metadata "d2" Nothing, 2)
          , (Node 4 . Just $ Metadata "r3" Nothing, 1) ]
          [ (Edge 0 0 2 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 2 1 . Just $ Metadata "d12" Nothing, 2)
          , (Edge 3 3 4 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 4 0 0 . Just $ Metadata "f1" Nothing, 3)
          , (Edge 5 0 0 . Just $ Metadata "f2" Nothing, 3)
          , (Edge 6 0 0 . Just $ Metadata "f3" Nothing, 3)
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
        [ (Node 0 . Just $ Metadata "n1" Nothing, 1)
        , (Node 1 . Just $ Metadata "n2" Nothing, 1) ]
        [ (Edge 0 0 1 . Just $ Metadata "e1" Nothing, 1)
        , (Edge 1 0 1 . Just $ Metadata "e2" Nothing, 1) ]

    it "resolves imported modules relative to the current module's parent" $ do
      (tgraph, values) <- compileSuccess "case4/main.grl"
      Map.keys values `shouldBe` ["g"]
      let VGraph g = Ann.drop $ values Map.! "g"
      g `shouldBe` fromNodesAndEdges tgraph
        [ (Node 0 . Just $ Metadata "n1" Nothing, 1)
        , (Node 1 . Just $ Metadata "n2" Nothing, 1) ]
        [ (Edge 0 0 1 . Just $ Metadata "e1" Nothing, 1)
        , (Edge 1 0 1 . Just $ Metadata "e2" Nothing, 1) ]

    it "imports each module only once" $ do
      (tgraph, values) <- compileSuccess "case5/main.grl"
      Map.keys values `shouldBe` ["g"]
      let VGraph g = Ann.drop $ values Map.! "g"
      g `shouldBe` fromNodesAndEdges tgraph
        [ (Node 0 . Just $ Metadata "n1" Nothing, 1)
        , (Node 1 . Just $ Metadata "n2" Nothing, 1) ]
        [ (Edge 0 0 1 . Just $ Metadata "e1" Nothing, 1)
        , (Edge 1 0 1 . Just $ Metadata "e2" Nothing, 1) ]

    it "hides elements that weren't explicitly imported by the current module" $ do
      _ <- compileFailure "case6/main.grl"
      return ()

    it "fails when source or target of edges have invalid types" $ do
      errors <- compileFailure "case7.grl"
      length (DList.toList errors) `shouldBe` 2

  describe "compileRule" $ do

    it "always compiles the result of generation" .
      property . monadic runIdentity $ do
        let tgraph = TypeGraph.fromNodesAndEdges
              [ Node 1 . Just $ Metadata "N" Nothing ]
              [ Edge 1 1 1 . Just $ Metadata "E" Nothing ]
        -- TODO: replace explicit test cases with pseudo-random generation
        let testCases =
              [ [ DeclMatch [DeclNodes ["n1"] "N"]
                , DeclMatch [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
                , DeclMatch [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
                ]
              , [ DeclMatch [DeclNodes ["n1"] "N"]
                , DeclForbid Nothing [DeclNodes ["n3"] "N"]
                , DeclForbid "nodeAndEdge" [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
                ]
              , [ DeclCreate [DeclNodes ["n1"] "N"]
                , DeclCreate [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
                , DeclCreate [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
                ]
              , [ DeclMatch [DeclNodes ["n1"] "N"]
                , DeclMatch [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
                , DeclMatch [DeclNodes ["n2", "n3"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
                , DeclDelete ["n1", "n3"] WithMatchedEdges
                , DeclDelete ["n2"] Isolated
                ]
              , [ DeclMatch [DeclNodes ["n1"] "N"]
                , DeclMatch [DeclEdges "n1" [(AnonymousEdge, "E")] "n1"]
                , DeclMatch [DeclNodes ["n2"] "N", DeclEdges "n1" [(AnonymousEdge, "E")] "n2"]
                , DeclClone "n1" ["n1'"]
                , DeclClone "n2" ["n3", "n4", "n5"]
                ]
              , [ DeclMatch [DeclNodes ["n1"] "N"]
                  , DeclClone "n1" ["n1'"]
                ]
              , [ DeclMatch [DeclNodes ["n1","n2","n3","n4","n5"] "N"]
                , DeclJoin ["n1", "n2"] "n"
                , DeclJoin ["n3", "n4", "n5"] Nothing
                ]
              ]
        decls <- pick (elements testCases)
        let Right rule = evalGrLang (initState tgraph) (compileRule decls)
        testRoundTrip generateRule compileRule validateRule (typeGraph . leftObject) tgraph rule

    it "compiles a no-op rule" $ do
      (tgraph, values) <- compileSuccess "case8.grl"
      Map.keys values `shouldBe` ["r"]
      let VRule r = Ann.drop $ values Map.! "r"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2) ]
          [ (Edge 0 0 2 . Just $ Metadata "r1" Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata "r2" Nothing, 1) ])
        (leftObject r)
      assertBool "lhs morphism is iso" (isIsomorphism $ leftMorphism r)
      assertBool "rhs morphism is iso" (isIsomorphism $ rightMorphism r)
      assertEqual "NACs" [] (nacs r)

    it "compiles a rule with deletion" $ do
      (tgraph, values) <- compileSuccess "case9.grl"
      Map.keys values `shouldBe` ["r"]
      let VRule r = Ann.drop $ values Map.! "r"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2) ]
          [ (Edge 0 0 2 . Just $ Metadata "r1" Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata "r2" Nothing, 1) ])
        (leftObject r)
      assertEqual "RHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2) ]
          [ ])
        (rightObject r)
      assertBool "lhs morphism is mono" (isMonic $ leftMorphism r)
      assertBool "lhs morphism is not iso" (not . isIsomorphism $ leftMorphism r)
      assertBool "rhs morphism is iso" (isIsomorphism $ rightMorphism r)
      assertEqual "NACs" [] (nacs r)

    it "compiles a rule with creation" $ do
      (tgraph, values) <- compileSuccess "case10.grl"
      Map.keys values `shouldBe` ["commit"]
      let VRule r = Ann.drop $ values Map.! "commit"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "d1" Nothing, 2) ]
          [ (Edge 0 0 1 . Just $ Metadata Nothing Nothing, 1) ])
        (leftObject r)
      assertEqual "RHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "d1" Nothing, 2)
          , (Node 2 . Just $ Metadata "r2" Nothing, 1)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2) ]
          [ (Edge 0 0 1 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 2 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 3 0 . Just $ Metadata Nothing Nothing, 2) ])
        (rightObject r)
      assertBool "lhs morphism is iso" (isIsomorphism $ leftMorphism r)
      assertBool "rhs morphism is monic" (isMonic $ rightMorphism r)
      assertBool "rhs morphism is not iso" (not . isIsomorphism $ rightMorphism r)
      assertEqual "NACs" [] (nacs r)

    it "compiles a rule with creation and cloning" $ do
      (tgraph, values) <- compileSuccess "case11.grl"
      Map.keys values `shouldBe` ["commit"]
      let VRule r = Ann.drop $ values Map.! "commit"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "d1" Nothing, 2) ]
          [ (Edge 0 0 1 . Just $ Metadata Nothing Nothing, 1) ])
        (leftObject r)
      assertEqual "RHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d2" Nothing, 2) ]
          [ (Edge 0 0 1 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 3 2 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 2 0 . Just $ Metadata Nothing Nothing, 2) ])
        (rightObject r)
      assertEqual "LHS morphism applied to d2" (Just 1) (TGraph.applyNodeId (leftMorphism r) 2)

      assertBool "lhs morphism is epic" (isEpic $ leftMorphism r)
      assertBool "lhs morphism is not monic" (not . isMonic $ leftMorphism r)
      assertBool "rhs morphism is monic" (isMonic $ rightMorphism r)
      assertBool "rhs morphism is not iso" (not . isIsomorphism $ rightMorphism r)
      assertEqual "NACs" [] (nacs r)

    it "compiles a rule with joining" $ do
      (tgraph, values) <- compileSuccess "case19.grl"
      Map.keys values `shouldBe` ["merge"]
      let VRule r = Ann.drop $ values Map.! "merge"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2) ]
          [ (Edge 0 0 2 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata Nothing Nothing, 1) ])
        (leftObject r)
      assertEqual "interface"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2)
          , (Node 4 . Just $ Metadata "d1'" Nothing, 2)
          , (Node 5 . Just $ Metadata "d2'" Nothing, 2)]
          [ (Edge 0 0 2 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata Nothing Nothing, 1) ])
        (interfaceObject r)
      assertEqual "RHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "d1" Nothing, 2)
          , (Node 3 . Just $ Metadata "d2" Nothing, 2)
          , (Node 6 . Just $ Metadata "d3" Nothing, 2)
          , (Node 7 . Just $ Metadata "r3" Nothing, 1)]
          [ (Edge 0 0 2 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 7 6 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 3 6 0 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 4 6 1 . Just $ Metadata Nothing Nothing, 2) ])
        (rightObject r)
      assertEqual "LHS morphism applied to d1'" (Just 2) (TGraph.applyNodeId (leftMorphism r) 4)
      assertEqual "LHS morphism applied to d2'" (Just 3) (TGraph.applyNodeId (leftMorphism r) 5)
      assertEqual "RHS morphism applied to d1'" (Just 6) (TGraph.applyNodeId (rightMorphism r) 4)
      assertEqual "RHS morphism applied to d2'" (Just 6) (TGraph.applyNodeId (rightMorphism r) 5)

      assertBool "lhs morphism is not monic" (not . isMonic $ leftMorphism r)
      assertBool "lhs morphism is epic" (isEpic $ leftMorphism r)
      assertBool "rhs morphism is not monic" (not . isMonic $ rightMorphism r)
      assertBool "rhs morphism is not epic" (not . isEpic $ rightMorphism r)

    it "compiles a no-op rule with NACs" $ do
      (tgraph, values) <- compileSuccess "case12.grl"
      Map.keys values `shouldBe` ["rebaseStart"]
      let VRule r = Ann.drop $ values Map.! "rebaseStart"
      assertValidRule r
      assertEqual "LHS"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "rRoot" Nothing, 1)
          , (Node 3 . Just $ Metadata "d1" Nothing, 2)
          , (Node 4 . Just $ Metadata "d2" Nothing, 2)
          , (Node 5 . Just $ Metadata "dRoot" Nothing, 2) ]
          [ (Edge 0 0 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 4 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 2 5 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 3 3 2 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 4 4 2 . Just $ Metadata Nothing Nothing, 2) ])
        (leftObject r)
      assertBool "lhs morphism is iso" (isIsomorphism $ leftMorphism r)
      assertBool "rhs morphism is iso" (isIsomorphism $ rightMorphism r)
      assertEqual "number of NACs" 2 (length $ nacs r)
      let [n1, n2] = nacs r
      assertEqual "NAC 1"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "rRoot" Nothing, 1)
          , (Node 3 . Just $ Metadata "d1" Nothing, 2)
          , (Node 4 . Just $ Metadata "d2" Nothing, 2)
          , (Node 5 . Just $ Metadata "dRoot" Nothing, 2)
          , (Node 6 . Just $ Metadata "rRoot'" Nothing, 1)
          , (Node 7 . Just $ Metadata "dRoot'" Nothing, 2) ]
          [ (Edge 0 0 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 4 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 2 5 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 3 3 2 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 4 4 2 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 5 6 7 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 6 3 6 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 7 4 6 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 8 7 2 . Just $ Metadata Nothing Nothing, 2) ])
        (codomain n1)
      assertEqual "NAC 2"
        (fromNodesAndEdges tgraph
          [ (Node 0 . Just $ Metadata "r1" Nothing, 1)
          , (Node 1 . Just $ Metadata "r2" Nothing, 1)
          , (Node 2 . Just $ Metadata "rRoot" Nothing, 1)
          , (Node 3 . Just $ Metadata "d1" Nothing, 2)
          , (Node 4 . Just $ Metadata "d2" Nothing, 2)
          , (Node 5 . Just $ Metadata "dRoot" Nothing, 2)
          , (Node 6 . Just $ Metadata "d3" Nothing, 2) ]
          [ (Edge 0 0 3 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 1 1 4 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 2 2 5 . Just $ Metadata Nothing Nothing, 1)
          , (Edge 3 3 2 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 4 4 2 . Just $ Metadata Nothing Nothing, 2)
          , (Edge 5 6 0 . Just $ Metadata Nothing Nothing, 2) ])
        (codomain n2)

    it "fails when deleting unknown elements" $ do
      errors <- compileFailure "case13.grl"
      length (DList.toList errors) `shouldBe` 1

    it "fails when cloning unknown elements" $ do
      errors <- compileFailure "case14.grl"
      length (DList.toList errors) `shouldBe` 1

    it "fails when joining unknown elements" $ do
      errors <- compileFailure "case23.grl"
      length (DList.toList errors) `shouldBe` 1

    it "fails when create has a name clash" $ do
      errors <- compileFailure "case15.grl"
      length (DList.toList errors) `shouldBe` 3

    it "fails when clone has a name clash" $ do
      errors <- compileFailure "case16.grl"
      length (DList.toList errors) `shouldBe` 3

    it "fails when join has a name clash" $ do
      errors <- compileFailure "case24.grl"
      length (DList.toList errors) `shouldBe` 1

    it "fails when deleting leaves dangling edges" $ do
      errors <- compileFailure "case17.grl"
      length (DList.toList errors) `shouldBe` 2

    it "fails when source or target of created edge have invalid type" $ do
      errors <- compileFailure "case18.grl"
      length (DList.toList errors) `shouldBe` 2

    it "fails when joining nodes of different types" $ do
      errors <- compileFailure "case20.grl"
      length (DList.toList errors) `shouldBe` 1

    it "fails when joining edges of different types, sources or targets" $ do
      errors <- compileFailure "case21.grl"
      length (DList.toList errors) `shouldBe` 3

    it "fails when joining a node with an edge" $ do
      errors <- compileFailure "case22.grl"
      length (DList.toList errors) `shouldBe` 1

testRoundTrip :: (Iso val, Valid val, Show val, Pretty ast, Show ast, Monad m) =>
                  (val -> ast)
                  -> (ast -> ExceptT Error (GrLangT Identity) val)
                  -> (val -> ValidationResult)
                  -> (val -> TypeGraph)
                  -> TypeGraph
                  -> val
                  -> PropertyM m b
testRoundTrip generate compile validateValue getTypeGraph tgraph value = do
  let generated = generate value
  --monitor (counterexample . show . pretty $ generated)
  monitor (counterexample . show $ value)
  case validateValue value of
    IsInvalid errors -> stop . counterexample "Invalid value!" $ counterexample (show errors) False
    IsValid -> return ()
  let recompiled = evalGrLang (initState tgraph) (compile generated)
  case recompiled of
    Left error -> stop .
      counterexample "Cannot compile generated value!" $
      counterexample (show . prettyError $ error) False
    Right result -> do
      when (getTypeGraph result /= tgraph) . stop .
        counterexample "Invalid parsed type graph!" $
        counterexample (show (getTypeGraph result, tgraph)) False
      stop $ case validateValue result of
        IsInvalid errors ->
          counterexample "Compilation created invalid value!" $
          counterexample (show errors) False
        IsValid -> --counterexample (show . pretty $ generate result) .
          counterexample (show value) .
          counterexample "Result not isomorphic to value!" $ result ~= value

-- TODO: make a proper SqPO.Production type with its own validation
validateRule :: (Eq (Obj morph), Category morph, Valid morph) => Production morph -> ValidationResult
validateRule (Production l r nacs) = mconcat $
  [ withContext "left morphism" (validate l)
  , withContext "right morphism" (validate r)
  , ensure (domain l == domain r) "The domains of the left and right morphisms aren't the same"
  ] ++ zipWith validateNac nacs ([1..] :: [Int])
  where
    validateNac nac index =
      mconcat
        [ withContext ("NAC #" ++ show index) (validate nac)
        , ensure (codomain l == domain nac) ("The domain of NAC #" ++ show index ++ " is not the left side of the production")
        ]


assertValidRule :: (Category mor, Valid mor, Valid (Obj mor), Eq (Obj mor)) => Production mor -> IO ()
assertValidRule rule =
  assertEqual "rule validation" IsValid (validateRule rule)
