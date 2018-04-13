module GrLang.TestUtils
  ( makeTypeGraph
  , parseGraph
  , parseMorphism
  , parseRule
  , failWithError
  , quickCheckError
  , runSuccess
  , runFailure
  , fromSuccess
  ) where

import           Control.Monad
import           Control.Monad.Except (ExceptT (..))
import           Control.Monad.Trans
import qualified Data.Char            as Char
import qualified Data.List            as List
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Test.Hspec
import           Test.QuickCheck

import           Base.Annotation      (Annotated (..), Located)
import           Base.Location
import qualified Data.TypedGraph      as TG
import           GrLang.AST
import qualified GrLang.Compiler      as GrLang
import           GrLang.Monad
import           GrLang.Parser        (reservedNames)
import qualified GrLang.Parser        as GrLang
import           GrLang.Value

makeTypeGraph :: [Text] -> [(Text, Text, Text)] -> TypeGraph
makeTypeGraph nodeTypes edgeTypes = fromSuccess . evalGrLang emptyState $ do
  forM_ nodeTypes (addNodeType . A Nothing)
  forM_ edgeTypes (\(e, s, t) -> addEdgeType (A Nothing e) (A Nothing s) (A Nothing t))
  getTypeGraph

parseGraph :: TypeGraph -> String -> GrGraph
parseGraph tgraph string = fromSuccess . evalGrLang (initState tgraph) $ do
  parsed <- GrLang.parseGraph "<test>" string
  lift $ unsafeMakeVisible "<test>"
  GrLang.compileGraph parsed

parseMorphism :: GrGraph -> GrGraph -> String -> GrMorphism
parseMorphism dom cod string = fromSuccess . evalGrLang (initState $ TG.typeGraph dom) $ do
  parsed <- GrLang.parseMorphism "<test>" string
  lift $ unsafeMakeVisible "<test>"
  GrLang.compileMorphism Nothing dom cod parsed

parseRule :: TypeGraph -> String -> GrRule
parseRule tgraph string = fromSuccess . evalGrLang (initState tgraph) $ do
  parsed <- GrLang.parseRule "<test>" string
  lift $ unsafeMakeVisible "<test>"
  GrLang.compileRule parsed

quickCheckError :: Error -> Property
quickCheckError errs = counterexample (show $ prettyError errs) False

failWithError :: Error -> Expectation
failWithError = expectationFailure . show . prettyError

fromSuccess :: Either Error a -> a
fromSuccess (Right x)  = x
fromSuccess (Left err) =  error . show . prettyError $ err

runSuccess :: ExceptT Error (GrLangT IO) a -> IO a
runSuccess action = do
  result <- evalGrLangT emptyState action
  case result of
    Left errs -> failWithError errs >> fail "error"
    Right result' -> return result'

runFailure :: Show a => ExceptT Error (GrLangT IO) a -> IO Error
runFailure action = do
  result <- evalGrLangT emptyState action
  case result of
    Left errs -> return errs
    Right result' -> do
      expectationFailure ("Expected the computation to fail, but it succeeded with:\n\t" ++ show result')
      fail "error"

instance Arbitrary TopLevelDeclaration where
  arbitrary = oneof
    [ Import <$> located genFilePath
    , DeclNodeType <$> located genIdentifier
    , DeclEdgeType <$> located genIdentifier <*> located genIdentifier <*> located genIdentifier
    , DeclGraph <$> located genIdentifier <*> listOf arbitrary
    , DeclMorphism <$> located genIdentifier <*> located genIdentifier <*> located genIdentifier <*> listOf arbitrary
    , DeclRule <$> located genIdentifier <*> listOf arbitrary ]

  shrink Import{}                      = []
  shrink DeclNodeType{}                = []
  shrink DeclEdgeType{}                = []
  shrink (DeclGraph g body)            = DeclGraph g <$> shrink body
  shrink (DeclMorphism f dom cod body) = DeclMorphism f dom cod <$> shrink body
  shrink (DeclRule r body)             = DeclRule r <$> shrink body

instance Arbitrary GraphDeclaration where
  arbitrary = oneof
    [ DeclNodes <$> listOf1 (located genIdentifier) <*> located genIdentifier
    , DeclEdges <$> located genIdentifier <*> listOf1 edges <*> located genIdentifier ]
    where
      edges = (,) <$> arbitrary <*> located genIdentifier

  shrink (DeclNodes ns t)   = [ DeclNodes ns' t | ns' <- shrinkList1 ns ]
  shrink (DeclEdges s es t) = [ DeclEdges s es' t | es' <- shrinkList1 es ]

instance Arbitrary MorphismDeclaration where
  arbitrary = DeclMapping <$> listOf1 (located genIdentifier) <*> located genIdentifier
  shrink (DeclMapping froms to)    = DeclMapping <$> shrinkList1 froms <*> pure to

instance Arbitrary ParallelEdgesDeclaration where
  arbitrary = oneof [ pure AnonymousEdge, NamedEdges <$> listOf1 (located genIdentifier) ]

  shrink AnonymousEdge   = []
  shrink (NamedEdges es) = NamedEdges <$> shrinkList1 es

instance Arbitrary RuleDeclaration where
  arbitrary = oneof
    [ DeclMatch <$> listOf1 arbitrary
    , DeclForbid <$> optional (located genIdentifier) <*> listOf1 arbitrary
    , DeclCreate <$> listOf1 arbitrary
    , DeclDelete <$> listOf1 (located genIdentifier) <*> arbitrary
    , DeclClone <$> located genIdentifier <*> listOf1 (located genIdentifier) ]

  shrink (DeclMatch es)       = DeclMatch <$> shrinkList1 es
  shrink (DeclForbid n es)    = DeclForbid n <$> shrinkList1 es
  shrink (DeclCreate es)      = DeclCreate <$> shrinkList1 es
  shrink (DeclDelete ns mode) = DeclDelete <$> shrinkList1 ns <*> pure mode
  shrink (DeclClone e cs)     = DeclClone e <$> shrinkList1 cs
  shrink (DeclJoin es j)      = DeclJoin <$> shrinkList1 es <*> pure j

instance Arbitrary DeletionMode where
  arbitrary = elements [Isolated, WithMatchedEdges]

shrinkList1 :: [a] -> [[a]]
shrinkList1 []  = []
shrinkList1 [_] = []
shrinkList1 l   = filter (not . null) $ shrinkList (:[]) l

optional :: Gen a -> Gen (Maybe a)
optional gen = oneof [ Just <$> gen, pure Nothing ]

located :: Gen a -> Gen (Located a)
located x = A <$> optional (Location "QuickCheck" <$> arbitrary) <*> x

instance (Arbitrary info, Arbitrary a) => Arbitrary (Annotated info a) where
  arbitrary = A <$> arbitrary <*> arbitrary
  shrink (A i x) = [ A i x' | x' <- shrink x ]

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink = fmap Text.pack . shrink . Text.unpack

genFilePath :: Gen String
genFilePath = listOf (arbitrary `suchThat` isValid)
  where
    isValid c = (Char.isAlphaNum c || c == ' ' || Char.isSymbol c) && Char.isAscii c && c /= '"' && c /= '\\'

genIdentifier :: Gen Text
genIdentifier =
  Text.pack <$> ((:) <$> startChar <*> listOf midChar) `suchThat` (`List.notElem` reservedNames)
  where
    startChar = elements ('?' : startChars)
    startChars = ['a'..'z'] ++ "_"

    midChar = elements midChars
    midChars = startChars ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'"
